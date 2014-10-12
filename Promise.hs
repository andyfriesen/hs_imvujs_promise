{-# LANGUAGE RecordWildCards #-}

module Promise where

import Control.Monad
import Data.IORef
import Data.Maybe

data Status
    = Pending
    | Accepted
    | Rejected

data Promise result err = Promise
    { pValue           :: IORef (Maybe (Either err result))
    , pAcceptCallbacks :: IORef [result -> IO ()]
    , pRejectCallbacks :: IORef [err -> IO ()]
    }

data Resolver result err = Resolver (Promise result err)

data PromiseResult result err
    = Success result
    | Failure err
    | Chain (Promise result err)

getStatus :: Promise result err -> IO Status
getStatus Promise{..} = do
    value <- readIORef pValue
    return $ case value of
        Nothing -> Pending
        (Just (Left _)) -> Rejected
        (Just (Right _)) -> Accepted

scheduleCallbacks :: Promise result err -> IO ()
scheduleCallbacks promise = do
    let Promise{..} = promise
    value <- readIORef pValue
    case value of
        Just (Left err)
            -> processCallbacks pRejectCallbacks err
        Just (Right result)
            -> processCallbacks pAcceptCallbacks result
        _
            -> return ()

processCallbacks :: IORef [a -> IO b] -> a -> IO ()
processCallbacks cbRef arg = do
    cbs <- readIORef cbRef
    forM_ cbs ($ arg)
    writeIORef cbRef []

accept :: Resolver result err -> result -> IO ()
accept resolver result = do
    let Resolver promise = resolver
    let Promise{..} = promise
    v <- readIORef pValue
    when (not $ isJust v) $ do
        writeIORef pValue (Just $ Right result)
        writeIORef pRejectCallbacks []
        scheduleCallbacks promise

reject :: Resolver result err -> err -> IO ()
reject resolver err = do
    let Resolver promise = resolver
    let Promise{..} = promise
    v <- readIORef pValue
    when (not $ isJust v) $ do
        writeIORef pValue (Just $ Left err)
        writeIORef pAcceptCallbacks []
        scheduleCallbacks promise

resolve :: Resolver result err -> Promise result err -> IO ()
resolve resolver promise = do
    let adaptAccept r = do
            accept resolver r
            return $ Success r
    let adaptReject r = do
            reject resolver r
            return $ Failure r

    void $ then_ promise adaptAccept adaptReject

newPromise :: (Resolver result err -> IO ()) -> IO (Promise result err)
newPromise init = do
    pValue           <- newIORef Nothing
    pAcceptCallbacks <- newIORef []
    pRejectCallbacks <- newIORef []
    let promise = Promise{..}
    init (Resolver promise)
    return promise

acceptedPromise :: result -> IO (Promise result err)
acceptedPromise result =
    newPromise (\resolver -> accept resolver result)

rejectedPromise :: err -> IO (Promise result err)
rejectedPromise err =
    newPromise (\resolver -> reject resolver err)

wrap :: Resolver result err
     -> (t -> IO (PromiseResult result err))
     -> t
     -> IO ()
wrap resolver cb arg = do
    value <- cb arg
    case value of
        Success s -> accept resolver s
        Failure e -> reject resolver e
        Chain pr -> resolve resolver pr

then_ :: Promise result1 error1
      -> (result1 -> IO (PromiseResult result2 error2))
      -> (error1 -> IO (PromiseResult result2 error2))
      -> IO (Promise result2 error2)
then_ promise acceptCb rejectCb = do
    let Promise{..} = promise
    newPromise $ \resolver -> do
        let wrappedAccept = wrap resolver acceptCb
            wrappedReject = wrap resolver rejectCb

        modifyIORef pAcceptCallbacks (wrappedAccept:)
        modifyIORef pRejectCallbacks (wrappedReject:)

        value <- readIORef pValue
        when (isJust value) $ scheduleCallbacks promise

then2 :: Promise result err
      -> (result -> IO (PromiseResult result2 err))
      -> IO (Promise result2 err)
then2 promise acceptCb = do
    let Promise{..} = promise
    newPromise $ \resolver -> do
        let wrappedAccept = wrap resolver acceptCb
            wrappedReject = reject resolver

        modifyIORef pAcceptCallbacks (wrappedAccept:)
        modifyIORef pRejectCallbacks (wrappedReject:)

        value <- readIORef pValue
        when (isJust value) $ scheduleCallbacks promise

catch :: Promise result err
      -> (err -> IO (PromiseResult result err2))
      -> IO (Promise result err2)
catch promise rejectCb = do
    let Promise{..} = promise
    newPromise $ \resolver -> do
        let wrappedAccept = accept resolver
            wrappedReject = wrap resolver rejectCb

        modifyIORef pAcceptCallbacks (wrappedAccept:)
        modifyIORef pRejectCallbacks (wrappedReject:)

        value <- readIORef pValue
        when (isJust value) $ scheduleCallbacks promise
