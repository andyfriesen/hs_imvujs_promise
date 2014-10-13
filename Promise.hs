{-# LANGUAGE RecordWildCards #-}

module Promise where

import Control.Monad
import Data.IORef
import Data.Maybe
import Data.Default
import Data.List (sortBy)
import qualified Data.HashMap.Strict as HM

data Status
    = Pending
    | Accepted
    | Rejected

data Promise result err = Promise
    { pLabel           :: String
    , pValue           :: IORef (Maybe (Either err result))
    , pProcessing      :: IORef Bool
    , pAcceptCallbacks :: IORef [result -> IO ()]
    , pRejectCallbacks :: IORef [err -> IO ()]
    }

data Resolver result err = Resolver (Promise result err)

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
    print ("scheduleCallbacks", pLabel)
    value <- readIORef pValue
    case value of
        Just (Left err)
            -> processCallbacks pProcessing pRejectCallbacks err
        Just (Right result)
            -> processCallbacks pProcessing pAcceptCallbacks result
        _
            -> return ()

processCallbacks :: IORef Bool -> IORef [a -> IO b] -> a -> IO ()
processCallbacks processingRef cbRef arg = do
    processing <- readIORef processingRef
    when (not processing) $ do
        writeIORef processingRef True
        cbs <- readIORef cbRef
        print ("processCallbacks", length cbs)
        writeIORef cbRef []
        forM_ cbs ($ arg)
        writeIORef processingRef False

accept :: Resolver result err -> result -> IO ()
accept resolver result = do
    let Resolver promise = resolver
    let Promise{..} = promise
    v <- readIORef pValue
    when (not $ isJust v) $ do
        writeIORef pValue (Just $ Right result)
        writeIORef pRejectCallbacks []
        print "accept"
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
            print ("adaptAccept")
            accept resolver r
            acceptedPromise ()
    let adaptReject r = do
            reject resolver r
            rejectedPromise ()

    void $ then_ promise adaptAccept adaptReject

-- newPromise :: (Resolver result err -> IO ()) -> IO (Promise result err)
newPromise pLabel init = do
    pValue           <- newIORef Nothing
    pProcessing      <- newIORef False
    pAcceptCallbacks <- newIORef []
    pRejectCallbacks <- newIORef []
    let promise = Promise{..}
    init (Resolver promise)
    return promise

acceptedPromise :: result -> IO (Promise result err)
acceptedPromise result =
    newPromise "accepted" $ \resolver -> do
        print "acceptedPromise"
        accept resolver result

rejectedPromise :: err -> IO (Promise result err)
rejectedPromise err =
    newPromise "rejected" (\resolver -> reject resolver err)

wrap :: Resolver result err
     -> (t -> IO (Promise result err))
     -> t
     -> IO ()
wrap resolver cb arg = do
    value <- cb arg
    resolve resolver value

then_ :: Promise result1 error1
      -> (result1 -> IO (Promise result2 error2))
      -> (error1 -> IO (Promise result2 error2))
      -> IO (Promise result2 error2)
then_ promise acceptCb rejectCb = do
    let Promise{..} = promise
    newPromise (pLabel ++ "2") $ \resolver -> do
        let wrappedAccept = wrap resolver acceptCb
            wrappedReject = wrap resolver rejectCb

        modifyIORef pAcceptCallbacks (wrappedAccept:)
        modifyIORef pRejectCallbacks (wrappedReject:)

        value <- readIORef pValue
        print "then_"
        when (isJust value) $ scheduleCallbacks promise

then2 :: Promise result err
      -> (result -> IO (Promise result2 err))
      -> IO (Promise result2 err)
then2 promise acceptCb = do
    let Promise{..} = promise
    newPromise (pLabel ++ "then2") $ \resolver -> do
        let wrappedAccept = wrap resolver acceptCb
            wrappedReject = reject resolver

        modifyIORef pAcceptCallbacks (wrappedAccept:)
        modifyIORef pRejectCallbacks (wrappedReject:)

        value <- readIORef pValue
        print "then2"
        when (isJust value) $ scheduleCallbacks promise

-- catch :: Promise result err
--       -> (err -> IO (Promise result err2))
--       -> IO (Promise result err2)
catch promise rejectCb = do
    let Promise{..} = promise
    newPromise "catch" $ \resolver -> do
        let wrappedAccept = accept resolver
            wrappedReject = wrap resolver rejectCb

        modifyIORef pAcceptCallbacks (wrappedAccept:)
        modifyIORef pRejectCallbacks (wrappedReject:)

        value <- readIORef pValue
        when (isJust value) $ scheduleCallbacks promise

any :: Default result => [Promise result err] -> IO (Promise result err)
any [] = acceptedPromise def
any promises = newPromise "any" $ \resolver -> do
    let acceptCb result = do
            accept resolver result
            acceptedPromise result
        rejectCb result = do
            reject resolver result
            rejectedPromise result
    forM_ promises $ \promise -> then_ promise acceptCb rejectCb

enumerate :: [a] -> [(Int, a)]
enumerate someList = zip [0..] someList

every :: [Promise a err] -> IO (Promise [a] err)
every [] = acceptedPromise []
every promises = newPromise "any" $ \resolver -> do
    countdown <- newIORef (length promises)
    args <- newIORef HM.empty

    let rejectCb result = do
            reject resolver result
            rejectedPromise result

    forM_ (enumerate promises) $ \(index, promise) -> do
        let acceptCb result = do
                modifyIORef args (HM.insert index result)
                modifyIORef countdown (\i -> i - 1)
                cd <- readIORef countdown
                if 0 == cd then do
                    a <- readIORef args
                    let ordered = snd $ unzip $ sortBy (\a b -> compare (fst a) (fst b)) $ HM.toList a
                    accept resolver ordered
                    acceptedPromise ordered
                else
                    acceptedPromise def -- ?????

        then_ promise acceptCb rejectCb

-- some :: [Promise [a] a] -> IO (Promise [a] err)
some :: Default result => [Promise result error1] -> IO (Promise result [error1])
some [] = acceptedPromise def
some promises = newPromise "some" $ \resolver -> do
    countdown <- newIORef (length promises)
    args <- newIORef HM.empty

    let acceptCb result = do
            accept resolver result
            acceptedPromise result

    forM_ (enumerate promises) $ \(index, promise) -> do
        let rejectCb result = do
                modifyIORef args (HM.insert index result)
                modifyIORef countdown (\i -> i - 1)
                cd <- readIORef countdown
                if 0 == cd then do
                    a <- readIORef args
                    let ordered = snd $ unzip $ sortBy (\a b -> compare (fst a) (fst b)) $ HM.toList a
                    reject resolver ordered
                    rejectedPromise ordered
                else
                    rejectedPromise def -- ?????

        then_ promise acceptCb rejectCb
