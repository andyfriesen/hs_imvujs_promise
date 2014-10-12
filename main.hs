{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.IORef
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit
import Promise

mkPromise :: IO (Promise result err, Resolver result err)
mkPromise = do
    r <- newIORef undefined
    promise <- newPromise $ writeIORef r
    resolver <- readIORef r
    return (promise, resolver)

mkCallback :: (result -> wrappedResult) -> IO (IORef [result], result -> IO wrappedResult)
mkCallback f = do
    list <- newIORef []
    let callback r = do
            modifyIORef list (r:)
            return $ f r

    return (list, callback)

mkAcceptor :: IO (IORef [result], result -> IO (PromiseResult result err))
mkAcceptor = mkCallback Success

mkRejector :: IO (IORef [err], err -> IO (PromiseResult result err))
mkRejector = mkCallback Failure

assertIORefEqual name expected actualRef = do
    actual <- readIORef actualRef
    assertEqual name expected actual

main = defaultMain $ testGroup "Tests"
    [ testCase "accept after then" $ do
        (promise, resolver) <- mkPromise
        (accepts, acceptCallback) <- mkAcceptor

        then2 promise acceptCallback
        accept resolver "Hello"

        assertIORefEqual "Accepted" ["Hello"] accepts
    , testCase "then after accept" $ do
        (promise, resolver) <- mkPromise
        (accepts, acceptCallback) <- mkAcceptor

        accept resolver "Hello"
        then2 promise acceptCallback

        assertIORefEqual "Accepted" ["Hello"] accepts
    , testCase "reject after then" $ do
        (promise, resolver) <- mkPromise
        (rejects, rejectCallback) <- mkRejector

        catch promise rejectCallback
        reject resolver "Bye"

        assertIORefEqual "Rejected" ["Bye"] rejects

    , testCase "second accept is ignored" $ do
        (promise, resolver) <- mkPromise
        (accepts,  acceptCallback) <- mkAcceptor
        then2 promise acceptCallback
        accept resolver "Hello"
        accept resolver "Two"

        assertIORefEqual "Only one" ["Hello"] accepts

    , testCase "resolve given a future" $ do
        (promise, resolver) <- mkPromise
        (p2, r2) <- mkPromise
        (accepts, acceptCallback) <- mkAcceptor

        then2 promise acceptCallback
        resolve resolver p2

        assertIORefEqual "No calls yet" [] accepts

        accept r2 "Hello"

        assertIORefEqual "Resolved" ["Hello"] accepts

    , testCase "then2" $ do
        (promise, resolver) <- mkPromise
        (accepts, acceptCallback) <- mkAcceptor

        then2 promise acceptCallback
        accept resolver "Hello"

        assertIORefEqual "Accepted" ["Hello"] accepts

    , testCase "accept yields accepted Promise" $ do
        promise <- acceptedPromise "Hello"
        (accepts, acceptCallback) <- mkAcceptor
        then2 promise acceptCallback
        assertIORefEqual "Accepted" ["Hello"] accepts

    , testCase "then chained to catch" $ do
        p <- acceptedPromise "value"
        result <- newIORef Nothing
        -- p2 <- p `then2` (\value -> return $ Failure value)
        -- p3 <- p2 `catch` (\error -> writeIORef result (Just error) >> return (Failure ()))
        p `then2` (\value -> return $ Failure value) >>=
         (`catch` (\error -> writeIORef result (Just error) >> return (Failure ())))

        r <- readIORef result
        assertIORefEqual "error was called" (Just "value") result

    , testCase "does not trip reentrancy" $ do
        (promise, resolver) <- mkPromise
        c <- newIORef (0 :: Integer)
        reentrant <- newIORef False

        let fxn () = do
                modifyIORef c (+1)
                then2 promise $ \() -> do
                    c' <- readIORef c
                    when (c' > 1) $ writeIORef reentrant True
                    return $ Success ()
                writeIORef c 0
                return $ Success ()

        then2 promise fxn
        then2 promise fxn
        accept resolver ()

        assertIORefEqual "Did not reenter" False reentrant
    ]
