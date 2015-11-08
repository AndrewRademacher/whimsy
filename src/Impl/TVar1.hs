{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Impl.TVar1 where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Proxy
import           Data.Time
import           Network.Wai.Handler.Warp   (run)
import           Servant.API
import           Servant.Server

import           API

data Environment
    = Environment
        { serverFortuneMap :: TVar (HashMap Email (TVar Fortune))
        }

runApp :: IO ()
runApp = do
    env <- createEnvironment
    run 8080 $ serve (Proxy :: Proxy API)
        (    createPlayer env
        :<|> tradeFortunes env)

createEnvironment :: IO Environment
createEnvironment = do
    fm <- newTVarIO Map.empty
    return $ Environment fm

createPlayer :: Environment -> NewPlayer -> EitherT ServantErr IO ()
createPlayer env player = do
    now <- liftIO getCurrentTime
    liftIO $ atomically $ do
        f <- newTVar (Fortune (npFortune player) now)
        m <- readTVar (serverFortuneMap env)
        let m' = Map.insert (npEmail player) f m
        writeTVar (serverFortuneMap env) m'

tradeFortunes :: Environment -> Email -> Email -> EitherT ServantErr IO FortunePair
tradeFortunes env from to = do
    res <- liftIO $ atomically $ do
        m <- readTVar (serverFortuneMap env)
        let mgivenv    = Map.lookup from m
            mreceivedv = Map.lookup to m
        case (mgivenv, mreceivedv) of
            (Just givenv, Just receivedv) -> do
                given    <- readTVar givenv
                received <- readTVar receivedv
                writeTVar givenv received
                writeTVar receivedv given
                return $ Just $ FortunePair given received
            _ -> return Nothing
    case res of
        Just v  -> return v
        Nothing -> left err404
