{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Impl.MVar2
    ( runApp
    ) where

import           Control.Concurrent.MVar
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
        { serverFortuneMap :: MVar (HashMap Email (MVar Fortune))
        }

runApp :: IO ()
runApp = do
    fm <- newMVar Map.empty
    let env = Environment fm
    run 8080 $ serve (Proxy :: Proxy API)
        (    createPlayer env
        :<|> tradeFortunes env)

createPlayer :: Environment -> NewPlayer -> EitherT ServantErr IO ()
createPlayer env player = do
    now <- liftIO getCurrentTime
    f <- liftIO $ newMVar (Fortune (npFortune player) now)
    m <- liftIO $ takeMVar (serverFortuneMap env)
    let m' = Map.insert (npEmail player) f m
    liftIO $ putMVar (serverFortuneMap env) m'

tradeFortunes :: Environment -> Email -> Email -> EitherT ServantErr IO FortunePair
tradeFortunes env from to = do
    m <- liftIO $ readMVar (serverFortuneMap env)
    let mgivenv    = Map.lookup from m
        mreceivedv = Map.lookup to m
    case (mgivenv, mreceivedv) of
        (Just givenv, Just receivedv) -> do
            given    <- liftIO $ takeMVar givenv
            received <- liftIO $ takeMVar receivedv
            liftIO $ putMVar givenv received
            liftIO $ putMVar receivedv given
            return $ FortunePair given received
        _ -> left err404
