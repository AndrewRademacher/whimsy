{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Impl.IORef2 where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.IORef
import           Data.Proxy
import           Data.Time
import           Network.Wai.Handler.Warp   (run)
import           Servant.API
import           Servant.Server

import           API

data Environment
    = Environment
        { serverFortuneMap :: IORef (HashMap Email Fortune)
        }

runApp :: IO ()
runApp = do
    env <- createEnvironment
    run 8080 $ serve (Proxy :: Proxy API)
        (    createPlayer env
        :<|> tradeFortunes env)

createEnvironment :: IO Environment
createEnvironment = do
    fm <- newIORef Map.empty
    return $ Environment fm

createPlayer :: Environment -> NewPlayer -> EitherT ServantErr IO ()
createPlayer env player = do
    now <- liftIO getCurrentTime
    let f = Fortune (npFortune player) now
    m <- liftIO $ readIORef (serverFortuneMap env)
    let m' = Map.insert (npEmail player) f m
    liftIO $ writeIORef (serverFortuneMap env) m'

tradeFortunes :: Environment -> Email -> Email -> EitherT ServantErr IO FortunePair
tradeFortunes env from to = do
    m <- liftIO $ readIORef (serverFortuneMap env)
    let mgiven    = Map.lookup from m
        mreceived = Map.lookup to m
    case (mgiven, mreceived) of
        (Just given, Just received) -> do
            let m'  = Map.insert from received m
                m'' = Map.insert to given m'
            liftIO $ writeIORef (serverFortuneMap env) m''
            return $ FortunePair given received
        _ -> left err404
