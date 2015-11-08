{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Impl.IORef1
    ( runApp
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.IORef
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Proxy
import           Data.Time
import           Network.Wai.Handler.Warp   (run)
import           Servant.API
import           Servant.Server

import           API

data Environment
    = Environment
        { serverFortuneMap :: IORef (Map Email Fortune)
        }

runApp :: IO ()
runApp = do
    fm <- newIORef Map.empty
    let env = Environment fm
    run 8080 $ serve (Proxy :: Proxy API)
        (    createPlayer env
        :<|> tradeFortunes env)

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
            let m'  = Map.update (\_ -> Just received) from m
                m'' = Map.update (\_ -> Just given) to m'
            liftIO $ writeIORef (serverFortuneMap env) m''
            return $ FortunePair given received
        _ -> left err404
