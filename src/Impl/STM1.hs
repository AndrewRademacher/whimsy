{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Impl.STM1 where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           Data.Proxy
import           Data.Time
import           Network.Wai.Handler.Warp   (run)
import           Servant.API
import           Servant.Server
import           STMContainers.Map          (Map)
import qualified STMContainers.Map          as Map

import           API

data Environment
    = Environment
        { serverFortuneMap :: Map Email Fortune
        }

runApp :: IO ()
runApp = do
    env <- createEnvironment
    run 8080 $ serve (Proxy :: Proxy API)
        (    createPlayer env
        :<|> tradeFortunes env)

createEnvironment :: IO Environment
createEnvironment = do
    fm <- Map.newIO
    return $ Environment fm

createPlayer :: Environment -> NewPlayer -> EitherT ServantErr IO ()
createPlayer env player = do
    now <- liftIO getCurrentTime
    liftIO $ atomically $ do
        let f = Fortune (npFortune player) now
        Map.insert f (npEmail player) (serverFortuneMap env)

tradeFortunes :: Environment -> Email -> Email -> EitherT ServantErr IO FortunePair
tradeFortunes env from to = do
    res <- liftIO $ atomically $ do
        mgiven    <- Map.lookup from (serverFortuneMap env)
        mreceived <- Map.lookup to (serverFortuneMap env)
        case (mgiven, mreceived) of
            (Just given, Just received) -> do
                Map.insert given to (serverFortuneMap env)
                Map.insert received from (serverFortuneMap env)
                return $ Just $ FortunePair given received
            _ -> return Nothing
    case res of
        Just  v -> return v
        Nothing -> left err404
