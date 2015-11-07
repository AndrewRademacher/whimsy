{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.IORef
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Proxy
import           Data.Text                  (Text)
import           Data.Time
import           GHC.Generics
import           Network.Wai.Handler.Warp   (run)
import           Servant.API
import           Servant.Server

type Email = Text

type API
    =    "player" :> ReqBody '[JSON] NewPlayer :> Post '[JSON] ()
    :<|> "fortune" :> "trade" :> "player" :> Capture "email" Text :> Get '[JSON] FortunePair

data NewPlayer
    = NewPlayer
        { npEmail   :: Email
        , npFortune :: Text
        }
    deriving (Eq, Ord, Show, Generic)

instance FromJSON NewPlayer where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data Fortune
    = Fortune
        { fFortune   :: Text
        , fCreatedAt :: UTCTime
        }
    deriving (Eq, Ord, Show, Generic)

instance ToJSON Fortune where
    toJSON = genericToJSON $ aesonPrefix snakeCase

data FortunePair
    = FortunePair
        { fpFortuneGiven    :: Fortune
        , fpFortuneReceived :: Fortune
        }
    deriving (Eq, Ord, Show, Generic)

instance ToJSON FortunePair where
    toJSON = genericToJSON $ aesonPrefix snakeCase

data Environment
    = Environment
        { serverFortuneMap :: IORef (Map Email Fortune)
        }

main :: IO ()
main = do
    fm <- newIORef Map.empty
    let env = Environment fm
    run 8080 $ serve (Proxy :: Proxy API)
        (    createPlayer env
        :<|> tradeFortunes env)

createPlayer :: Environment -> NewPlayer -> EitherT ServantErr IO ()
createPlayer = undefined

tradeFortunes :: Environment -> Email -> EitherT ServantErr IO FortunePair
tradeFortunes = undefined
