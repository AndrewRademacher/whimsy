{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module API where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time
import           GHC.Generics
import           Servant.API

type Email = Text

type API
    =    "player" :> ReqBody '[JSON] NewPlayer :> Post '[JSON] ()
    :<|> "fortune" :> "trade"
        :> Capture "email" Text :> Capture "email" Text :> Get '[JSON] FortunePair

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
