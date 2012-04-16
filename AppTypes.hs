{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module AppTypes where

import Data.Acid
import Data.SafeCopy
import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Applicative  ( (<$>) )
import Prelude
import Data.Typeable
import Data.Text (Text)
import Data.Map
import Data.Time.Clock (UTCTime)

data User = User
  { email    :: Text
  , pass     :: Maybe Text
  , verkey   :: Maybe Text
  , verified :: Bool
  }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''User)

data NewsItem = NewsItem
  { title   :: Text
  , url     :: Text
  , created :: UTCTime
  , user    :: User 
  }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''NewsItem)

data Database = Database
  { hits  :: Int
  , news  :: [NewsItem]
  , users :: Map Text User
  }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Database)

