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
import Data.IxSet
import Data.Data

-- Simple Types (for querying with IxSet)
data Email
  = Email Text
  deriving (Data, Eq, Ord, Show, Typeable)
$(deriveSafeCopy 0 'base ''Email)

-- End Query Types

data User = User
  { email    :: Email
  , password :: Maybe Text
  , verkey   :: Maybe Text
  , verified :: Bool
  }
  deriving (Data, Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''User)

instance Indexable User where
   empty = ixSet 
             [ ixFun (\u -> [email u])
             ]

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
  , users :: IxSet User
  }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Database)

