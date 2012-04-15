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

data NewsItem = NewsItem
  { title :: Text
  , url   :: Text
  }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''NewsItem)

data User = User
  { email    :: Text
  , pass     :: Maybe Text
  , verkey   :: Maybe Text
  , verified :: Bool
  }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''User)

data Database = Database
  { hits  :: Int
  , news  :: [NewsItem]
  , users :: Map Text User
  }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Database)

