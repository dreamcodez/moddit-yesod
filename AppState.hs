{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module AppState
  ( openFrom
  , NewsItem(..)
  , AddNews(..)
  , ReadNews(..)
  , IncrementHits(..)
  , ReadHits(..)
  , Database
  )
where

import Data.Acid
import Data.SafeCopy
import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Applicative  ( (<$>) )
import Prelude
import Data.Typeable
import Data.Text (Text)

data NewsItem = NewsItem
  { title :: Text
  , url   :: Text
  }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''NewsItem)

--instance SafeCopy NewsItem where
--  putCopy = contain . safePut
-- getCopy = contain $ safeGet

data Database = Database
  { hits :: Int
  , news :: [NewsItem]
  }

$(deriveSafeCopy 0 'base ''Database)

addNews :: NewsItem -> Update Database ()
addNews n =
  do db <- get
     put db{news = n:(news db)}

readNews :: Int -> Query Database [NewsItem]
readNews limit = take limit <$> news <$> ask

incrementHits :: Update Database ()
incrementHits =
  do db <- get
     put db{hits = (hits db) + 1}

readHits :: Query Database Int
readHits = hits <$> ask

$(makeAcidic ''Database ['addNews, 'readNews, 'incrementHits, 'readHits])

openFrom :: String -> IO (AcidState Database)
openFrom path =
  openLocalStateFrom path (Database 0 [])

