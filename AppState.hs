{-# LANGUAGE TypeFamilies, DeriveDataTypeable, NamedFieldPuns, TemplateHaskell #-}
module AppState
  ( openFrom
  , NewsItem(..)
  , AddNews(..)
  , ReadNews(..)
  , IncrementHits(..)
  , ReadHits(..)
  , LookupUser(..)
  , Database
  , module Data.Acid
  )
where

import Data.Acid
import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Applicative  ( (<$>) )
import Prelude
import Data.Typeable
import Data.Text (Text)
import Data.IxSet

import AppTypes

defaultFixtures =
  Database
    0
    []
    empty

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

lookupUser :: Text -> Query Database (Maybe User)
lookupUser e = do
  Database{users} <- ask
  return . getOne $
    users @= Email e

$(makeAcidic ''Database ['addNews, 'readNews, 'incrementHits, 'readHits, 'lookupUser])

openFrom :: String -> IO (AcidState Database)
openFrom path =
  openLocalStateFrom path defaultFixtures 

