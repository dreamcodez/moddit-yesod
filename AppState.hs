{-# LANGUAGE TypeFamilies, DeriveDataTypeable, TemplateHaskell #-}
module AppState
  ( openFrom
  , CreateMessage(..)
  , ReadMessages(..)
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

data Database = Database
  { hits     :: Int
  , messages :: [String]
  }

$(deriveSafeCopy 0 'base ''Database)

createMessage :: String -> Update Database ()
createMessage msg =
  do db <- get
     put db{messages = msg:(messages db)}

readMessages :: Int -> Query Database [String]
readMessages limit = take limit <$> messages <$> ask

incrementHits :: Update Database ()
incrementHits =
  do db <- get
     put db{hits = (hits db) + 1}

readHits :: Query Database Int
readHits = hits <$> ask

$(makeAcidic ''Database ['createMessage, 'readMessages, 'incrementHits, 'readHits])

openFrom :: String -> IO (AcidState Database)
openFrom path =
  openLocalStateFrom path (Database 0 [])

