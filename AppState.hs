{-# LANGUAGE TypeFamilies, DeriveDataTypeable, NamedFieldPuns, TemplateHaskell #-}
module AppState
  ( openFrom
  , NewsItem(..)
  , AddNews(..)
  , ReadNews(..)
  , IncrementHits(..)
  , ReadHits(..)
  , LookupUser(..)
  , LookupUserByEmail(..)
  , UpdateUser(..)
  , AddUser(..)
  , UpdateUserVerkey(..)
  , UpdateUserPassword(..)
  , Database
  , module Data.Acid
  , module Data.Acid.Advanced
  )
where

import Data.Acid
import Data.Acid.Advanced
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
    (UserId 1)

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

lookupUser :: UserId -> Query Database (Maybe User)
lookupUser uid =
  do Database{users} <- ask
     return . getOne $
       users @= uid

lookupUserByEmail :: Email -> Query Database (Maybe User)
lookupUserByEmail email =
  do Database{users} <- ask
     return . getOne $
       users @= email

updateUser :: User -> Update Database ()
updateUser u =
  do db <- get
     put db{users = updateIx (uID u) u (users db)}

getNextUserId :: Update Database UserId
getNextUserId =
  do db <- get
     let uid = nextUserId db
     put db{nextUserId = UserId ((unUserId uid) + 1)}
     return uid

addUser :: User -> Update Database UserId
addUser u =
  do db <- get
     new_uid <- getNextUserId
     updateUser u{uID=new_uid}
     return new_uid

updateUserVerkey :: UserId -> Maybe Text -> Update Database Bool
updateUserVerkey uid vk =
  do db <- get
     let mu = getOne $ (users db) @= uid
     case mu of
       Nothing ->
         return False
       Just u ->
         updateUser u{uVerkey=vk} >> return True

updateUserPassword :: UserId -> Maybe Text -> Update Database Bool
updateUserPassword uid pass =
  do db <- get
     let mu = getOne $ (users db) @= uid
     case mu of
       Nothing ->
         return False
       Just u ->
         updateUser u{uPassword=pass} >> return True

$(makeAcidic ''Database
  ['addNews
  , 'readNews
  , 'incrementHits
  , 'readHits
  , 'lookupUser
  , 'lookupUserByEmail
  , 'updateUser
  , 'addUser
  , 'updateUserVerkey
  , 'updateUserPassword
  ])

openFrom :: String -> IO (AcidState Database)
openFrom path =
  openLocalStateFrom path defaultFixtures 

