{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns, TemplateHaskell #-}
module AppTypes where

import Data.Acid
import Data.SafeCopy
import Control.Monad.State  ( get, put )
import Control.Monad.Reader ( ask )
import Control.Applicative  ( (<$>) )
import Prelude
import Data.Typeable
import Data.Text (Text, pack, unpack)
import Data.Map
import Data.Time.Clock (UTCTime)
import Data.IxSet
import Data.Data
import Yesod (PathPiece(..))
import Safe

--

data UserId =
  UserId { unUserId :: Int }
  deriving (Data, Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''UserId)

instance PathPiece UserId where
  toPathPiece = pack . show . unUserId
  fromPathPiece piece =
    case readMay (unpack piece) of
      Nothing -> Nothing
      Just i  -> Just (UserId i)

--

data UserAlias =
  UserAlias { unUserAlias :: Text }
  deriving (Data, Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''UserAlias)

instance PathPiece UserAlias where
  toPathPiece = unUserAlias
  fromPathPiece = Just . UserAlias

--

data Email =
  Email { unEmail :: Text }
  deriving (Data, Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''Email)

--

data User =
  User
    { uID       :: UserId
    , uAlias    :: Maybe UserAlias
    , uEmail    :: Email
    , uPassword :: Maybe Text
    , uVerkey   :: Maybe Text
    , uVerified :: Bool
    }
  deriving (Data, Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''User)

instance Indexable User where
   empty = ixSet 
             [ ixFun $ \User {uID}    -> [uID]
             , ixFun $ \User {uEmail} -> [uEmail]
             ]

--

data Comment =
  Comment
    { cBody :: Text
    , cUser :: User
    , cReplies :: [Comment]
    }
  deriving (Data, Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''Comment)

--

data NewsItem =
  NewsItem
    { niTitle     :: Text
    , niUrl       :: Text
    , niCreated   :: UTCTime
    , niUserId    :: UserId
    , niUserAlias :: UserAlias
    , niComments  :: [Comment]
    }
  deriving (Data, Eq, Ord, Show, Typeable)

$(deriveSafeCopy 0 'base ''NewsItem)

--

data Database =
  Database
    { hits        :: Int
    , news        :: [NewsItem]
    , users       :: IxSet User
    , nextUserId  :: UserId
    }
  deriving (Show, Typeable)

$(deriveSafeCopy 0 'base ''Database)

