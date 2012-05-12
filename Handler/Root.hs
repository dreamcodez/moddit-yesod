module Handler.Root where

import Yesod hiding (update)
import Yesod.Auth
import Import
import Data.Acid
import AppState
import Forms
import Data.Time.Clock (getCurrentTime)
--import Control.Monad
import AppTypes
import Text.JSON


{-
-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  db <- getDatabase <$> getYesod
  
  defaultLayout $ do
    -- track site hits
    hits <- liftIO $ query db ReadHits
    _ <- liftIO $ update db IncrementHits

    h2id <- lift newIdent
    setTitle "moddit homepage"
    $(widgetFile "homepage")
-}

-- get user object of current logged in user
maybeUser =
  do muid <- maybeAuthId
     case muid of
       Nothing  -> return Nothing
       Just uid ->
         do db <- getDatabase <$> getYesod
            query' db (LookupUser uid)

-- get user object of current logged in user, and require that they have an alias
maybeAliasedUser =
  do muser <- maybeUser
     case muser of
       Nothing   -> return Nothing
       Just user ->
         case uAlias user of
           Nothing -> return Nothing
           Just _  -> return (Just user)
         
-- new implementation (WIP)
getRootR :: Handler RepHtml
getRootR = do
  db <- getDatabase <$> getYesod
  hits <- liftIO $ query db ReadHits
  _ <- liftIO $ update db IncrementHits
  maid <- maybeAuthId
  now <- liftIO getCurrentTime

  defaultLayout $(widgetFile "homepage")

getNewsFormR :: Handler RepHtml
getNewsFormR =
  do muser <- maybeAliasedUser
     case muser of
       Nothing -> notFound
       Just User{uID=uid, uAlias=Just ua} ->
         do now <- liftIO getCurrentTime
            (widget, enctype) <- generateFormPost (addNewsItemForm now uid ua)
            defaultLayout $(widgetFile "news-form")

postNewsR :: Handler RepHtml
postNewsR =
  do muser <- maybeAliasedUser
     case muser of
       Nothing -> notFound
       Just User{uID=uid, uAlias=Just ua} ->
         do now <- liftIO getCurrentTime
            ((result, widget), enctype) <- runFormPost (addNewsItemForm now uid ua)
            case result of
              (FormSuccess newsItem) -> do
                db <- getDatabase <$> getYesod
                _ <- liftIO $ update db (AddNews newsItem)
                redirect NewsR
              _ -> do
                defaultLayout [whamlet|
<p>Invalid input, let's try again.
<form method=post action=@{RootR} enctype=#{enctype}>
  ^{widget}
  <input type=submit>
|]

getNewsR :: Handler RepHtml
getNewsR = do
  db <- getDatabase <$> getYesod
  news <- liftIO $ query db (ReadNews 10)
  hits <- liftIO $ query db ReadHits
  defaultLayout [whamlet|
    $forall n <- news
      <p>
        <a href=#{niUrl n}>#{niTitle n} by #{show $ niUserAlias n} at #{show $ niCreated n}

    <p>main page hits: #{hits}
  |]

getUsersR :: Handler RepHtml
getUsersR = do
  db <- getDatabase <$> getYesod
  users <- query' db (ReadUsers 100)
  defaultLayout [whamlet|
    <p>#{show users}
  |]

