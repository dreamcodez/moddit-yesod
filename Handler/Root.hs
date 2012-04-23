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

fakeuser = User (Email "beppu@nowhere.com") Nothing Nothing False

-- new implementation (WIP)
getRootR :: Handler RepHtml
getRootR = do
  db <- getDatabase <$> getYesod
  hits <- liftIO $ query db ReadHits
  _ <- liftIO $ update db IncrementHits
  maid <- maybeAuthId
  now <- liftIO getCurrentTime

  (widget, enctype) <- generateFormPost (addNewsItemForm now fakeuser)
  defaultLayout $ do
    h2id <- lift newIdent
    $(widgetFile "homepage")

postNewsR :: Handler RepHtml
postNewsR = do
  now <- liftIO getCurrentTime
  ((result, widget), enctype) <- runFormPost (addNewsItemForm now fakeuser)
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
        <a href=#{niUrl n}>#{niTitle n} by #{show $ uEmail $ niUser n} at #{show $ niCreated n}

    <p>main page hits: #{hits}
  |]

