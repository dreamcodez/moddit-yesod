module Handler.Root where

import Yesod hiding (update)
import Import
import Data.Acid
import AppState
import Forms
--import Control.Monad


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

-- new implementation (WIP)
getRootR :: Handler RepHtml
getRootR = do
  db <- getDatabase <$> getYesod
  hits <- liftIO $ query db ReadHits
  _ <- liftIO $ update db IncrementHits

  (widget, enctype) <- generateFormPost addNewsItemForm
  defaultLayout [whamlet|
    <form method=post action=@{NewsR} enctype=#{enctype}>
      ^{widget}
      <input type=submit>
    <p>Homepage hits: #{hits}
|]

postNewsR :: Handler RepHtml
postNewsR = do
  ((result, widget), enctype) <- runFormPost addNewsItemForm
  case result of
    (FormSuccess newsItem) -> do
      db <- getDatabase <$> getYesod
      _ <- liftIO $ update db (AddNews newsItem)
      -- defaultLayout [whamlet|<strong>success|]
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
        <a href=#{url n}>#{title n}

    <p>main page hits: #{hits}
  |]

