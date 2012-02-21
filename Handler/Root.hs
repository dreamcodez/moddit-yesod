module Handler.Root where

import Import
import Data.Acid
import AppState


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