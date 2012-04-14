{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings #-}
module Forms where

import Import
import Yesod
import Yesod.Form
import Text.Blaze.Internal

data NewsItem = NewsItem
  { title :: Text
  , url   :: Text
  }

--addNewsItemForm :: Text.Blaze.Internal.Html -> 
addNewsItemForm =
  aFormToForm aform
  where aform =
          NewsItem
            <$> areq textField "Title" Nothing
            <*> areq textField "URL" Nothing

