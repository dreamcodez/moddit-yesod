{-# LANGUAGE QuasiQuotes, TemplateHaskell, MultiParamTypeClasses,
    OverloadedStrings, TypeFamilies #-}
module Forms where

import Control.Applicative ((<$>), (<*>), pure)
import Prelude
import Yesod
import Yesod.Form ()
import Foundation (App)
--import Yesod.Form.Jquery
--import Data.Text
--import Data.String

import AppTypes
import Data.Time.Clock (UTCTime)

addNewsItemForm a1 a2 a3 =
  renderDivs (customform a1 a2 a3)
  where customform :: UTCTime -> UserId -> UserAlias -> AForm App App NewsItem
        customform now uid ualias =
          NewsItem
            <$> areq textField "Title" Nothing
            <*> areq textField "URL" Nothing
            <*> pure now
            <*> pure uid
            <*> pure ualias
            <*> pure []


{-
--personForm :: Html -> MForm Synopsis Synopsis (FormResult Person, Widget)
personForm = renderDivs $ Person
    <$> areq textField "Name" Nothing
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) "Birthday" Nothing
    <*> aopt textField "Favorite color" Nothing
    <*> areq emailField "Email address" Nothing
    <*> aopt urlField "Website" Nothing
-}

