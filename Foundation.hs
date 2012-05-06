    {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TemplateHaskell, TypeFamilies #-} 
module Foundation
    ( App (..)
    , Route (..)
    , AppMessage (..)
    , resourcesApp
    , Handler
    , Widget
    , module Yesod.Core
    , module Settings
    , liftIO
    ) where

import Data.Maybe (isJust)
import Prelude
import Yesod.Core hiding (Route)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static
import Settings.StaticFiles
import Yesod.Logger (Logger, logMsg, formatLogText)
import qualified Settings
import Settings (Extra (..), widgetFile)
import Control.Monad.IO.Class (liftIO)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import Control.Applicative ((<$>))
import AppState
import Yesod.Form
import Yesod.Auth
import Yesod.Auth.Dummy
import Yesod.Auth.Email
import Network.Mail.Mime
import qualified Data.Text.Lazy.Encoding
import Text.Blaze.Renderer.Utf8 (renderHtml)
import Text.Hamlet (shamlet)
import Text.Shakespeare.Text (stext)
import Data.Text
import AppState
import AppTypes

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings    :: AppConfig DefaultEnv Extra
    , getLogger   :: Logger
    , getStatic   :: Static -- ^ Settings for static file serving.
    , getDatabase :: AcidState Database
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings
    
    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 120

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        maid <- maybeAuthId

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            -- $(widgetFile "normalize")
            addStylesheet $ StaticR bootstrap_css_bootstrap_min_css
            addStylesheet $ StaticR css_main_css
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    messageLogger y loc level msg =
      formatLogText (getLogger y) loc level msg >>= logMsg (getLogger y)

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal (const $ Left ()) base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

-- Tells our application to use the standard English messages.
-- If you want i18n, then you can supply a translating function instead.
instance RenderMessage t FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth App where
    type AuthId App = UserId

    loginDest _ = RootR
    logoutDest _ = RootR
    authPlugins _ = [authEmail]
    --authPlugins _ = [authDummy]

    -- in the future, credsIdent might be something other than an email, i.e. facebook credsIdent
    getAuthId Creds{credsIdent} =
      do db <- getDatabase <$> getYesod
         let eml = Email credsIdent
         mu <- query' db (LookupUserByEmail eml)
         return $
           case mu of
             Nothing -> Nothing
             Just u  -> Just (uID u)

    authHttpManager = error "n/a"

-- Here's all of the email-specific code
instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    addUnverified email verkey =
      do db <- getDatabase <$> getYesod
         let eml = Email email
         -- in the future, when we support facebook login etc, alias should be user defined/unique (instead of email)
         uid <- update' db $ AddUser (User (UserId (-1)) (Just $ UserAlias email) eml Nothing (Just verkey) False)
         return uid

    sendVerifyEmail email _ verurl =
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8 [stext|
Please confirm your email address by clicking on the link below.

\#{verurl}

Thank you
|]
            , partHeaders = []
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [shamlet|
<p>Please confirm your email address by clicking on the link below.
<p>
    <a href=#{verurl}>#{verurl}
<p>Thank you
|]
            , partHeaders = []
            }
    getVerifyKey uid =
      do db <- getDatabase <$> getYesod
         mu <- query' db (LookupUser uid)
         case mu of
           Nothing -> return Nothing
           Just u  -> return $ uVerkey u

    setVerifyKey uid key =
      do db <- getDatabase <$> getYesod
         update' db (UpdateUserVerkey uid $ Just key)
         return ()

    verifyAccount uid =
      do db <- getDatabase <$> getYesod
         mu <- query' db (LookupUser uid)
         case mu of
           Nothing ->
             return Nothing
           Just u ->
             do update' db (UpdateUser u)
                return $ Just (uID u)

    getPassword uid =
      do db <- getDatabase <$> getYesod
         mu <- query' db (LookupUser uid)
         case mu of
           Nothing -> return Nothing
           Just u  -> return $ uPassword u

    setPassword uid pass =
      do db <- getDatabase <$> getYesod
         update' db (UpdateUserPassword uid $ Just pass)
         return ()

    getEmailCreds email_txt =
      do db <- getDatabase <$> getYesod
         mu <- query' db (LookupUserByEmail (Email email_txt))
         case mu of
           Nothing -> return Nothing
           Just u  -> (return . Just)
             EmailCreds
               { emailCredsId = (uID) u
               , emailCredsAuthId = (Just . uID) u
               , emailCredsStatus = isJust (uPassword u)
               , emailCredsVerkey = uVerkey u
               }

    getEmail uid =
      do db <- getDatabase <$> getYesod
         mu <- query' db (LookupUser uid)
         case mu of
           Nothing -> return Nothing
           Just u  -> (return . Just . unEmail . uEmail) u

