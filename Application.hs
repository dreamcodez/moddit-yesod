{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , getApplicationDev
    ) where

import Import
import Settings (parseExtra)
import Settings.StaticFiles (staticSite)
import Yesod.Default.Config
import Yesod.Default.Main (defaultDevelApp)
import Yesod.Default.Handlers (getFaviconR, getRobotsR)
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
#endif
import Network.Wai (Application)
import qualified AppState as AS
import System.Environment (getEnv)

-- Import all relevant handler modules here.
import Handler.Root

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication conf logger = do
    s <- staticSite
    acid <- AS.openFrom "_appstate"
    let foundation = App conf setLogger s acid
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

-- for yesod devel
-- lets start customizing this codebase, to set our code apart, we start indenting 2 lines ; )
getApplicationDev :: IO (Int, Application)
getApplicationDev = do
  config <- loadConfig(configSettings Development){csParseExtra = parseExtra}
  portStr <- getEnvWithDefault "MODDIT_PORT" (show $ appPort config)

  -- assign new env-influenced vals
  let config' = config{appPort = read portStr}

  defaultDevelApp (return config') getApplication

  where getEnvWithDefault v d = getEnv v `catch` const (return d)

