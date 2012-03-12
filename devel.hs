{-# LANGUAGE PackageImports #-}
import "moddit" Application (getApplicationDev)
import Network.Wai.Handler.Warp
    (runSettings, defaultSettings, settingsPort)
import Control.Concurrent (forkIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import System.Environment (getEnv)

getEnvWithDefault v d = getEnv v `catch` const (return d)

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    portString <- getEnvWithDefault "MODDIT_PORT" $ show port
    let port' = read portString :: Int
    forkIO $ runSettings defaultSettings
        { settingsPort = port'
        } app
    loop

loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "dist/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
