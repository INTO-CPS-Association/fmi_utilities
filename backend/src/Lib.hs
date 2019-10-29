{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib
    ( someFunc
    ) where

import qualified FMUAnalyzer
import qualified VDMCheck
import Web.Scotty
import qualified Web.Scotty.Internal.Types as Scotty
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as B
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as TL
import System.Directory as Dir
import Network.Wai.Middleware.Static
import Network.Wai.Internal
import qualified Network.Wai as Wai
import Network.Wai.Middleware.RequestLogger as ReqLogger
import Data.Aeson
import Environment
import Validation
import qualified General
import qualified Network.Wai.Logger as Logger

someFunc :: IO ()
someFunc = do
    appConfig <- loadAppConfig
    workingDir <- Dir.getCurrentDirectory
    putStrLn $ "Working directory: " <> workingDir
    fileLogger <- createLogger 
    Dir.createDirectoryIfMissing False "./upload"
    case appConfig of
        Nothing -> putStrLn "Failed to load appconfig.json"
        Just appConfig' ->
            let checkSizeRequirement' = checkSizeRequirement (sizeLimitGB appConfig')
                base = "./frontend/" 
                in
                scotty (port appConfig') $ do
                    middleware logStdoutDev
                    middleware $ mw fileLogger
                    middleware $ staticPolicy (addBase base)
                    post "/api/fmichecker" $  checkSizeRequirement' VDMCheck.postFile
                    post "/api/fmuanalyzer" $ checkSizeRequirement' FMUAnalyzer.postFMUAnalyzer
                    get "/api/fmuanalyzer/" FMUAnalyzer.getFMUAnalyzer
                    Web.Scotty.delete "/api/general/" General.delete
                    get (function (\req ->
                        case pathInfo req of
                            -- All requests to API should have been handled by this point.
                            "api":_ -> Nothing
                            -- Case for only host name without subdirectory or filename.
                            -- Case for 404's
                            _ -> Just [])) $ Web.Scotty.file $ base ++ "index.html"

checkSizeRequirement :: Integer -> Scotty.ActionT TL.Text IO () -> Scotty.ActionT TL.Text IO ()
checkSizeRequirement sizeLimit a = do
            path <- liftIO $ Dir.canonicalizePath "./upload"
            sizeOK <- liftIO $ Validation.validateEnoughSize path sizeLimit
            if sizeOK
              then a
              -- 507 marks Insufficient Storage
              else status $ mkStatus 507 "Service currently unavailble due to size constraints"

loadAppConfig :: IO (Maybe Environment)
loadAppConfig = decode <$> BS.readFile "appconfig.json"

-- type Middleware = Application -> Application
-- Type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
mw :: Logger.ApacheLogger -> Wai.Middleware
mw logger (app::Wai.Application) (req::Request) (fRes::(Response -> IO ResponseReceived)) =
    app req myResponseFunction
    where
        myResponseFunction :: Response -> IO ResponseReceived
        myResponseFunction response = do
            logger req (Wai.responseStatus response) Nothing
            fRes response

createLogger :: IO Logger.ApacheLogger
createLogger = Logger.apacheLogger <$> createLoggerActions
createLoggerActions :: IO Logger.ApacheLoggerActions
createLoggerActions = let
    -- 5 MB in bytes = 5242880
    mb5InBytes = 5242880
    -- From fast-logger default
    defaultBufSize = 4096 in
        Logger.initLogger
        Logger.FromFallback
        (Logger.LogFile Logger.FileLogSpec {Logger.log_file = "reqLogger", Logger.log_file_size = mb5InBytes, Logger.log_backup_number = 5} defaultBufSize)
        ( return $ B.pack "%d/%b/%Y:%T %z")