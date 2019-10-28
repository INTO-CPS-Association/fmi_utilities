{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- For aeson generic
{-# LANGUAGE DeriveGeneric #-}

module FMUAnalyzer (postFMUAnalyzer, getFMUAnalyzer) where

import qualified Validation as Val
import qualified FileUtilities as FileUtils
import qualified WebUtilities as WebUtils
import Web.Scotty
import qualified Web.Scotty.Internal.Types as Scotty
import qualified Data.Text.Lazy as TL
import Data.Aeson
import GHC.Generics
import qualified Data.UUID.V4 as UUIDV4
import qualified Data.UUID as UUID
import qualified Data.ByteString.Lazy as BS
import qualified System.Directory as Dir
import Control.Monad.IO.Class as MIO
import System.Process
import System.IO
import qualified System.FilePath.Posix as Posix
import Network.HTTP.Types.Status

postFMUAnalyzer :: Scotty.ActionT TL.Text IO ()
postFMUAnalyzer = do
                fs :: [Web.Scotty.File] <- Web.Scotty.files
                params' <- params
                case validateParameters fs params' of
                    Right parsedParameters -> processFile parsedParameters
                    Left err' -> text $ TL.pack err'
    where
        processFile :: FMUAnalyzerParametersParsed -> Scotty.ActionT TL.Text IO ()
        processFile FMUAnalyzerParametersParsed{fappfile=(_,fmuFile), fappn=(_,nVal), fappl=(_,lVal)}=
            let file'@(filename, _) :: (String,BS.ByteString) = WebUtils.getFilenameAndFile fmuFile in
                do
                    uuid :: UUID.UUID <- liftIO UUIDV4.nextRandom
                    directory :: String <- liftIO $ FileUtils.writeFilesToRandomFolder uuid [file']
                    let fmupath = directory Posix.</> filename
                    liftIO $ putStrLn $ "files directory: " ++ directory
                    (stdOut,stdErr) :: (String,String) <- liftIO $ runFMUAnalyzer fmupath directory (show nVal) (show lVal)
                    liftIO $ putStrLn "FMUAnalyzer Finished"
                    Web.Scotty.json FMUAnalyzerJSON {out = stdOut, err = stdErr, faID = UUID.toString uuid}
        validateParameters :: [Web.Scotty.File] -> [Scotty.Param] -> Either String FMUAnalyzerParametersParsed
        validateParameters reqFiles parameters =
            do
                reqFile <- Val.validateSingleFileParameter reqFiles
                (n,l) <- validateNLParameters parameters
                return FMUAnalyzerParametersParsed {fappfile = reqFile, fappn = n, fappl = l}

getFMUAnalyzer :: Scotty.ActionT TL.Text IO ()
getFMUAnalyzer = do
    uuid <- param "faID"
    liftIO $ putStrLn $ "getFMUAnalyzer : uuid: " ++ uuid
    let filePath = "./upload" Posix.</> uuid Posix.</>  "results.zip"
    liftIO $ putStrLn $ "Looking in: " ++ filePath
    fileExists <- liftIO $ Dir.doesPathExist filePath
    if fileExists
      then Web.Scotty.file filePath
      else status $ mkStatus 404 "Could not find file"

runFMUAnalyzer :: String -> String -> String -> String ->  IO (String, String)
runFMUAnalyzer fmuFilePath resultsDirectory n l = do
    (_, Just hout, Just herr, _) <- createProcess (proc "./fmuanalyzer/analyze.sh" [fmuFilePath, resultsDirectory, n, l] ) {std_out = CreatePipe, std_err = CreatePipe }
    hout' <- hGetContents hout
    herr' <- hGetContents herr
    return (hout', herr')

data FMUAnalyzerParametersParsed = FMUAnalyzerParametersParsed {fappfile :: Web.Scotty.File, fappn :: (String, Int), fappl :: (String, Int)}
data FMUAnalyzerJSON = FMUAnalyzerJSON {out :: String, err:: String, faID :: String} deriving (Show, Generic)
instance ToJSON FMUAnalyzerJSON where
    toEncoding = genericToEncoding defaultOptions

validateNLParameters :: [Scotty.Param] -> Either String ((String,Int),(String,Int))
validateNLParameters [p1,p2] = do
    n <- Val.validateStringIntParameter (WebUtils.convertParam p1) "n" (10,100)
    l <- Val.validateStringIntParameter (WebUtils.convertParam p2) "l" (10,100)
    return (n,l)
