{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module VDMCheck where

import qualified WebUtilities as WebUtils
import qualified FileUtilities as FileUtils
import qualified Validation as Val
import Web.Scotty
import qualified Web.Scotty.Internal.Types as Scotty
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Lazy as BS
import qualified System.Directory as Dir
import System.Process
import Control.Monad.IO.Class
import qualified System.FilePath.Posix as Posix
import System.Exit (ExitCode)

postFile :: Scotty.ActionT TL.Text IO ()
postFile = do
        fs :: [Web.Scotty.File] <- Web.Scotty.files
        params' <- params
        case Val.validateSingleFileParameter fs of
            Left err -> text (TL.pack err)
            Right (_,mdFile) ->
                case validateParams params' of 
                    Left err -> text (TL.pack err)
                    Right fmiVersion -> 
                        (liftIO . runCheck fmiVersion . WebUtils.getFilenameAndFile) mdFile >>= text . TL.pack

validateParams :: [Param] -> Either String FmiVersion
validateParams [] = Left "Missing fmiVersino parameteter"
validateParams [("fmiVersion","FMI2")] = Right FMI2
validateParams [("fmiVersion","FMI3")] = Right FMI3
validateParams ps = Left $ "Invalid params: " ++ show ps

data FmiVersion = FMI2 | FMI3

runCheck :: FmiVersion -> (String,BS.ByteString) -> IO String
runCheck fmiVersion file'@(filename, _) = do
    dir :: String<- FileUtils.writeFilesToRandomFolderGenUUID [file']
    (_, stdOut, _) :: (ExitCode, String,String) <- execVDMCheck (dir Posix.</> filename) fmiVersion
    putStrLn $  "Stored VDMCheck files in: " <> dir
    liftIO $ Dir.removeDirectoryRecursive dir
    return stdOut;
    where 
        execVDMCheck :: String -> FmiVersion -> IO (ExitCode, String, String)
        execVDMCheck path fmi = readProcessWithExitCode (getVdmCheckPath fmi) [path] ""
        getVdmCheckPath :: FmiVersion -> String
        getVdmCheckPath FMI2 = "./vdmcheck-0.0.2/VDMCheck2.sh"
        getVdmCheckPath FMI3 = "./vdmcheck-0.0.3/VDMCheck3.sh"