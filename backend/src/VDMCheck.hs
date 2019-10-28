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

postModelDescription :: Scotty.ActionT TL.Text IO ()
postModelDescription = do
        fs :: [Web.Scotty.File] <- Web.Scotty.files
        case Val.validateSingleFileParameter fs of
            Left err -> text (TL.pack err)
            Right (_,mdFile) ->
                (liftIO . runCheck . WebUtils.getFilenameAndFile) mdFile >>=
                    text . TL.pack

runCheck :: (String,BS.ByteString) -> IO String
runCheck file'@(filename, _) = do
    dir :: String<- FileUtils.writeFilesToRandomFolderGenUUID [file']
    (_, stdOut, _) :: (ExitCode, String,String) <- execVDMCheck (dir Posix.</> filename)
    putStrLn $  "Stored VDMCheck files in: " <> dir
    liftIO $ Dir.removeDirectoryRecursive dir
    return stdOut;
    where 
        execVDMCheck :: String -> IO (ExitCode, String, String)
        execVDMCheck path = readProcessWithExitCode "./vdmcheck-0.0.1/VDMCheck.sh" [path] ""