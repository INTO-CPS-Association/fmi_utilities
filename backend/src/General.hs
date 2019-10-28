{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module General where

import Web.Scotty
import qualified Web.Scotty.Internal.Types as Scotty
import qualified Data.Text.Lazy as TL
import qualified System.Directory as Dir
import Control.Monad.IO.Class
import qualified System.FilePath.Posix as Posix

delete :: Scotty.ActionT TL.Text IO ()
delete = do
    uuid <- param "faID"
    let filePath = "./upload" Posix.</> uuid
    liftIO $ Dir.removeDirectoryRecursive filePath
    text $ TL.pack $ "Deleted: " ++ uuid
