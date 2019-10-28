module FileUtilities where

import qualified System.Directory as Dir
import qualified Data.ByteString.Lazy as BS
import qualified Data.UUID.V4 as UUIDV4
import qualified Data.UUID as UUID
import qualified System.FilePath.Posix as Posix

writeFilesToRandomFolderGenUUID :: [(String,BS.ByteString)] -> IO String
writeFilesToRandomFolderGenUUID files = do
    uuid <- UUIDV4.nextRandom
    writeFilesToRandomFolder uuid files

writeFilesToRandomFolder :: UUID.UUID -> [(String,BS.ByteString)] -> IO String
writeFilesToRandomFolder uuid files = do
    let directory = "./upload" Posix.</> UUID.toString uuid
    Dir.createDirectoryIfMissing True directory
    mapM_ (\(fn, fc) -> BS.writeFile (directory Posix.</> fn) fc ) files
    fmap (`Posix.combine` directory) Dir.getCurrentDirectory >>= Dir.canonicalizePath
