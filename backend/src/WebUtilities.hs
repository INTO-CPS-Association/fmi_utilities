module WebUtilities where
import qualified Data.ByteString.Lazy as BSL
import Network.Wai.Parse
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.Text.Lazy as TL
import qualified Web.Scotty.Internal.Types as Scotty

getFilenameAndFile :: FileInfo BSL.ByteString -> (String, BSL.ByteString)
getFilenameAndFile fileInfo = (BSC8.unpack (fileName fileInfo), fileContent fileInfo)

convertParam :: Scotty.Param -> (String, String)
convertParam (a,b) = (TL.unpack a, TL.unpack b)