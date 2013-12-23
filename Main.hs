-- A tool to pack and unpack .dar files, specification is found in the README.

import Control.Applicative
import Control.Monad
import Data.Binary
import Data.Binary.Get          
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.IO

data DarFile = Dar { headerDAR :: DarHeader
                   , dataDAR :: DataHeader
                   } 

data DarHeader = DarH { identifyDAR :: B.ByteString 
                      , majorV :: Word8
                      , minorV :: Word8
                      , nodeCount :: Word64
                      , checksum :: Word32
                      } deriving (Show)
                                       
-- Doesn't compare nodeCount or checksum as they're not required.
-- This makes sure the file is a DAR file and is of the right version.
instance Eq DarHeader where
  darH1 == darH2 = identifyDAR darH1 == identifyDAR darH2
                   && majorV darH1 == majorV darH2
                   && minorV darH1 == minorV darH2


data DataHeader = DataH { identifyDATA :: B.ByteString --Should be ".data"
-- not part of spec yet?, lengthDATA :: Word64
                        , dataNodes :: [DataNode] -- Not sure if this is best?
                        } deriving (Show)

-- Are lenidentifyDNode and lengthDNode actually needed in the data type?
data DataNode = DataN { identifyDNode :: B.ByteString
                      , contentDNode :: B.ByteString
                      } deriving (Show)

-- Check the header and .data here?
getDARFile :: Get DarFile
getDARFile = do
  header <- getDARHeader
  (Dar header) <$> (getDATAHeader $ nodeCount header)  

getDARHeader :: Get DarHeader
getDARHeader = DarH <$> getLazyByteString 3 <*> getWord8 <*> getWord8
               <*> getWord64le <*> getWord32le

-- Check that the dataHeader ".data" exists.
getDATAHeader :: Word64 -> Get DataHeader
getDATAHeader nodeN = DataH <$> getLazyByteString 5 <*> (sequence $ getDATANodes nodeN)

getDATANodes :: Word64 -> [Get DataNode]
getDATANodes nodeN = case nodeN == 0 of
  True -> []
  False -> getDATANode : (getDATANodes $ nodeN - 1)
  
getDATANode :: Get DataNode
getDATANode = do  
  nName <- getWord32le >>= getLazyByteString . fromIntegral
  nData <- getWord64le >>= getLazyByteString . fromIntegral 
  return $ DataN nName nData

defaultDarHeader = DarH { identifyDAR = BC.pack "DAR"
                        , majorV = 6
                        , minorV = 0
                        -- , nodeCount = cDAR_NODE_COUNT_TEST 
                        , checksum = 0
                        }

main = do
  args <- concat <$> getArgs -- Just so it compiles.
  darfile <- B.readFile args

  let decodedDar = runGet getDARFile darfile

  case (headerDAR decodedDar == defaultDarHeader) of
    True -> putStrLn $ args ++ " is TRUE DAR file.\n\nList of DataNodes:"
            ++ (concatMap (\(DataN n d) -> unwords ["\nDataNode name:", BC.unpack n
                                                   , "\nDataLenght:", show $ B.length d
                                                   , "\nData:", BC.unpack d, "\n"])
                $ dataNodes $ dataDAR decodedDar)
    False -> putStrLn $ args ++ " is FALSE DAR file, .data block possibly corrupt."



