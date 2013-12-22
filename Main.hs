-- A tool to pack and unpack .dar files, specification is found in the README.

import Control.Applicative
import Data.Binary
import Data.Binary.Get          
import Data.Binary.Put
import Data.Int
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.IO

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

data DataNode = DataN { identifyDNode :: DataNodeName
                      , lengthDNode :: Word64
                      , contentDNode :: B.ByteString
                      } deriving (Show)

data DataNodeName = NodeLN { nodeNameLen :: Word32
                           , nodeName :: B.ByteString
                           } deriving (Show)

-- Constants for the Dar header block.
cDAR_IDENTIFICATION = BC.pack "DAR" :: B.ByteString
cDAR_MAJOR_VERSION = 6 :: Word8
cDAR_MINOR_VERSION = 0 :: Word8
cDAR_DATANODE_BLOCK = BC.pack ".data" :: B.ByteString

defaultDarHeader = DarH { identifyDAR = cDAR_IDENTIFICATION
                        , majorV = cDAR_MAJOR_VERSION
                        , minorV = cDAR_MINOR_VERSION
                        -- , nodeCount = cDAR_NODE_COUNT_TEST 
                        -- , checksum = cDAR_CHECKSUM_TEST
                        }

main = do
  args <- getArgs



