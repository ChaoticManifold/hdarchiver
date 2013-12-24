-- Contains the types for dealing with DAR files.

-- Future Plans:
-- Function to convert associated list or a map to a [DataNode]
-- Make a typeclass for the above, so users can make anthing into DataNodes
-- Add the index
-- Get single named DataNode
-- Return DataNodes as a map
-- Or maybe make another typeclass for this.
-- Append single DataNode
-- DataNode should have size parameters
-- Make things strict where nessesary
-- Add error checks when getting DAR files

module Data.DarTypes
       (
         DarFile (..), DarHeader (..), DataHeader (..), DataNode (..),
         getDARFile, putDARFile
       ) where
                
import Control.Applicative
import Control.Monad
import Control.Monad.Loops (unfoldrM)
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word

data DarFile = Dar { headerDAR :: DarHeader
                   , dataDAR :: DataHeader
                   } 

data DarHeader = DarH { identifyDAR :: BC.ByteString 
                      , majorV :: Word8
                      , minorV :: Word8
                      , nodeCount :: Word64
                      , checksum :: Word32
                      } deriving (Show)
                                       
data DataHeader = DataH { identifyDATA :: BC.ByteString --Should be ".data"
-- not part of spec yet?, lengthDATA :: Word64
                        , dataNodes :: [DataNode] -- Not sure if this is best?
                        } deriving (Show)

-- Are lenidentifyDNode and lengthDNode actually needed in the data type?
data DataNode = DataN { identifyDNode :: BC.ByteString
                      , contentDNode :: BC.ByteString
                      } deriving (Show)

-- Change second return type so user doen't have to know about DataNodes.
getDARFile :: BC.ByteString -> Either String [DataNode]
getDARFile bs = 
  case (runGetOrFail getDARFile' bs) of
    Left (leftBs, pos, msg) -> Left $ msg ++ " at " ++ (show pos) ++ " bytes"
                               ++ (if BC.null leftBs
                                   then " and no more bytes to consume."
                                   else " and " ++ (show $ BC.length leftBs)
                                        ++ " bytes left to consume.")
    Right (_, _, val) -> Right $ dataNodes . dataDAR $ val

-- Check the header and .data here?
getDARFile' :: Get DarFile
getDARFile' = do
  header <- getDARHeader
  (Dar header) <$> (getDATAHeader $ nodeCount header)  

getDARHeader :: Get DarHeader
getDARHeader = do
  let header = headerDAR defaultDar

  dar <- getLazyByteString 3
  when (dar /= (identifyDAR header))
    (fail "Not a DAR file")

  (maV, miV) <- (,) <$> getWord8 <*> getWord8
  when (maV /= majorV header || miV /= minorV header)
    (fail "DAR file version incompatible")
    
  (DarH dar maV miV) <$> getWord64le <*> getWord32le

-- Check that the dataHeader ".data" exists.
getDATAHeader :: Word64 -> Get DataHeader
getDATAHeader nodeNum = do
  let dataName = dataDAR defaultDar
  
  name <- getLazyByteString 5
  when (name /= identifyDATA dataName)
    (fail "No data header found in DAR file")

  -- The unfoldrM calls getDATANode nodeNum times. Getting all the DataNodes.
  (DataH name) <$> unfoldrM (\x -> if x == 0 
                                   then return Nothing
                                   else do
                                     node <- getDATANode
                                     return $ Just $ (node, x - 1)) nodeNum
  
getDATANode :: Get DataNode
getDATANode = do  
  nName <- getWord32le >>= getLazyByteString . fromIntegral
  nData <- getWord64le >>= getLazyByteString . fromIntegral 
  return $ DataN nName nData

defaultDar = Dar defaultDarHeader defaultDataHeader 

defaultDarHeader = DarH { identifyDAR = BC.pack "DAR"
                        , majorV = 6
                        , minorV = 0
                        , nodeCount = 0
                        , checksum = 0
                        }

defaultDataHeader = DataH { identifyDATA = BC.pack ".data"
                          , dataNodes = []
                          }

putDARFile :: [DataNode] -> Put
putDARFile nodes = do
  putDARHeader . fromIntegral . length $ nodes
  putDATAHeader nodes

putDARHeader :: Word64 -> Put
putDARHeader nodeN = do
  let header = headerDAR defaultDar
  putLazyByteString $ identifyDAR header
  putWord8 $ majorV header
  putWord8 $ minorV header
  putWord64le nodeN
  putWord32le 0

putDATAHeader :: [DataNode] -> Put
putDATAHeader nodes = do
  let headerD = dataDAR defaultDar
  putLazyByteString $ identifyDATA headerD
  mapM_ putDATANode nodes

putDATANode :: DataNode -> Put
putDATANode node = do
  putWord32le . fromIntegral . BC.length $ identifyDNode node
  putLazyByteString $ identifyDNode node
  putWord64le . fromIntegral . BC.length $ contentDNode node
  putLazyByteString $ contentDNode node
 
