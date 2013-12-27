-- Contains the types for dealing with DAR files.

-- Future Plans:
-- Function to convert associated list or a map to a [DataNode]
-- Make a typeclass for the above, so users can make anthing into DataNodes
-- Add the index
-- Get single named DataNode
-- Return DataNodes as a map
-- Or maybe make another typeclass for this
-- Append single DataNode
-- DataNode should have size parameters for performace reasons
-- Make things strict where nessesary

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

-- A standard DAR file for comparison perposes.
defaultDar = Dar (DarH (BC.pack "DAR") 6 0 0 0) (DataH (BC.pack ".data") []) 

--
-- **************************************************
-- Functions for getting a DAR file and its DataNodes from a ByteString.

-- Tries to gets the DataNodes from a DAR file (in ByteString from),
-- If the DAR file can't be parsed an error message is created that
-- includes the position where the error was and the number of bytes
-- remaining unparsed.
getDARFile :: BC.ByteString -> Either String [DataNode]
getDARFile bs = 
  case (runGetOrFail getDARFile' bs) of
    -- Combines all the error details in to a single message.
    Left (leftBs, pos, msg) -> Left $ msg ++ " at " ++ (show pos) ++ " bytes"
                               ++ (if BC.null leftBs
                                   then " and no more bytes to consume."
                                   else " and " ++ (show $ BC.length leftBs)
                                        ++ " bytes left to consume.")
    -- Extract the DataNodes from the DAR file.
    Right (_, _, val) -> Right $ dataNodes . dataDAR $ val

getDARFile' :: Get DarFile
getDARFile' = do
  header <- getDARHeader
  (Dar header) <$> (getDATAHeader $ nodeCount header)  

getDARHeader :: Get DarHeader
getDARHeader = do
  let header = headerDAR defaultDar

  -- Get 3 bytes that identify this as a DAR file and check it.
  dar <- getLazyByteString 3
  when (dar /= (identifyDAR header))
    (fail "Not a DAR file")

  -- Get the DAR file's major and minor version numbers and check them.
  (maV, miV) <- (,) <$> getWord8 <*> getWord8
  when (maV /= majorV header || miV /= minorV header)
    (fail "DAR file version incompatible")
    
  -- Get the number of DataNodes and the checksum.
  (DarH dar maV miV) <$> getWord64le <*> getWord32le

getDATAHeader :: Word64 -> Get DataHeader
getDATAHeader nodeNum = do
  -- Get 5 bytes that identify this as the data header of a DAR file and check it.
  name <- getLazyByteString 5
  when (name /= (identifyDATA $ dataDAR defaultDar))
    (fail "No data header found in DAR file")

                   -- Get the DataNodes from the data block.
  (DataH name) <$> replicateM (fromIntegral nodeNum) getDATANode

getDATANode :: Get DataNode
getDATANode = do
  -- Get the length of the DataNode's name and get that many bytes.
  nName <- getLazyByteString . fromIntegral =<< getWord32le
  -- Get the length of the DataNode's data and get that many bytes.
  nData <- getLazyByteString . fromIntegral =<< getWord64le
  
  return $ DataN nName nData

--
-- **************************************************
-- Functions for putting DataNodes into DAR a file and than into a ByteStrings.

putDARFile :: [DataNode] -> BC.ByteString
putDARFile = runPut . putDARFile' 

putDARFile' :: [DataNode] -> Put
putDARFile' nodes = do
  putDARHeader . fromIntegral . length $ nodes
  putDATAHeader nodes

putDARHeader :: Word64 -> Put
putDARHeader nodeN = do
  let header = headerDAR defaultDar
  putLazyByteString $ identifyDAR header -- 3 bytes that identify this as a DAR file.
  putWord8 $ majorV header               -- Major Version of the DAR file.
  putWord8 $ minorV header               -- Minor Version of the DAR file.
  putWord64le nodeN                      -- Number of DataNodes in the DAR file.
  putWord32le 0                          -- Checksum (not used).

putDATAHeader :: [DataNode] -> Put
putDATAHeader nodes = do
  putLazyByteString . identifyDATA $ dataDAR defaultDar -- 5 bytes start data block.
  mapM_ putDATANode nodes                               

putDATANode :: DataNode -> Put
putDATANode node = do
  -- Length of the DataNode's name in bytes.
  putWord32le . fromIntegral . BC.length $ identifyDNode node
  -- The DataNode's name.
  putLazyByteString $ identifyDNode node

  -- Length of the DataNode's data in bytes.
  putWord64le . fromIntegral . BC.length $ contentDNode node
  -- The DataNode's data.
  putLazyByteString $ contentDNode node