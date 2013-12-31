-- Contains the types for dealing with DAR files.

-- Future Plans:
-- Function to convert associated list or a map to a [DataNode]
-- Make a typeclass for the above, so users can make anthing into DataNodes
-- Get single named DataNode
-- Return DataNodes as a map
-- Or maybe make another typeclass for this
-- Append single DataNode
-- Make things strict where nessesary

module Data.DarTypes
       (
         DarFile (..), DarHeader (..),
         IndexHeader (..), IndexNode (..),
         DataHeader (..), DataNode (..),
         getDARFile, getDARIndex, getDARLookup, putDARFile
       ) where
                
import Control.Applicative
import Control.Monad
import Control.Monad.Loops (unfoldrM)
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List (find)
import Data.Word

data DarFile = Dar { headerDAR :: DarHeader
                   , indexDAR :: IndexHeader
                   , dataDAR :: DataHeader
                   } 

data DarHeader = DarH { identifyDAR :: BC.ByteString -- 3 bytes 
                      , majorV :: Word8
                      , minorV :: Word8
                      , nodeCount :: Word64
                      , checksum :: Word32
                      } deriving (Show) -- total: 17 bytes

data IndexHeader = IndexH { identifyINDEX :: BC.ByteString --Should be ".index" 6 bytes
                          , indexNodes :: [IndexNode]
                          } deriving (Show)

data IndexNode = IndexN { identifyINodeLen :: Word32 -- 4 bytes
                        , identifyINode :: BC.ByteString -- (1 to maxBound Word32) bytes
                        , nodePosition :: Word64 -- 8 bytes
                        } deriving (Show)

data DataHeader = DataH { identifyDATA :: BC.ByteString --Should be ".data" 5 bytes
                        , dataNodes :: [DataNode] 
                        } deriving (Show)

data DataNode = DataN { identifyDNodeLen :: Word32 -- 4 bytes
                      , identifyDNode :: BC.ByteString -- (1 to maxBound Word32) bytes
                      , contentDNodeLen :: Word64 -- 8 bytes
                      , contentDNode :: BC.ByteString -- (1 to maxBound Word64) bytes
                      } deriving (Show)

-- A standard DAR file for comparison perposes.
defaultDar = Dar 
             (DarH (BC.pack "DAR") 6 0 0 0)
             (IndexH (BC.pack ".index") [])
             (DataH (BC.pack ".data") [])

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
  (Dar header) <$> (getINDEXHeader $ nodeCount header)
    <*> (getDATAHeader $ nodeCount header)

getDARIndex :: BC.ByteString -> Either String [IndexNode]
getDARIndex bs =
  case (runGetOrFail getDARIndex' bs) of
    Left (leftBs, pos, msg) -> Left $ msg ++ " at " ++ (show pos) ++ " bytes"
                               ++ (if BC.null leftBs
                                   then " and no more bytes to consume."
                                   else " and " ++ (show $ BC.length leftBs)
                                        ++ " bytes left to consume.")
    Right (_, _, val) -> Right $ indexNodes . indexDAR $ val

getDARIndex' :: Get DarFile
getDARIndex' = do
  header <- getDARHeader
  (Dar header) <$> (getINDEXHeader $ nodeCount header)
    <*> (pure $ dataDAR defaultDar)

getDARLookup :: BC.ByteString -> BC.ByteString -> Either String DataNode
getDARLookup name bs = 
  case (runGetOrFail getDARIndex' bs) of
    Left (leftBs, pos, msg) -> Left $ msg ++ " at " ++ (show pos) ++ " bytes"
                               ++ (if BC.null leftBs
                                   then " and no more bytes to consume."
                                   else " and " ++ (show $ BC.length leftBs)
                                        ++ " bytes left to consume.")
    Right (_, _, iNodes) -> do
      let index = find (\n -> identifyINode n == name) $ indexNodes . indexDAR $ iNodes
      case index of
        Nothing -> Left "No DataNode with that name exists."
        Just iNode -> do
          let nPos = nodePosition iNode
          Right $ runGet getDATANode (BC.drop (fromIntegral nPos) bs)
    
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

getINDEXHeader :: Word64 -> Get IndexHeader
getINDEXHeader nodeNum = do
  name <- getLazyByteString 6
  when (name /= (identifyINDEX $ indexDAR defaultDar))
    (fail "No index header found in DAR file")

  (IndexH name) <$> replicateM (fromIntegral nodeNum) getINDEXNode

getINDEXNode :: Get IndexNode
getINDEXNode = do
  nNameLen <- getWord32le
  nName <- getLazyByteString $ fromIntegral nNameLen
  nPos <- getWord64le

  return $ IndexN nNameLen nName nPos

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
  nNameLen <- getWord32le
  nName <- getLazyByteString $ fromIntegral nNameLen
  nDataLen <- getWord64le
  nData <- getLazyByteString $ fromIntegral nDataLen
  
  return $ DataN nNameLen nName nDataLen nData

--
-- **************************************************
-- Functions for putting DataNodes into DAR a file and than into a ByteStrings.

putDARFile :: [DataNode] -> BC.ByteString
putDARFile = runPut . putDARFile' 

putDARFile' :: [DataNode] -> Put
putDARFile' nodes = do
  putDARHeader . fromIntegral . length $ nodes
  putINDEXHeader nodes
  putDATAHeader nodes

putDARHeader :: Word64 -> Put
putDARHeader nodeN = do
  let header = headerDAR defaultDar
  putLazyByteString $ identifyDAR header -- 3 bytes that identify this as a DAR file.
  putWord8 $ majorV header               -- Major Version of the DAR file.
  putWord8 $ minorV header               -- Minor Version of the DAR file.
  putWord64le nodeN                      -- Number of DataNodes in the DAR file.
  putWord32le 0                          -- Checksum (not used).

putINDEXHeader :: [DataNode] -> Put
putINDEXHeader nodes = do            --- here lies the bug ------------------------
  let nSizeAndName = reverse . fst . foldl (\(acc,p) (i,x) -> ((i,p):acc, p+x)) ([],0)
                     $ zip
                     (map (\x -> (identifyDNodeLen x, identifyDNode x) ) nodes)
                     (map getsize nodes)
      getsize x = 4 + (fromIntegral $ identifyDNodeLen x)
                  + 8 + (fromIntegral $ contentDNodeLen x)
      preSize = 6 + (sum $ map (\x -> 4 + (fromIntegral $ identifyDNodeLen x) + 8) nodes)
                + 17 + 5 -- DAR header, data header - .data

  putLazyByteString $ identifyINDEX $ indexDAR defaultDar

-- lol! offset needs to calculate base on size of the previous nodes summed together.
  mapM_ (flip putINDEXNode preSize) nSizeAndName

putINDEXNode :: ((Word32, BC.ByteString),Word64) -> Word64 -> Put
putINDEXNode ((nl, n), s) preS = do
  putWord32le  nl
  putLazyByteString n
  putWord64le $ preS + s

putDATAHeader :: [DataNode] -> Put
putDATAHeader nodes = do
  putLazyByteString . identifyDATA $ dataDAR defaultDar -- 5 bytes start data block.
  mapM_ putDATANode nodes                               

putDATANode :: DataNode -> Put
putDATANode node = do
  -- Length of the DataNode's name in bytes.
  putWord32le . fromIntegral $ identifyDNodeLen node
  -- The DataNode's name.
  putLazyByteString $ identifyDNode node

  -- Length of the DataNode's data in bytes.
  putWord64le . fromIntegral $ contentDNodeLen node
  -- The DataNode's data.
  putLazyByteString $ contentDNode node
