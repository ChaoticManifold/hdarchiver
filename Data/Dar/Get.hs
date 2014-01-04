module Data.Dar.Get
       (
       getDARFile, getDARIndex, getDARLookup
       ) where

import Control.Applicative
import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.List (find)
import Data.Tuple.Curry (uncurryN)
import Data.Word

import Data.Dar.Types

--
-- **************************************************
-- Functions for getting a DAR file and its DataNodes from a ByteString.

-- 
formatError (leftBs, pos, msg) = Left $ msg ++ " at " ++ show pos ++ " bytes"
                                 ++ " and " ++ (show $ BC.length leftBs)
                                 ++ " bytes left to consume."

-- Tries to gets the DataNodes from a DAR file (in ByteString from),
-- If the DAR file can't be parsed an error message is created that
-- includes the position where the error was and the number of bytes
-- remaining unparsed.
getDARFile :: BC.ByteString -> Either String [DataNode]
getDARFile bs = 
  case (runGetOrFail getDARFile' bs) of
    -- Combines all the error details in to a single message.
    Left err -> formatError err
    -- Extract the DataNodes from the DAR file.doubleListdoubleList
    Right (_, _, val) -> Right $ dataNodes . dataDAR $ val

getDARFile' :: Get DarFile
getDARFile' = do
  header <- getDARHeader
  (Dar header)
    <$> (getINDEXHeader $ nodeCount header)
    <*> (getDATAHeader $ nodeCount header)

getDARIndex :: BC.ByteString -> Either String [IndexNode]
getDARIndex bs =
  case (runGetOrFail getDARIndex' bs) of
    Left err -> formatError err
    Right (_, _, val) -> Right $ indexNodes . indexDAR $ val

getDARIndex' :: Get DarFile
getDARIndex' = do
  header <- getDARHeader
  (Dar header)
    <$> (getINDEXHeader $ nodeCount header)
    <*> (pure $ dataDAR defaultDar)

getDARLookup :: BC.ByteString -> BC.ByteString -> Either String DataNode
getDARLookup name bs = 
  case (runGetOrFail getDARIndex' bs) of
    Left err -> formatError err
    Right (_, _, iNodes) -> do
      let index = find ((==) name . identifyINode) $ indexNodes . indexDAR $ iNodes
      case index of
        Nothing -> Left "No DataNode with that name exists."
        Just iNzode -> do
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
  maV <- getWord8
  miV <- getWord8
  when (maV /= majorV header || miV /= minorV header)
    (fail "DAR file version incompatible")
    
  -- Get the number of DataNodes and the checksum.
  dataNodeNum <- getWord64le
  checksum <- getWord32le

  return $ DarH dar maV miV dataNodeNum checksum

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
