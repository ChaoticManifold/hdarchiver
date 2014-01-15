module Data.Dar.Put
       (
         putDARFile, calcINDEXHeader
       ) where

import Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Word

import Data.Dar.Types

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
putINDEXHeader nodes = do
  -- let nSizeAndName = reverse . fst . foldl (\(acc,p) (i,x) -> ((i,p):acc, p+x)) ([],0)
  --                    $ zip
  --                    (map (\x -> (identifyDNodeLen x, identifyDNode x) ) nodes)
  --                    (map getsize nodes)
  --     getsize x = 4 + (fromIntegral $ identifyDNodeLen x)
  --                 + 8 + (fromIntegral $ contentDNodeLen x)
  --     -- The size (everything preceding DataNode block)
  --     -- the DAR header (17 bytes), ".index" (6 bytes), ".data" (5 bytes), 
  --     -- 
  --     -- offset (8 bytes)). Total 28 bytes + ()
  --     preSize = 28 + (sum $ map ((+) 12 . fromIntegral . identifyDNodeLen) nodes)

  -- putLazyByteString . identifyINDEX $ indexDAR defaultDar
  -- mapM_ (flip putINDEXNode preSize) nSizeAndName
  let index = calcINDEXHeader
              (map identifyDNode nodes)
              (map identifyDNodeLen nodes)
              (map contentDNodeLen nodes)

  putLazyByteString $ identifyINDEX index
  mapM_ putINDEXNode $ indexNodes index

-- Calculates the index and offsets for DataNodes
calcINDEXHeader :: [BC.ByteString] -> [Word32] -> [Word64] -> IndexHeader
calcINDEXHeader names nameLens dataSizes = IndexH
                                           (identifyINDEX $ indexDAR defaultDar)
                                           iNodes
  where --nameLens = map (fromIntegral . BC.length) names
        sizeBeforeData = 28 + (sum $ map ((+) 12 . fromIntegral) nameLens)
        offsets = reverse . fst $
                  foldr (\off (acc,prevOff) -> (prevOff:acc, prevOff + off))
                  ([], sizeBeforeData) dataSizes
        iNodes = zipWith3 IndexN nameLens names offsets

putINDEXNode :: IndexNode -> Put
putINDEXNode iNode = do
  putWord32le $ identifyINodeLen iNode
  putLazyByteString $ identifyINode iNode
  putWord64le $ nodePosition iNode

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
