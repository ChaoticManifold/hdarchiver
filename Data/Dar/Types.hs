module Data.Dar.Types
       (
       DarFile (..), DarHeader (..),
       IndexHeader (..), IndexNode (..),
       DataHeader (..), DataNode (..),
       defaultDar
       ) where

import Data.ByteString.Lazy.Char8 (pack, ByteString)
import Data.Word

data DarFile = Dar { headerDAR :: DarHeader
                   , indexDAR :: IndexHeader
                   , dataDAR :: DataHeader
                   } 

data DarHeader = DarH { identifyDAR :: ByteString    -- 3 bytes 
                      , majorV :: Word8              -- 1 byte
                      , minorV :: Word8              -- 1 byte
                      , nodeCount :: Word64          -- 8 bytes
                      , checksum :: Word32           -- 4 bytes
                      }                      -- total: 17 bytes

data IndexHeader = IndexH { identifyINDEX :: ByteString --Should be ".index" 6 bytes
                          , indexNodes :: [IndexNode]
                          } 

data IndexNode = IndexN { identifyINodeLen :: Word32  -- 4 bytes
                        , identifyINode :: ByteString -- (1 to maxBound Word32) bytes
                        , nodePosition :: Word64      -- 8 bytes ??? Int64 ???
                        } 

data DataHeader = DataH { identifyDATA :: ByteString -- Should be ".data" 5 bytes
                        , dataNodes :: [DataNode] 
                        } 

data DataNode = DataN { identifyDNodeLen :: Word32  -- 4 bytes
                      , identifyDNode :: ByteString -- (1 to maxBound Word32) bytes
                      , contentDNodeLen :: Word64   -- 8 bytes
                      , contentDNode :: ByteString  -- (1 to maxBound Word64) bytes
                      } 

 -- A standard DAR file for comparison perposes.
defaultDar = Dar 
             (DarH (pack "DAR") 6 0 0 0)
             (IndexH (pack ".index") [])
             (DataH (pack ".data") [])
