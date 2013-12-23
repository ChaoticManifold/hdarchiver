-- A tool to pack and unpack .dar files, specification is found in the README.

module Main where

import Control.Applicative
import Control.Monad
import Data.Binary.Get          
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.IO

import Data.DarTypes

main = do
  (action:args) <- getArgs
  
  case action of
    "test" -> B.writeFile (concat args) $ runPut $ putDARFile testDNodes
    "read" -> do

      darfile <- B.readFile $ concat args

      let decodedDar = runGet getDARFile darfile

      case (checkDarHeader $ headerDAR decodedDar) of
        True -> putStrLn $ (concat args) ++ " is TRUE DAR file."-- \n\nList of DataNodes:"
                -- ++ (concatMap (\(DataN n d) -> unwords ["\nDataNode name:", BC.unpack n
                --                                        , "\nDataLenght:", show $ B.length d
                --                                        , "\nData:", BC.unpack d, "\n"])
                --     $ dataNodes $ dataDAR decodedDar)
        False -> putStrLn $ (concat args) ++ " is FALSE DAR file,"-- .data block possibly corrupt."


testDNodes = [ DataN (BC.pack "test") (BC.pack "This is the data section.\nHello again!")
             , DataN (BC.pack "hello") (BC.pack "A hello test, whats that?")
             ]



