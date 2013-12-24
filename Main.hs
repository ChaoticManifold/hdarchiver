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
    "help" -> putStrLn helpText
    "test" -> B.writeFile (concat args) $ runPut $ putDARFile testDNodes
    "read" -> do

      darfile <- B.readFile $ concat args

      let decodedDar =  getDARFile darfile

      case decodedDar of
        Right _ -> putStrLn $ (concat args) ++ " is TRUE DAR file."-- \n\nList of DataNodes:"
                -- ++ (concatMap (\(DataN n d) -> unwords ["\nDataNode name:", BC.unpack n
                --                                        , "\nDataLenght:", show $ B.length d
                --                                        , "\nData:", BC.unpack d, "\n"])
                --     $ dataNodes $ dataDAR decodedDar)
        Left msg -> putStrLn $ (concat args) ++ " has an error:\n" ++ msg


testDNodes = [ DataN (BC.pack "test") (BC.pack "This is the data section.\nHello again!")
             , DataN (BC.pack "hello") (BC.pack "A hello test, whats that?")
             ]

helpText = unwords [ "Options:\n"
                   , "help : For this help text.\n"
                   , "pack <DAR file> <list of files>"
                   ,  ": packs the files into a named DAR file.\n"
                   , "unpack <DAR file> : unpack the files stored in the DAR file.\n"
                   , "dump <DAR file>"
                   , " : prints the contents on the DAR file to the screen.\n"]

