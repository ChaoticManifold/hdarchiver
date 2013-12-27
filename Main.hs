-- A tool to pack and unpack .dar files, specification is found in the README.

module Main where

import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BC
import System.Environment
import System.IO
import System.Directory
import System.FilePath

import Data.DarTypes

main = do
  (option:darFile:args) <- getArgs
  
  case option of
    "help" -> putStrLn helpText
    "test" -> BC.writeFile (concat args) $ putDARFile testDNodes
    "pack" -> packDar darFile args
    "unpack" -> unpackDar darFile
    "dump" -> dumpDar darFile
    _ -> putStrLn "Invalid option, use 'help' for a list of options."

testDNodes = [ DataN (BC.pack "test") (BC.pack "This is the data section.\nHello again!")
             , DataN (BC.pack "hello") (BC.pack "A hello test, whats that?")
             ]

helpText = unwords [ "Options:\n"
                   , "help : For this help text.\n"
                   , "pack <DAR file> <list of files>"
                   , ": packs the files into a named DAR file."
                   , "To pack the contents of a directory replace"
                   , "unpack <DAR file> : unpack the files stored in the DAR file.\n"
                   , "dump <DAR file>"
                   , " : prints the DataNodes of the DAR file to the screen.\n"]

dumpDar :: FilePath -> IO ()
dumpDar dFile = do
  darfile <- BC.readFile dFile

  case getDARFile darfile of 
    Right dNodes -> putStrLn $ dFile ++ " is TRUE DAR file.\nContains:\n\n"
                    ++ (concatMap (\d -> (BC.unpack $ identifyDNode d) ++ " \n"
                                      ++ (BC.unpack $ contentDNode d) ++ "\n\n") dNodes)
  
    Left msg -> putStrLn $ dFile ++ " has an error:\n" ++ msg

packDar :: FilePath -> [FilePath] -> IO ()
packDar dFile files@(d:_) = do
  dirExists <- (length files == 1 &&) <$> doesDirectoryExist d
  if dirExists
    then do
--    putStrLn "Getting files from directory."
    packDar dFile =<< (map (combine d)) <$>
      filter (`notElem` [".", ".."]) <$> getDirectoryContents d
    else do
--    putStrLn $ "Packing '" ++ (unwords files) ++ "' into " ++  dFile
    dContents <- mapM BC.readFile files
    let dNodes = map (\(n,d) -> DataN (BC.pack n) d) $ zip (map takeFileName files) dContents
    BC.writeFile dFile $ putDARFile dNodes

unpackDar :: FilePath -> IO ()
unpackDar dFile = do
  darfile <- BC.readFile dFile

  case getDARFile darfile of
    Right dNodes -> mapM_ (\x -> BC.writeFile (BC.unpack $ identifyDNode x) $ contentDNode x) dNodes 

    Left msg -> putStrLn $ dFile ++ " has an error:\n" ++ msg
