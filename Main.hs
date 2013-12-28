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
    "pack" -> packDar darFile args
    "unpack" -> unpackDar darFile
    "dump" -> dumpDar darFile
    "list" -> listDar darFile
    _ -> putStrLn "Invalid option, use 'help' for a list of options."

helpText = unwords [ "Options:\n"
                   , "help : For this help text.\n"
                   , "pack <DAR file> <list of files>"
                   , ": packs the files into a named DAR file."
                   , "To pack the contents of a directory replace"
                   , "unpack <DAR file> : unpack the files stored in the DAR file.\n"
                   , "dump <DAR file>"
                   , " : prints the DataNodes of the DAR file to the screen.\n"
                   , "list <DAR file> : lists the names of the DataNodes in the DAR file"
                   ]

packDar :: FilePath -> [FilePath] -> IO ()
packDar dFile files@(dir:_) = do
  dirExists <- (length files == 1 &&) <$> doesDirectoryExist dir
  if dirExists
    then packDar dFile =<< (map (combine dir)) <$>
         filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
    else do
    dContents <- mapM BC.readFile files

    let dNodes = map mkdNode $ zip (map (BC.pack . takeFileName) files) dContents
        mkdNode (n, d) = DataN (fromIntegral $ BC.length n) n
                         (fromIntegral $ BC.length d) d

    BC.writeFile dFile $ putDARFile dNodes

unpackDar :: FilePath -> IO ()
unpackDar dFile = do
  darfile <- BC.readFile dFile

  case getDARFile darfile of
    Right dNodes -> mapM_ (\x -> BC.writeFile (BC.unpack $ identifyDNode x)
                                 $ contentDNode x) dNodes 

    Left msg -> putStrLn $ dFile ++ " has an error:\n" ++ msg

dumpDar :: FilePath -> IO ()
dumpDar dFile = do
  darfile <- BC.readFile dFile

  case getDARFile darfile of 
    Right dNodes -> putStrLn $ dFile ++ "\n\n"
                    ++ (concatMap (\d -> (BC.unpack $ identifyDNode d) ++ " \n"
                                      ++ (BC.unpack $ contentDNode d) ++ "\n\n") dNodes)
  
    Left msg -> putStrLn $ dFile ++ " has an error:\n" ++ msg

listDar :: FilePath -> IO ()
listDar dFile = do
  darfile <- BC.readFile dFile

  case getDARIndex darfile of
    Right iNodes -> putStrLn $ dFile ++ "\n\n"
                    ++ (concatMap (\d -> (BC.unpack $ identifyINode d) ++ " \n") iNodes)

    Left msg -> putStrLn $ dFile ++ " has an error:\n" ++ msg

