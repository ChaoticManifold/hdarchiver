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
  args <- getArgs

  putStrLn $ unwords args

  let option = head args
      darFile = head $ tail args
      otherArgs = tail $ tail args

  if args == [] || option `notElem` posOptions
    then  putStrLn "Invalid option, use 'help' for a list of options."
    else
    case option of
      "help" -> putStrLn helpText
      "pack" -> packDar darFile otherArgs
      "unpack" -> unpackDar darFile
      "dump" -> dumpDar darFile
      "list" -> listDar darFile
      "lookup" -> lookupDar darFile otherArgs -- Look up each arg?

posOptions = ["help", "pack", "unpack", "dump", "list", "lookup"]

helpText = unwords [ "Options:\n"
                   , "help : For this help text.\n"
                   , "pack <DAR file> <list of files>"
                   , ": packs the files into a named DAR file."
                   , "To pack the contents of a directory replace"
                   , "unpack <DAR file> : unpack the files stored in the DAR file.\n"
                   , "dump <DAR file>"
                   , " : prints the DataNodes of the DAR file to the screen.\n"
                   , "list <DAR file> : lists the names of the DataNodes in the DAR file"
                   , "lookup <DAR file> <DataNode Name> : if the Data Node exsits."]

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

  either
    (\msg -> hPutStrLn stderr msg)
    (\dNodes -> mapM_ (\x -> BC.writeFile (BC.unpack $ identifyDNode x)
                             $ contentDNode x) dNodes)
    (getDARFile darfile)

dumpDar :: FilePath -> IO ()
dumpDar dFile = do
  darfile <- BC.readFile dFile

  either
    (\msg -> hPutStrLn stderr msg)
    (\dNodes -> putStrLn $ dFile ++ "\n\n"
                ++ (concatMap (\d -> (BC.unpack $ identifyDNode d) ++ "\n"
                                     ++ (BC.unpack $ contentDNode d) ++ "\n\n") dNodes))
    (getDARFile darfile)

listDar :: FilePath -> IO ()
listDar dFile = do
  darfile <- BC.readFile dFile

  either
    (\msg -> hPutStrLn stderr msg)
    (\iNodes -> putStrLn (concatMap (\d -> (BC.unpack $ identifyINode d) ++
                                           (show $ nodePosition d) ++ "\n") iNodes))
    (getDARIndex darfile)

lookupDar :: FilePath -> [String] -> IO ()
lookupDar dFile names = do
  darfile <- BC.readFile dFile

  mapM_ (\name -> either 
          (\msg -> hPutStrLn stderr msg)
          (\dNode -> putStrLn $ (BC.unpack $ identifyDNode dNode) ++ "\n"
                     ++ (BC.unpack $ contentDNode dNode) ++ "\n")
          (getDARLookup (BC.pack name) darfile)) names

