-- A tool to pack and unpack .dar files, specification is found in the README.

import Control.Applicative
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as B
import System.Environment

main = do
  args <- getArgs
  dar <- B.readFile $ head args
--  putStrLn $ B.unpack dar
  let testDar = checkIsDar $ B.take 3 dar
  if testDar
    then putStrLn "Is Dar file."
    else putStrLn "No Dar file."

--checkIsDar :: B.ByteString
checkIsDar bs = bs == (B.pack "DAR")
