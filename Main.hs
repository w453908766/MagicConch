
import System.Environment
import System.IO
import Data.Text.Lazy.IO

import Text.Pretty.Simple (pPrint)

import Parser

import Helper


main = do
  handle <- openFile "test.mc" ReadMode  
  contents <- Data.Text.Lazy.IO.hGetContents handle  
  pPrint $ run parseModule contents  
  hClose handle  

 
