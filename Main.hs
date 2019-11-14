
import System.Environment
import System.IO
import Data.Text.Lazy.IO

import Text.Pretty.Simple (pPrint)

import ParseUtils
import Parser
import Eval



main = do
  handle <- openFile "test.mc" ReadMode  
  contents <- Data.Text.Lazy.IO.hGetContents handle  
  let par = run parseModule contents
--  pPrint par
  let (Right mod)  = par 
  ret <- evalModule mod
  pPrint ret
  hClose handle  

 
