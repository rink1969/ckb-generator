module Call where

import System.IO
import Shelly
import Data.Text

call :: String -> [String] -> IO String
call c args =  do
  hSetBuffering stdout LineBuffering
  let path = fromText $ pack c
  let targs = Prelude.map pack args
  d <- shelly $ (run path targs)
  return $ unpack d
--  putStrLn $ unpack d