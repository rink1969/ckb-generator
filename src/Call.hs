module Call where

import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Shelly (fromText, shelly, run)
import Data.Text (pack, unpack)

call :: String -> [String] -> IO String
call c args =  do
  hSetBuffering stdout LineBuffering
  let path = fromText $ pack c
  let targs = Prelude.map pack args
  d <- shelly $ (run path targs)
  return $ unpack d
--  putStrLn $ unpack d

compile_contract :: String -> String -> IO ()
compile_contract source_path elf_path = do
  call "riscv64-unknown-elf-gcc" ["-O2", source_path, "-o", elf_path]
  call "riscv64-unknown-elf-strip" [elf_path]
  return ()