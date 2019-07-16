module Call where

import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import Shelly (fromText, shelly, run)
import Data.Text (pack, unpack)
import System.Directory (getCurrentDirectory)

call :: String -> [String] -> IO String
call c args =  do
  hSetBuffering stdout LineBuffering
  let path = fromText $ pack c
  let targs = Prelude.map pack args
  d <- shelly $ (run path targs)
  return $ unpack d
--  putStrLn $ unpack d

compile_contract :: String -> IO ()
compile_contract target = do
  pwd <- getCurrentDirectory
  elf_path <- elf_abs_path target
  source_path <- source_abs_path target
  call "riscv64-unknown-elf-gcc" ["-O2", "-mcmodel=medlow", "-DSECP256K1_CUSTOM_FUNCS", "-I", pwd <> "/contract/" <> "deps/flatcc/include", "-I", pwd <> "/contract/" <> "deps/secp256k1/src", "-I", pwd <> "/contract/" <> "deps/secp256k1", "-I", pwd <> "/contract/" <> "header", "-Wall", "-Werror", "-Wno-nonnull-compare", "-Wl,-static", "-fdata-sections", "-ffunction-sections", "-Wl,--gc-sections", "-Wl,-s", "-o", elf_path, source_path]
  call "riscv64-unknown-elf-strip" [elf_path]
  return ()


source_abs_path :: String -> IO String
source_abs_path name = do
  pwd <- getCurrentDirectory
  return $ pwd <> "/contract/src/" <> name <> ".c"

elf_abs_path :: String -> IO String
elf_abs_path name = do
  pwd <- getCurrentDirectory
  return $ pwd <> "/contract/build/" <> name