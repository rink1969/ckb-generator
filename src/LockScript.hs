{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module LockScript where

import Type

import Language.C99.Simple

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

import Control.Monad.State (State, state, modify)

import Data.Word (Word8)
import Data.HexString (hexString, toBytes, fromBinary, toText)
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- define of DSL
data LockOperator tx next =
  Nop next
  deriving (Functor)

makeFree ''LockOperator

type ScriptOperator = LockOperator ResolvedTransaction

type LockScript = Free ScriptOperator


-- util functions for interpreter
data MultiSigConfig = MultiSigConfig
  { multi_sig_total :: Word8
  , multi_sig_threshold :: Word8
  , multi_sig_args :: [[Word8]]
  }
-- arg is hex string without 0x prefix
mkMultiSigConfig :: Name -> Int -> Int -> [String] -> IO ()
mkMultiSigConfig path total threshold args =  BS.writeFile path bs where
  wtotal = fromIntegral total
  wthreshold = fromIntegral threshold
  wargs = map (BS.unpack . toBytes . hexString . CB.pack) args
  config = MultiSigConfig wtotal wthreshold wargs
  bs = BS.pack ([multi_sig_total config] <> [multi_sig_threshold config] <> (concat $ multi_sig_args config))

-- lock script interpreter: translate DSL code to lock process
-- type of eval result
type LockST = State ResolvedTransaction

lockInterpreter :: ScriptOperator (LockST next) -> LockST next
lockInterpreter (Nop next) = do
  modify id
  next


-- contract interpreter: translate DSL code to c code
-- type of eval result
type ContractST = State [Stmt]

contractInterpreter :: ScriptOperator (ContractST next) -> ContractST next
contractInterpreter (Nop next) = do
  modify ((Return $ Just (LitInt 0)) :)
  next
