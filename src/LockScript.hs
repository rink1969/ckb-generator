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

import Control.Lens (set)

-- define of DSL
data LockOperator tx next =
  LockOperatorNop next
  | LockOperatorUpdateCell next
  | LockOperatorBinaryVote next
  | LockOperatorMultiSigData next
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

processBinaryVote :: ResolvedTransaction -> ResolvedTransaction
processBinaryVote (ResolvedTransaction tx deps inputs LockScriptBinaryVote) = ResolvedTransaction new_tx [] [] LockScriptMultiSig where
  data_lens = map (length . _output_data. cell_with_status_cell) inputs
  total = length data_lens
  yes = length $ filter (> 2) data_lens
  output_data = T.unpack $ toText $ fromBinary ((fromIntegral total) :: Word8, (fromIntegral yes) :: Word8)
  output_cap = foldl (+) 0 (map ((read :: String -> Int) . _output_capacity. cell_with_status_cell) inputs)
  output_script = fetchOldOutputScript tx
  new_output = Output (show output_cap) ("0x" <> output_data) output_script Nothing
  new_tx = set transaction_outputs [new_output] tx

processUpdateCell :: ResolvedTransaction -> ResolvedTransaction
processUpdateCell (ResolvedTransaction tx _ _ LockScriptUpdateCell) = ResolvedTransaction tx [] [] LockScriptSystemLock

processMultiSigData :: ResolvedTransaction -> ResolvedTransaction
processMultiSigData (ResolvedTransaction tx _ _ LockScriptMultiSigData) = ResolvedTransaction tx [] [] LockScriptMultiSig

-- lock script interpreter: translate DSL code to lock process
-- type of eval result
type LockST = State ResolvedTransaction

lockInterpreter :: ScriptOperator (LockST next) -> LockST next
lockInterpreter (LockOperatorNop next) = do
  modify id
  next
lockInterpreter (LockOperatorUpdateCell next) = do
  modify processUpdateCell
  next
lockInterpreter (LockOperatorBinaryVote next) = do
  modify processBinaryVote
  next
lockInterpreter (LockOperatorMultiSigData next) = do
  modify processMultiSigData
  next


-- contract interpreter: translate DSL code to c code
-- type of eval result
type ContractST = State [Stmt]

contractInterpreter :: ScriptOperator (ContractST next) -> ContractST next
contractInterpreter (LockOperatorNop next) = do
  next
contractInterpreter (LockOperatorUpdateCell next) = do
  modify ((Expr $ AssignOp Assign (Ident "ret") (BinaryOp LOr (Ident "ret") (Funcall (Ident "verify_sighash_all") [Index (Ident "argv") (LitInt 0), LitInt 0]))) :)
  next
contractInterpreter (LockOperatorBinaryVote next) = do
  modify ((Expr $ AssignOp Assign (Ident "ret") (BinaryOp LOr (Ident "ret") (Funcall (Ident "verify_binary_vote") []))) :)
  next
contractInterpreter (LockOperatorMultiSigData next) = do
  modify ((Expr $ AssignOp Assign (Ident "ret") (BinaryOp LOr (Ident "ret") (Funcall (Ident "mdsc_run") []))) :)
  next