module Dapp.AlwaysSuccess where

import Type
import EDSL
import LockScript
import Dapp.Util
import Dapp.SystemScript
import Call

import System.Directory (getCurrentDirectory)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Free (iterM)
import Control.Monad.State (execState)

-- contract always_success
always_success_lock_script :: LockScript ()
always_success_lock_script = do
  nop

-- lock script runner
always_success_lock_script_func :: ResolvedTransaction -> ResolvedTransaction
always_success_lock_script_func init_rtx = execState (iterM lockScriptInterpreter $ always_success_lock_script) init_rtx

-- contract runner
always_success_lock_script_contract :: IO ()
always_success_lock_script_contract = iterM contractInterpreter $ always_success_lock_script

always_success :: String -> Dapp ContractInfo
always_success elf_path = do
  userinfo <- userInfo
  deployContract (userinfo, elf_path)

always_success_lock :: Transaction -> Dapp Transaction
always_success_lock tx = do
  let rtx = ResolvedTransaction tx [] []
  let new_rtx = always_success_lock_script_func rtx
  return $ _resolved_transaction_tx new_rtx


-- example 1: move capacity from system script to new contract
moveCapacityToContract :: ContractInfo -> Dapp Hash
moveCapacityToContract contractInfo = do
  user_info <- userInfo
  system_script_info <- system_script
  cap <- capacity
  ret <- query system_script_info user_info cap
  let input_capacity_s = ret_queryLiveCells_capacity ret
  let input_capacity = read input_capacity_s :: Int
  let inputs = ret_queryLiveCells_inputs ret
  let script = Script (contract_info_code_hash contractInfo) [userInfo_blake160 user_info]
  let lock_output = Output (show cap) "0x" script Nothing
  let outputs = [lock_output]
  let charge = input_capacity - cap
  let charge_script = Script (contract_info_code_hash system_script_info) [userInfo_blake160 user_info]
  let charge_output = Output (show charge) "0x" charge_script Nothing
  let outputs = if charge /= 0 then [lock_output, charge_output] else [lock_output]
  let dep = mkDepFormContract system_script_info
  let deps = [dep]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  sendTransaction (user_info, tx)

callContract :: ContractInfo -> Hash -> Dapp Hash
callContract contractInfo preHash = do
  c <- getLiveCellByTxHashIndex (preHash, "0")
  let output = cell_with_status_cell c
  let input = mkInput preHash "0" "0"
  let dep = mkDepFormContract contractInfo
  let deps = [dep]
  let inputs = [input]
  let outputs = [output]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  stx <- always_success_lock tx
  sendRawTransaction stx

-- runner
generate_and_compile_always_success :: IO ()
generate_and_compile_always_success = do
  always_success_lock_script_contract
  pwd <- getCurrentDirectory
  let elf_path = pwd <> "/contract/build/always_success"
  putStrLn elf_path
  compile_contract generated_contract_path elf_path

-- run program write by DSL
runDeploy elf_path = runMaybeT (iterM clientInterpreter $ always_success elf_path)
runSetup info = runMaybeT (iterM clientInterpreter $ (moveCapacityToContract info))
runCall info hash = runMaybeT (iterM clientInterpreter $ (callContract info hash))
