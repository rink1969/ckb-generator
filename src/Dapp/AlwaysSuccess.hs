module Dapp.AlwaysSuccess where

import Type
import EDSL
import LockScript
import Dapp.Util
import Dapp.SystemScript
import Call
import CodeGen

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Free (iterM)

-- contract always_success
always_success_name = "always_success"

always_success_lock_script :: LockScript ()
always_success_lock_script = do
  nop

loop :: DappInfo -> Hash -> Dapp Hash
loop info preHash = do
  c <- getLiveCellByTxHashIndex (preHash, "0")
  let output = cell_with_status_cell c
  let input = mkInput preHash "0" "0"
  let dep = mkDepFormContract $ dapp_contract_info info
  let deps = [dep]
  let inputs = [input]
  let outputs = [output]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  let Just lock_func = dapp_lock_func info
  init_rtx <- resolveTx tx
  let new_rtx = lock_func init_rtx
  sendRawTransaction $ _resolved_transaction_tx new_rtx

wrapLoop :: DappInfo -> Hash -> Dapp Hash
wrapLoop info prehash = do
  prehash <- loop info prehash
  s <- ask "Loop call contract always_sucess!\nPress Enter to continue...Input \"e\" to exit loop..."
  case s of
    "" -> wrapLoop info prehash
    "e" -> return prehash

always_success_dapp :: Dapp Hash
always_success_dapp = do
  ask "Begin to run Dapp always_sucess!\nPress Enter to continue..."
  system_info <- system_script_info
  always_success_info <- mkDappInfo (always_success_name, Just always_success_lock_script)
  ask "Move some capacity from system_script to contract always_sucess!\nPress Enter to continue..."
  prehash <- transferCapacity system_info always_success_info
  ask "Start loop call contract always_sucess!\nPress Enter to continue..."
  wrapLoop always_success_info prehash

-- run dapp write by DSL
runAlwaysSuccess = runMaybeT (iterM clientInterpreter $ always_success_dapp)
