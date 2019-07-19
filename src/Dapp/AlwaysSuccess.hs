module Dapp.AlwaysSuccess where

import Type
import EDSL
import LockScript
import Dapp.Util
import Call
import CodeGen

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Free (iterM)

-- contract always_success
always_success_name = "always_success"

always_success_lock_script :: LockScript ()
always_success_lock_script = do
  lockOperatorNop

-- dapp always_success
loop :: DappInfo -> Hash -> Dapp Hash
loop info prehash = do
  prehash <- updateCell id info prehash "0"
  s <- ask "Loop call contract always_sucess!\nPress Enter to continue...Input \"e\" to exit loop..."
  case s of
    "" -> loop info prehash
    "e" -> return prehash

always_success_dapp :: Dapp Hash
always_success_dapp = do
  display "Begin to run Dapp always_sucess!"
  display "Deploy contract always_success!"
  always_success_info <- mkDappInfo (always_success_name, Just always_success_lock_script)
  system_info <- system_script_info
  display "Move some capacity from system_script to contract always_sucess!"
  prehash <- transferCapacity system_info always_success_info
  display "Start loop call contract always_sucess!"
  loop always_success_info prehash

-- run dapp write by DSL
runAlwaysSuccess = runMaybeT (iterM clientInterpreter $ always_success_dapp)
