module Dapp.SystemScript where

import EDSL
import Type

import Control.Lens (set)

-- system script
system_script :: Dapp ContractInfo
system_script = do
  systemScript

system_script_lock :: UserInfo -> Transaction -> Dapp Transaction
system_script_lock user_info tx = do
  witnesses <- sign (user_info, tx)
  let stx = set transaction_witnesses witnesses tx
  return stx