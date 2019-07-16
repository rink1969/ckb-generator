module Dapp.SystemScript where

import EDSL
import Type

import Control.Lens (set)

-- system script
system_script_info :: Dapp DappInfo
system_script_info = do
  mkDappInfo ("system", Nothing)

system_script_lock :: UserInfo -> Transaction -> Dapp Transaction
system_script_lock user_info tx = do
  witnesses <- sign (user_info, tx)
  let stx = set transaction_witnesses witnesses tx
  return stx