module Dapp.SystemScript where

import EDSL
import Type
import Dapp.Util

import Control.Lens (set)

-- system script
system_script_info :: Dapp DappInfo
system_script_info = do
  mkDappInfo ("system", Nothing)

system_script_lock :: Transaction -> Dapp Transaction
system_script_lock tx = do
  user_info <- userInfo
  witnesses <- sign (user_info, tx)
  let stx = set transaction_witnesses witnesses tx
  return stx