module Dapp.Util where

import Type
import EDSL

-- util code for DSL
userInfo :: Dapp UserInfo
userInfo = do
  key <- ask "privkey"
  getUserInfo key

capacity :: Dapp Int
capacity = do
  scap <- ask "input capacity"
  let cap = read scap :: Int
  return cap

query :: DappInfo -> UserInfo -> Int -> Dapp RetQueryLiveCells
query dapp_info user_info cap = do
  let code_hash = contract_info_code_hash $ dapp_contract_info dapp_info
  let args = [userInfo_blake160 user_info]
  lock_hash <- lockHash (code_hash, args)
  queryLiveCells (lock_hash, cap)

deploy :: Dapp ContractInfo
deploy = do
  userinfo <- userInfo
  path <- ask "contract path"
  deployContract (userinfo, path)

resolveTx :: Transaction -> Dapp ResolvedTransaction
resolveTx tx = do
  let deps = _transaction_deps tx
  resolved_deps <- mapM (getLiveCellByTxHashIndex . outPoint2Tuple) deps
  let inputs = _transaction_inputs tx
  resolved_inputs <- mapM (getLiveCellByTxHashIndex . outPoint2Tuple . input_previous_output) inputs
  return $ ResolvedTransaction tx resolved_deps resolved_inputs ""


transferCapacity :: DappInfo -> DappInfo -> Dapp Hash
transferCapacity from to = do
  key <- ask "Please input sender user privkey:"
  from_user_info <- getUserInfo key
  to_blake160 <- ask "Please input receiver blake160:"
  cap <- capacity
  output_data <- ask "data in output"
  ret <- query from from_user_info cap
  let input_capacity_s = ret_queryLiveCells_capacity ret
  let input_capacity = read input_capacity_s :: Int
  let inputs = ret_queryLiveCells_inputs ret
  let script = Script (contract_info_code_hash $ dapp_contract_info to) [to_blake160]
  let lock_output = Output (show cap) ("0x" <> output_data) script Nothing
  let outputs = [lock_output]
  let charge = input_capacity - cap
  let charge_script = Script (contract_info_code_hash $ dapp_contract_info from) [userInfo_blake160 from_user_info]
  let charge_output = Output (show charge) "0x" charge_script Nothing
  let outputs = if charge /= 0 then [lock_output, charge_output] else [lock_output]
  let dep = mkDepFormContract $ dapp_contract_info from
  let deps = [dep]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  let maybe_lock_func = dapp_lock_func from
  case maybe_lock_func of
    Nothing -> sendTransaction (from_user_info, tx)
    Just lock_func -> do
      init_rtx <- resolveTx tx
      let new_rtx = lock_func init_rtx
      sendRawTransaction $ _resolved_transaction_tx new_rtx



