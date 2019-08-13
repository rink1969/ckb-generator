module Dapp.Util where

import Type
import EDSL

import Control.Lens (set)

-- util code for DSL
userInfo :: Dapp UserInfo
userInfo = do
  key <- ask "privkey"
  getUserInfo key

hdUserInfo :: Dapp UserInfo
hdUserInfo = do
  index <- ask "index"
  getHDUserInfo (read index :: Int)

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

resolveTx :: LockScriptCmd -> Transaction -> Dapp ResolvedTransaction
resolveTx cmd tx = do
  let deps = _transaction_deps tx
  resolved_deps <- mapM (getLiveCellByTxHashIndex . outPoint2Tuple) deps
  let inputs = _transaction_inputs tx
  resolved_inputs <- mapM (getLiveCellByTxHashIndex . outPoint2Tuple . input_previous_output) inputs
  return $ ResolvedTransaction tx resolved_deps resolved_inputs cmd

gatherArgs :: [Hash] -> Dapp [Hash]
gatherArgs init_args = do
  s <- ask "Please input Arg or Input \"e\" to end of input"
  case s of
    "e" -> return init_args
    arg -> gatherArgs (init_args ++ [arg])


transferCapacity :: DappInfo -> DappInfo -> Dapp Hash
transferCapacity from to = do
  key <- ask "Please input sender user privkey:"
  from_user_info <- getUserInfo key
  args <- gatherArgs []
  cap <- capacity
  output_data <- ask "data in output"
  ret <- query from from_user_info cap
  let input_capacity_s = ret_queryLiveCells_capacity ret
  let input_capacity = read input_capacity_s :: Int
  let inputs = ret_queryLiveCells_inputs ret
  let script = Script (contract_info_code_hash $ dapp_contract_info to) args "Data"
  let lock_output = Output (show cap) ("0x" <> output_data) script Nothing Nothing
  let charge = input_capacity - cap
  let charge_script = Script (contract_info_code_hash $ dapp_contract_info from) [userInfo_blake160 from_user_info] "Data"
  let charge_output = Output (show charge) "0x" charge_script Nothing Nothing
  let outputs = if charge /= 0 then [lock_output, charge_output] else [lock_output]
  let dep = mkDepFormContract $ dapp_contract_info from
  let deps = [dep]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  let maybe_lock_func = dapp_lock_func from
  case maybe_lock_func of
    Nothing -> sendTransaction (from_user_info, tx)
    Just lock_func -> do
      init_rtx <- resolveTx LockScriptUpdateCell tx
      let new_rtx = lock_func init_rtx
      case (_resolved_transaction_func new_rtx) of
        LockScriptSystemLock -> do
          tx <- system_script_lock $ _resolved_transaction_tx new_rtx
          sendRawTransaction tx
        _ -> sendRawTransaction $ _resolved_transaction_tx new_rtx

updateCell :: (Output -> Output) ->  DappInfo-> Hash -> Index -> Dapp Hash
updateCell func info hash index = do
  c <- getLiveCellByTxHashIndex (hash, index)
  let output = cell_with_status_cell c
  let new_output = func output
  let input = mkInput hash index "0"
  let dep = mkDepFormContract $ dapp_contract_info info
  let deps = [dep]
  let inputs = [input]
  let outputs = [new_output]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  let maybe_lock_func = dapp_lock_func info
  case maybe_lock_func of
    Nothing -> do
      stx <- system_script_lock tx
      sendRawTransaction stx
    Just lock_func -> do
      init_rtx <- resolveTx LockScriptUpdateCell tx
      let new_rtx = lock_func init_rtx
      case (_resolved_transaction_func new_rtx) of
        LockScriptSystemLock -> do
          tx <- system_script_lock $ _resolved_transaction_tx new_rtx
          sendRawTransaction tx
        _ -> sendRawTransaction $ _resolved_transaction_tx new_rtx


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