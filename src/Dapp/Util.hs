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

query :: ContractInfo -> UserInfo -> Int -> Dapp RetQueryLiveCells
query contract_info user_info cap = do
  let code_hash = contract_info_code_hash contract_info
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
  return $ ResolvedTransaction tx resolved_deps resolved_inputs