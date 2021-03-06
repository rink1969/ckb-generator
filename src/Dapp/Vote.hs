module Dapp.Vote where

import Type
import EDSL
import LockScript
import Dapp.Util
import Call

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Free (iterM)
import Control.Monad.State (execState)

import Control.Lens (set)


-- contract vote
vote_name = "vote"

vote_lock_script :: LockScript ()
vote_lock_script = do
  lockOperatorMultiSigData
  lockOperatorAnd
  lockOperatorBinaryVote
  lockOperatorUpdateCell

-- vote config for mutil-signatures
voter1 = "4a88cef22e4e71c48c40da51c1d6bd16daa97aa7"
voter2 = "a47f8029997fcc67aff87384daac404f39e31ceb"
voter3 = "96f4093cf179aaa369379402d74f70090fae11ec"
admin = "0x4a88cef22e4e71c48c40da51c1d6bd16daa97aa7"

config_data_path = "/tmp/vote_config_data"

buildConfigData :: IO ()
buildConfigData = mkMultiSigConfig config_data_path 3 2 [voter1, voter2, voter3]

deployConfigData :: Dapp ContractInfo
deployConfigData = do
  userinfo <- userInfo
  deployContract (userinfo, config_data_path)

-- dapp vote
wrapMkInput :: Hash -> Input
wrapMkInput hash = mkInput hash "0x0" "0x0"

sumVotes :: [Hash] -> DappInfo -> ContractInfo -> DappInfo -> Arg -> Dapp ResolvedTransaction
sumVotes hashes vote_info config_info system_info arg = do
  let inputs = map wrapMkInput hashes
  system_info <- system_script_info
  let system_dep = mkDepFormContract $ dapp_contract_info system_info
  let deps = [mkDepFormContract $ dapp_contract_info vote_info, mkDepFormContract config_info, system_dep]
  let output_script = Script (contract_info_code_hash $ dapp_contract_info system_info) arg (contract_info_hash_type $ dapp_contract_info system_info)
  let outputs = [Output "0" output_script Nothing]
  let outputs_data = ["0x"]
  let tx = Transaction "0x" "0x0" deps [] inputs outputs outputs_data (fake_witness $ length inputs)
  rtx <- resolveTx LockScriptBinaryVote tx
  let Just lock_func = dapp_lock_func vote_info
  return $ lock_func rtx

vote_dapp :: Dapp Hash
vote_dapp = do
  display "Begin to run Dapp vote!"
  display "Deploy config data (Make sure has run buildConfigData)"
  config_info <- deployConfigData
  system_info <- system_script_info
  display "Deploy contract vote"
  vote_info <- mkDappInfo (vote_name, Just vote_lock_script)
  display "Begin to vote!\nEmpty data means No, otherwise Yes!"
  display "Voter1 ready to vote!"
  vote1_hash <- transferCapacity system_info vote_info
  display "Voter2 ready to vote!"
  vote2_hash <- transferCapacity system_info vote_info
  display "Voter3 ready to vote!"
  vote3_hash <- transferCapacity system_info vote_info
  display "Voter1 want to modify his vote!"
  new_data <- ask "input new vote data:"
  vote1_hash <- updateCell (\x -> ("0x" <> new_data)) vote_info vote1_hash "0x0"
  display "Gather all vote tx hashes and begin to sum votes!"
  sum_rtx <- sumVotes [vote1_hash, vote2_hash, vote3_hash] vote_info config_info system_info admin
  let sum_tx = _resolved_transaction_tx sum_rtx
  display "We need complete Multi-Signatures!"
  display "Voter1 sign for sum_tx!"
  stx1 <- system_script_lock sum_tx
  display "Voter2 sign for sum_tx!"
  stx2 <- system_script_lock sum_tx
  display "Voter3 sign for sum_tx!"
  stx3 <- system_script_lock sum_tx
  let mstx = mergeTransactions [stx1, stx2, stx3]
  sendRawTransaction mstx


-- run program write by DSL
runVote = runMaybeT (iterM clientInterpreter $ vote_dapp)