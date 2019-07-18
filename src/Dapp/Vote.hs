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
  lockOperatorUpdateCell
  --lockOperatorAnd lockOperatorMultiSigData lockOperatorBinaryVote


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
wrapMkInput hash = mkInput hash "0" "0"

sumVotes :: [Hash] -> DappInfo -> ContractInfo -> DappInfo -> Arg -> Dapp ResolvedTransaction
sumVotes hashes vote_info config_info system_info arg = do
  let inputs = map wrapMkInput hashes
  let deps = [mkDepFormContract $ dapp_contract_info vote_info, mkDepFormContract config_info]
  let output_script = Script (contract_info_code_hash $ dapp_contract_info system_info) [arg]
  let outputs = [Output "0" "0x" output_script Nothing]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  rtx <- resolveTx LockScriptBinaryVote tx
  let Just lock_func = dapp_lock_func vote_info
  return $ lock_func rtx

vote_dapp :: Dapp Hash
vote_dapp = do
  ask "Begin to run Dapp vote!\nPress Enter to continue..."
  ask "At first deploy config data (Make sure has run buildConfigData)\nPress Enter to continue..."
  config_info <- deployConfigData
  system_info <- system_script_info
  vote_info <- mkDappInfo (vote_name, Just vote_lock_script)
  ask "Begin to vote!\nEmpty data means No, otherwise Yse!\nPress Enter to continue..."
  ask "Voter1 ready to vote!\nPress Enter to continue..."
  vote1_hash <- transferCapacity system_info vote_info
  ask "Voter2 ready to vote!\nPress Enter to continue..."
  vote2_hash <- transferCapacity system_info vote_info
  ask "Voter3 ready to vote!\nPress Enter to continue..."
  vote3_hash <- transferCapacity system_info vote_info
  ask "Voter1 want to modify his vote\nPress Enter to continue..."
  new_data <- ask "input new vote data:"
  vote1_hash <- updateCell (updateOutputData new_data) vote_info vote1_hash "0"
  ask "Gather all vote tx hashes and begin to sum votes\nPress Enter to continue..."
  sum_rtx <- sumVotes [vote1_hash, vote2_hash, vote3_hash] vote_info config_info system_info admin
  let sum_tx = _resolved_transaction_tx sum_rtx
  ask "We need complete Multi-Signatures\nPress Enter to continue..."
  ask "Voter1 sign for sum_tx!\nPress Enter to continue..."
  stx1 <- system_script_lock sum_tx
  ask "Voter2 sign for sum_tx!\nPress Enter to continue..."
  stx2 <- system_script_lock sum_tx
  ask "Voter3 sign for sum_tx!\nPress Enter to continue..."
  stx3 <- system_script_lock sum_tx
  let mstx = mergeTransactions [stx1, stx2, stx3]
  sendRawTransaction mstx


-- run program write by DSL
runVote = runMaybeT (iterM clientInterpreter $ vote_dapp)