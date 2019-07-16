module Dapp.Vote where

import Type
import EDSL
import LockScript
import Dapp.Util
import Dapp.SystemScript
import Call

import Data.Word (Word8)
import Data.HexString (hexString, toBytes, fromBinary, toText)
import qualified Data.ByteString.Char8 as CB
import qualified Data.ByteString as BS
import Control.Lens (set)
import qualified Data.Text as T

import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Free (iterM)
import Control.Monad.State (execState)


-- contract vote
vote_lock_script :: LockScript ()
vote_lock_script = do
  nop

-- lock script runner
vote_lock_script_func :: ResolvedTransaction -> ResolvedTransaction
vote_lock_script_func init_rtx = execState (iterM lockInterpreter $ vote_lock_script) init_rtx

-- contract runner
vote_lock_script_contract :: IO ()
vote_lock_script_contract = do
  let stmts = execState (iterM contractInterpreter $ vote_lock_script) []
  let code = genCode stmts
  path <- source_abs_path "vote"
  writeFile path code

-- vote config for mutil-signatures
data VoteConfig = VoteConfig
  { total :: Word8
  , threshold :: Word8
  , args :: [[Word8]]
  }

-- arg is hex string without 0x prefix
mkVoteConfig :: Int -> Int -> [String] -> VoteConfig
mkVoteConfig total threshold args =  VoteConfig wtotal wthreshold wargs where
  wtotal = fromIntegral total
  wthreshold = fromIntegral threshold
  wargs = map (BS.unpack . toBytes . hexString . CB.pack) args

writeVoteConfig :: VoteConfig -> IO ()
writeVoteConfig vote_config = BS.writeFile voteConfigDataPath bs where
  bs = BS.pack ([total vote_config] <> [threshold vote_config] <> (concat $ args vote_config))
  voteConfigDataPath = elf_abs_path "vote_config_data"

-- deploy config data
deployConfigData :: Dapp ContractInfo
deployConfigData = do
  userinfo <- userInfo
  voteConfigDataPath <- elfAbsPath "vote_config_data"
  deployContract (userinfo, voteConfigDataPath)

deployVoteContract :: Dapp ContractInfo
deployVoteContract = do
  userinfo <- userInfo
  path <- elfAbsPath "vote"
  deployContract (userinfo, path)


-- vote data is null means no, otherwise means yes
vote :: Data -> ContractInfo -> Dapp Hash
vote vote_data contractInfo = do
  user_info <- userInfo
  system_script_info <- system_script
  cap <- capacity
  ret <- query system_script_info user_info cap
  let input_capacity_s = ret_queryLiveCells_capacity ret
  let input_capacity = read input_capacity_s :: Int
  let inputs = ret_queryLiveCells_inputs ret
  let script = Script (contract_info_code_hash contractInfo) [userInfo_blake160 user_info]
  let vote_output = Output (show cap) vote_data script Nothing
  let outputs = [vote_output]
  let charge = input_capacity - cap
  let charge_script = Script (contract_info_code_hash system_script_info) [userInfo_blake160 user_info]
  let charge_output = Output (show charge) "0x" charge_script Nothing
  let outputs = if charge /= 0 then [vote_output, charge_output] else [vote_output]
  let dep = mkDepFormContract system_script_info
  let deps = [dep]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  sendTransaction (user_info, tx)

reVote :: Data -> ContractInfo -> Hash -> Dapp Hash
reVote vote_data contractInfo preHash = do
  user_info <- userInfo
  c <- getLiveCellByTxHashIndex (preHash, "0")
  let output = cell_with_status_cell c
  let new_output = set output_data vote_data output
  let input = mkInput preHash "0" "0"
  let dep = mkDepFormContract contractInfo
  let deps = [dep]
  let inputs = [input]
  let outputs = [new_output]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  sendTransaction (user_info, tx)

wrapMkInput :: Hash -> Input
wrapMkInput hash = mkInput hash "0" "0"

sumVots :: [Hash] -> ContractInfo -> ContractInfo -> Arg -> Dapp Transaction
sumVots hashes contract_info config_info arg = do
  let inputs = map wrapMkInput hashes
  let deps = [mkDepFormContract contract_info, mkDepFormContract config_info]
  user_info <- userInfo
  cs <- mapM getLiveCellByTxHashIndex (zip hashes (repeat "0"))
  let data_lens = map (length . _output_data. cell_with_status_cell) cs
  let total = length data_lens
  let yes = length $ filter (> 2) data_lens
  let output_data = T.unpack $ toText $ fromBinary ((fromIntegral total) :: Word8, (fromIntegral yes) :: Word8)
  let output_cap = foldl (+) 0 (map ((read :: String -> Int) . _output_capacity. cell_with_status_cell) cs)
  system_script_info <- system_script
  let output_script = Script (contract_info_code_hash system_script_info) [arg]
  let outputs = [Output (show output_cap) ("0x" <> output_data) output_script Nothing]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  system_script_lock user_info tx

merge :: [Transaction] -> Dapp Hash
merge txs = do
  let all_witnesses = map _transaction_witnesses txs
  let witness = mergeWitnesses all_witnesses
  let tx = head txs
  let stx = set transaction_witnesses witness tx
  sendRawTransaction stx

-- run program write by DSL
runDeployConfigData = runMaybeT (iterM clientInterpreter $ deployConfigData)
runDeployVoteContract = runMaybeT (iterM clientInterpreter $ deployVoteContract)
runVote vote_data contract_info  = runMaybeT (iterM clientInterpreter $ (vote vote_data contract_info))
runReVote vote_data contract_info hash = runMaybeT (iterM clientInterpreter $ (reVote vote_data contract_info hash))
runSumVots hashes contract_info config_info arg = runMaybeT (iterM clientInterpreter $ (sumVots hashes contract_info config_info arg))
runMerge txs = runMaybeT (iterM clientInterpreter $ (merge txs))