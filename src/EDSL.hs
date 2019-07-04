{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module EDSL where

import Type
import Call

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

import Data.ByteString.Lazy (pack, unpack)
import Data.ByteString.Internal (c2w, w2c)

import Data.Aeson (encode, decode, FromJSON, ToJSON)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Control.Monad.IO.Class (liftIO)

import System.Directory (getCurrentDirectory)

import Control.Lens (set)

-- define of DSL
data Operator next =
  GetUserInfo Key (UserInfo -> next)
  | LockHash (Hash, [Arg]) (Hash -> next)
  | QueryLiveCells (Hash, Int) (RetQueryLiveCells -> next)
  | GetLiveCellByTxHashIndex (Hash, Index) (CellWithStatus -> next)
  | DeployContract (UserInfo, Path) (ContractInfo -> next)
  | Sign (UserInfo, Transaction) ([Witness] -> next)
  | SystemScript (ContractInfo -> next)
  | SendTransaction (UserInfo, Transaction) (Hash -> next)
  | SendRawTransaction Transaction (Hash -> next)
  | Ask String (String -> next)
  deriving (Functor)

makeFree ''Operator

type DappOperator = Operator

type Dapp = Free DappOperator

-- example of DSL for test

test :: Dapp String
test = do
  return "xxx"

-- contract always_success
always_success :: Dapp ContractInfo
always_success = do
  userinfo <- userInfo
  deployContract (userinfo, "/home/rink/work/github/edsl/contract/build/always_success")

always_success_lock :: Transaction -> Dapp Transaction
always_success_lock tx = return tx

-- system script
system_script :: Dapp ContractInfo
system_script = do
  systemScript

system_script_lock :: UserInfo -> Transaction -> Dapp Transaction
system_script_lock user_info tx = do
  witnesses <- sign (user_info, tx)
  let stx = set transaction_witnesses witnesses tx
  return stx


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

-- example 1: move capacity from system script to new contract
moveCapacityToContract :: ContractInfo -> Dapp Hash
moveCapacityToContract contractInfo = do
  user_info <- userInfo
  system_script_info <- system_script
  cap <- capacity
  ret <- query system_script_info user_info cap
  let input_capacity_s = ret_queryLiveCells_capacity ret
  let input_capacity = read input_capacity_s :: Int
  let inputs = ret_queryLiveCells_inputs ret
  let script = Script (contract_info_code_hash contractInfo) [userInfo_blake160 user_info]
  let lock_output = Output (show cap) "0x" script Nothing
  let outputs = [lock_output]
  let charge = input_capacity - cap
  let charge_script = Script (contract_info_code_hash system_script_info) [userInfo_blake160 user_info]
  let charge_output = Output (show charge) "0x" charge_script Nothing
  let outputs = if charge /= 0 then [lock_output, charge_output] else [lock_output]
  let dep = mkDepFormContract system_script_info
  let deps = [dep]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  sendTransaction (user_info, tx)

callContract :: ContractInfo -> Hash -> Dapp Hash
callContract contractInfo preHash = do
  c <- getLiveCellByTxHashIndex (preHash, "0")
  let output = cell_with_status_cell c
  let input = mkInput preHash "0" "0"
  let dep = mkDepFormContract contractInfo
  let deps = [dep]
  let inputs = [input]
  let outputs = [output]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  stx <- always_success_lock tx
  sendRawTransaction stx


-- util functions for client interpreter
aeson_decode :: FromJSON a => String -> Maybe a
aeson_decode str = decode $ pack $ map c2w str

aeson_encode :: ToJSON a => a -> String
aeson_encode x = (map w2c) $ unpack $ encode x

to_MaybeT :: Maybe a -> MaybeT IO a
to_MaybeT = MaybeT . return

decode_output :: FromJSON a => String -> MaybeT IO a
decode_output str = to_MaybeT $ aeson_decode str

call_ruby :: FromJSON b => String -> [String] -> MaybeT IO b
call_ruby func args = do
  pwd <- liftIO $ getCurrentDirectory
  output <- liftIO $ call (pwd <> "/ruby/" <> func <> ".sh") args
  decode_output output


-- type of eval result
type DappIO = MaybeT IO

-- client interpreter: translate DSL code to ruby
clientInterpreter :: DappOperator (DappIO next) -> DappIO next
clientInterpreter (GetUserInfo privkey next) = do
  ret <- call_ruby "getUserInfo" [privkey]
  next ret
clientInterpreter (LockHash (code_hash, args) next) = do
  ret <- call_ruby "lockHash" ([code_hash] <> args)
  next ret
clientInterpreter (QueryLiveCells (lock_hash, capacity) next) = do
  let scapacity = show capacity
  ret <- call_ruby "queryLiveCells" [lock_hash, scapacity]
  next ret
clientInterpreter (GetLiveCellByTxHashIndex (hash, index) next) = do
  ret <- call_ruby "getLiveCellByTxHashIndex" [hash, index]
  next ret
clientInterpreter (DeployContract (userinfo, path) next) = do
  let privkey = userInfo_privkey userinfo
  ret <- call_ruby "deployContract" [privkey, path]
  next ret
clientInterpreter (Sign (userinfo, tx) next) = do
  let privkey = userInfo_privkey userinfo
  let tx_s = aeson_encode tx
  let path = "/tmp/tx"
  liftIO $ writeFile path tx_s
  ret <- call_ruby "sign" [privkey, path]
  next ret
clientInterpreter (Ask prompt next) = do
  liftIO $ putStrLn prompt
  something <- liftIO $ getLine
  next something
clientInterpreter (SystemScript next) = do
  ret <- call_ruby "systemScript" []
  next ret
clientInterpreter (SendTransaction (userinfo, tx) next) = do
  let privkey = userInfo_privkey userinfo
  let tx_s = aeson_encode tx
  let path = "/tmp/tx"
  liftIO $ writeFile path tx_s
  ret <- call_ruby "sendTransaction" [privkey, path]
  next ret
clientInterpreter (SendRawTransaction rtx next) = do
  let rtx_s = aeson_encode rtx
  let path = "/tmp/rtx"
  liftIO $ writeFile path rtx_s
  ret <- call_ruby "sendRawTransaction" [path]
  next ret

-- run program write by DSL
runDeploy = runMaybeT (iterM clientInterpreter $ always_success)
runSetup info = runMaybeT (iterM clientInterpreter $ (moveCapacityToContract info))
runCall info hash = runMaybeT (iterM clientInterpreter $ (callContract info hash))
