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

-- define of DSL
data Operator next =
    GetLiveCellsByCapacity Int (RetGetLiveCellsByCapacity -> next)
  | GetLiveCellByTxHashIndex (Hash, Index) (CellWithStatus -> next)
  | DeployContract Path (ContractInfo -> next)
--  | Sign Transaction ([Witness] -> next)
  | SystemScriptDep (Dep -> next)
  | SendTransaction Transaction (Hash -> next)
  | SendRawTransaction Transaction (Hash -> next)
  | Ask String (String -> next)
  | MyLock (Script -> next)
  deriving (Functor)

makeFree ''Operator

type DappOperator = Operator

type Dapp = Free DappOperator

-- example of DSL for test
proc :: Dapp String
proc = do
  ask "input capacity"

test1 :: Dapp RetGetLiveCellsByCapacity
test1 = do
  scap <- proc
  let cap = read scap :: Int
  getLiveCellsByCapacity cap

test2 :: Dapp CellWithStatus
test2 = do
  ret <- test1
  let Just(c) =  outpoint_cell $ input_previous_output $ head $ ret_getLiveCellsByCapacity_inputs ret
  let args = (cell_outpoint_tx_hash c, cell_outpoint_index c)
  getLiveCellByTxHashIndex args

test3 :: Dapp ContractInfo
test3 = do
  path <- ask "contract path"
  deployContract path

deployAlwaysSuccess :: Dapp ContractInfo
deployAlwaysSuccess = do
  deployContract "/home/rink/work/github/edsl/contract/build/always_success"

lockCapacityWithContract :: ContractInfo -> Dapp Hash
lockCapacityWithContract contractInfo = do
  scap <- ask "input capacity"
  let cap = read scap :: Int
  ret <- getLiveCellsByCapacity cap
  let input_capacity_s = ret_getLiveCellsByCapacity_capacity ret
  let input_capacity = read input_capacity_s :: Int
  let inputs = ret_getLiveCellsByCapacity_inputs ret
  client_lock <- myLock
  let script = Script (contract_info_code_hash contractInfo) (script_args client_lock)
  let lock_output = Output scap "0x" script Nothing
  let outputs = [lock_output]
  let charge = input_capacity - cap
  let charge_output = Output (show charge) "0x" client_lock Nothing
  let outputs = if charge /= 0 then [lock_output, charge_output] else [lock_output]
  dep <- systemScriptDep
  let deps = [dep]
  let tx = Transaction "0x" "0" deps inputs outputs (fake_witness $ length inputs)
  sendTransaction tx

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
  sendTransaction tx

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
clientInterpreter (GetLiveCellsByCapacity capacity next) = do
  let scapacity = show capacity
  ret <- call_ruby "getLiveCellsByCapacity" [scapacity]
  next ret
clientInterpreter (GetLiveCellByTxHashIndex args next) = do
  let (hash, index) = args
  ret <- call_ruby "getLiveCellByTxHashIndex" [hash, index]
  next ret
clientInterpreter (DeployContract path next) = do
  ret <- call_ruby "deployContract" [path]
  next ret
clientInterpreter (Ask prompt next) = do
  liftIO $ putStrLn prompt
  something <- liftIO $ getLine
  next something
clientInterpreter (SystemScriptDep next) = do
  ret <- call_ruby "systemScriptDep" []
  next ret
clientInterpreter (MyLock next) = do
  ret <- call_ruby "myLock" []
  next ret
clientInterpreter (SendTransaction tx next) = do
  let tx_s = aeson_encode tx
  let path = "/tmp/tx"
  liftIO $ writeFile path tx_s
  ret <- call_ruby "sendTransaction" [path]
  next ret
clientInterpreter (SendRawTransaction rtx next) = do
  let rtx_s = aeson_encode rtx
  let path = "/tmp/rtx"
  liftIO $ writeFile path rtx_s
  ret <- call_ruby "sendRawTransaction" [path]
  next ret


runTest2 = runMaybeT (iterM clientInterpreter $ test2)
runDeploy = runMaybeT (iterM clientInterpreter $ test3)
runSetup info = runMaybeT (iterM clientInterpreter $ (lockCapacityWithContract info))
runCall info hash = runMaybeT (iterM clientInterpreter $ (callContract info hash))