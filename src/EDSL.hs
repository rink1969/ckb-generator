{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module EDSL where

import Type
import Call

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

import Data.ByteString.Lazy (pack)
import Data.ByteString.Internal (c2w)

import Data.Aeson (decode, FromJSON)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Control.Monad.IO.Class (liftIO)

import System.Directory (getCurrentDirectory)

-- define of DSL
data Operator next =
    GetLiveCellsByCapacity Int (RetGetLiveCellsByCapacity -> next)
  | GetLiveCellByTxHashIndex (Hash, Index) (CellWithStatus -> next)
--  | GetLiveCellByTxIndex (Transaction, Int) ([Input] -> next)
  | DeployContract Path (ContractInfo -> next)
--  | Lock (ContractInfo, [Arg], Maybe Capacity, Maybe Data) ([Output] -> next)
--  | Process ([Input], [Arg], Maybe Capacity, Maybe Data, Maybe Lock) ([Output] -> next)
--  | ResolveDeps [Input] ([Dep] -> next)
--  | Sign Transaction (TransactionWithWitnesses -> next)
--  | SendTx TransactionWithWitnesses next
  | Ask String (String -> next)
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


-- util functions for client interpreter
aeson_decode :: FromJSON a => String -> Maybe a
aeson_decode str = decode $ pack $ map c2w str

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

runTest2 = runMaybeT (iterM clientInterpreter $ test2)
runTest3 = runMaybeT (iterM clientInterpreter $ test3)