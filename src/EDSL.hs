{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
{-# LANGUAGE DuplicateRecordFields #-}
module EDSL where

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

import Type
import Call

import Data.ByteString.Lazy (pack)
import Data.ByteString.Internal (c2w)

import Data.Aeson (decode)

import Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import Control.Monad.IO.Class (liftIO)

import System.Directory (getCurrentDirectory)

data Operator next =
    GetLiveCellsByCapacity Int (RetGetLiveCellsByCapacity -> next)
--  | GetLiveCellByTxHashIndex (Hash, Int) ([Input] -> next)
--  | GetLiveCellByTxIndex (Transaction, Int) ([Input] -> next)
--  | DeployContract (Path, Lock) (([Output], ContractInfo) -> next)
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

proc :: Dapp String
proc = do
  ask "input capacity"

test1 :: Dapp RetGetLiveCellsByCapacity
test1 = do
  scap <- proc
  let cap = read scap :: Int
  getLiveCellsByCapacity cap


decodeRetGetLiveCellsByCapacity :: String -> Maybe RetGetLiveCellsByCapacity
decodeRetGetLiveCellsByCapacity str = decode $ pack $ map c2w str :: Maybe RetGetLiveCellsByCapacity

type DappIO = MaybeT IO
clientInterpreter :: DappOperator (DappIO next) -> DappIO next
clientInterpreter (GetLiveCellsByCapacity capacity next) = do
  let scapacity = show capacity
  pwd <- liftIO $ getCurrentDirectory
  output <- liftIO $ call (pwd <> "/ruby/getLiveCellsByCapacity.sh") [scapacity]
  ret <- MaybeT . return $ decodeRetGetLiveCellsByCapacity output
  next ret
clientInterpreter (Ask prompt next) = do
  liftIO $ putStrLn prompt
  something <- liftIO $ getLine
  next something

runTest = runMaybeT (iterM clientInterpreter $ test1)