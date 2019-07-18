{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module EDSL where

import Type
import Call
import LockScript
import CodeGen

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

import Data.ByteString.Lazy (pack, unpack)
import Data.ByteString.Internal (c2w, w2c)

import Data.Aeson (encode, decode, FromJSON, ToJSON)

import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.IO.Class (liftIO)

import System.Directory (getCurrentDirectory)

import Control.Monad.Free (iterM)
import Control.Monad.State (execState)


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
  | MkDappInfo (Name, Maybe (LockScript())) (DappInfo -> next)
  deriving (Functor)

makeFree ''Operator

type DappOperator = Operator

type Dapp = Free DappOperator


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

gen_contract :: Name -> LockScript () -> IO ()
gen_contract name lock_script = do
  let (expr, _) = execState (iterM contractInterpreter $ lock_script) (initValueGenCode, initFlag)
  let code = genCode expr
  path <- source_abs_path name
  writeFile path code


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

clientInterpreter (MkDappInfo ("system", Nothing) next) = do
  info <- call_ruby "systemScript" []
  let system_info = DappInfo info Nothing
  next system_info
clientInterpreter (MkDappInfo (name, Just lock_script) next) = do
  liftIO $ gen_contract name lock_script
  liftIO $ compile_contract name
  liftIO $ putStrLn "Please input privkey:"
  privkey <- liftIO $ getLine
  path <- liftIO $ elf_abs_path name
  contract_info <- call_ruby "deployContract" [privkey, path]
  let lock_func = \rtx -> execState (iterM lockInterpreter $ lock_script) rtx
  let dapp_info = DappInfo contract_info (Just lock_func)
  next dapp_info
