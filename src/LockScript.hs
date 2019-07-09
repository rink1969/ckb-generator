{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module LockScript where

import Type
import CodeGen

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

import Control.Monad.State (State, state)

-- define of DSL
data LockOperator tx next =
  Nop next
  deriving (Functor)

makeFree ''LockOperator

type ScriptOperator = LockOperator ResolvedTransaction

type LockScript = Free ScriptOperator

-- example of DSL
-- always_success
always_success_lock_script :: LockScript ()
always_success_lock_script = do
  nop

-- util functions for interpreter


-- lock script interpreter: translate DSL code to lock process
-- type of eval result
type LockScriptST = State ResolvedTransaction

lockScriptInterpreter :: ScriptOperator (LockScriptST next) -> LockScriptST next
lockScriptInterpreter (Nop next) = do
  state $ \tx -> ((), tx)
  next


-- contract interpreter: translate DSL code to c code
-- type of eval result
generated_contract_path = "/tmp/contract.c"
type LockScriptIO = IO

contractInterpreter :: ScriptOperator (LockScriptIO next) -> LockScriptIO next
contractInterpreter (Nop next) = do
  writeFile generated_contract_path printMain
  next
