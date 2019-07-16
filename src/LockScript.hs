{-# LANGUAGE DeriveFunctor, TemplateHaskell, FlexibleContexts #-}
module LockScript where

import Type

import Language.C99.Simple

import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)

import Control.Monad.State (State, state, modify)

-- define of DSL
data LockOperator tx next =
  Nop next
  deriving (Functor)

makeFree ''LockOperator

type ScriptOperator = LockOperator ResolvedTransaction

type LockScript = Free ScriptOperator

-- example of DSL
-- always_success


-- util functions for interpreter


-- lock script interpreter: translate DSL code to lock process
-- type of eval result
type LockST = State ResolvedTransaction

lockInterpreter :: ScriptOperator (LockST next) -> LockST next
lockInterpreter (Nop next) = do
  modify id
  next


-- contract interpreter: translate DSL code to c code
-- type of eval result
type ContractST = State [Stmt]

contractInterpreter :: ScriptOperator (ContractST next) -> ContractST next
contractInterpreter (Nop next) = do
  modify ((Return $ Just (LitInt 0)) :)
  next
