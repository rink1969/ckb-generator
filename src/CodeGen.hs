module CodeGen where

import Control.Monad.State  (runState)
import Data.List            (union)
import Data.Typeable        (Typeable)

import Text.PrettyPrint     (render)
import Language.C99.Pretty  (pretty)
import Language.C99.Simple


-- mainFunc = FunDef (TypeSpec Int) "main" [Param (TypeSpec Int) "argc", Param (Array (Ptr (TypeSpec Char)) Nothing) "argv"] [] [Return $ Just (LitInt 0)]

-- printMain = render $ pretty $ translate $ TransUnit [] [mainFunc]

genCode stmts = render $ pretty $ translate $ TransUnit [] [mainFunc] where
  mainFunc = FunDef (TypeSpec Int) "main" [Param (TypeSpec Int) "argc", Param (Array (Ptr (TypeSpec Char)) Nothing) "argv"] [] stmts