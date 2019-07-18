module CodeGen where

import Control.Monad.State  (runState)
import Data.List            (union)
import Data.Typeable        (Typeable)

import Text.PrettyPrint     (render)
import Language.C99.Pretty  (pretty)
import Language.C99.Simple


-- mainFunc = FunDef (TypeSpec Int) "main" [Param (TypeSpec Int) "argc", Param (Array (Ptr (TypeSpec Char)) Nothing) "argv"] [] [Return $ Just (LitInt 0)]

-- printMain = render $ pretty $ translate $ TransUnit [] [mainFunc]
retDecl = VarDecln Nothing (TypeSpec Int) "ret" (Just $ InitExpr $ LitInt 0)
includes = unlines [ "#include <script.h>"
                   , "#include \"components/witnessCount.h\""
                   , "#include \"components/binaryVote.h\""
                   , "#include \"components/hashLock.h\""
                   , "#include \"components/multiSignatures_data_config.h\""
                   , "#include \"components/multiSignatures_argv.h\""]

genCode stmts = includes <> code where
  new_stmts = reverse $ (Return $ Just (Ident "ret")) : stmts
  mainFunc = FunDef (TypeSpec Int) "main" [Param (TypeSpec Int) "argc", Param (Array (Ptr (TypeSpec Char)) Nothing) "argv"] [] new_stmts
  code = render $ pretty $ translate $ TransUnit [retDecl] [mainFunc]

testGenCode = genCode [Expr $ AssignOp Assign (Ident "ret") (BinaryOp LOr (Ident "ret") (Funcall (Ident "verify_sighash_all") [Index (Ident "argv") (LitInt 0), LitInt 0]))]