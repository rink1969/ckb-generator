module CodeGen where

import Control.Monad.State  (runState)
import Data.List            (union)
import Data.Typeable        (Typeable)

import Text.PrettyPrint     (render)
import Language.C99.Pretty  (pretty)
import Language.C99.Simple


initValueGenCode = LitInt 1;
initFlag = 1 :: Int;

retDecl = VarDecln Nothing (TypeSpec Int) "ret" (Just $ InitExpr $ LitInt 0)
includes = unlines [ "#include <script.h>"
                   , "#include \"components/getArg.h\""
                   , "#include \"components/binaryVote.h\""
                   , "//#include \"components/hashLock.h\""
                   , "#include \"components/multiSignatures_data_config.h\""
                   , "//#include \"components/multiSignatures_argv.h\""]

genCode expr = includes <> code where
  mainFunc = FunDef (TypeSpec Int) "main" [] [] [Return $ Just expr]
  code = render $ pretty $ translate $ TransUnit [] [mainFunc]

testGenCode = genCode (BinaryOp LOr (Funcall (Ident "verify_sighash_all") [Funcall (Ident "get_arg") [], LitInt 0]) (LitInt 0))