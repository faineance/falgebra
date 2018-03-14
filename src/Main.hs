
module Main where
import CodeGen
import Protolude hiding ((:+:))
import ExprToStack
import Algebra
import Text.Megaparsec
import Core.Var
import Core.Arith
import Core.Lit
import Expr
main :: IO ()
main = do
  putText "hello world"
