module ExprToStack where
import Protolude
import Algebra
import qualified Expr
import qualified Stack


instance Compile Expr.LitExpr Stack.Bytecode where
  compileAlgebra (Expr.Lit n)   = Stack.push n

instance Compile Expr.ArithExpr Stack.Bytecode where
  compileAlgebra (Expr.Add x y) = x . y . Stack.add

instance Compile Expr.VarExpr Stack.Bytecode where
  compileAlgebra (Expr.Var v) = Stack.load v
  compileAlgebra (Expr.Assign id e) = e . Stack.store id



instance Compile Expr.IncrExpr Stack.Bytecode where
  compileAlgebra (Expr.Incr v c) = Stack.push v
  {-compileAlgebra (Expr.Assign id e) = e . Stack.store id-}



