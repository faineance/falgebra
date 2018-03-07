module Expr where
import Protolude hiding ((:+:))
import Algebra



type Id = Text

-- Expression types
type Expr = Fix (LitExpr :+: ArithExpr :+: VarExpr)

data LitExpr a = Lit Int
  deriving (Functor)

data ArithExpr a =
    Add a a
  | Sub a a
  | Mul a a
  deriving (Functor)

data VarExpr a =
    Var Text
  | Assign Text a
  deriving (Functor)



-- Smart constructors
lit :: (LitExpr :<: f) => Int -> Fix f
lit = inject . Lit

add :: (ArithExpr :<: f) => Fix f -> Fix f -> Fix f
add x y = inject $ Add x y

var :: (VarExpr :<: f) => Id -> Fix f
var = inject . Var

assign :: (VarExpr :<: f) => Id -> Fix f -> Fix f
assign v x = inject $ Assign v x
