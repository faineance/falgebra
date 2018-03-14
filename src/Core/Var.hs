module Core.Var where
import Protolude hiding ((:+:))
import qualified Data.Map as Map
import Algebra
type Id = Text

data VarExpr a =
    Var Id
  | Assign Id a
  deriving (Functor)

var :: (VarExpr :<: f) => Id -> Fix f
var = inject . Var

assign :: (VarExpr :<: f) => Id -> Fix f -> Fix f
assign v x = inject $ Assign v x


-- Interpreter
{-instance MonadState (Map.Map Id v) Maybe => Eval VarExpr (StateT (Map.Map Id v) Maybe) v where-}
instance Eval VarExpr (StateT (Map.Map Id v) Maybe) v where
  evalAlgebra (Var id) = get >>= lift . Map.lookup id
  {-evalAlgebra (Assign id e) = do-}
    {-e' <- e-}
    {-modify $ Map.insert id e'-}
    {-return e'-}



instance Render VarExpr where
  {-render (Add a b) = pretty a ++ "+" ++ pretty b-}


