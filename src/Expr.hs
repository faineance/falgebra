{-# LANGUAGE UndecidableInstances #-}
module Expr where
import Protolude hiding ((:+:))
import qualified Data.Map as Map
import Algebra
import Common

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

data IncrExpr t   = Incr Int t         deriving Functor

data RecallExpr t = Recall (Int -> t)  deriving Functor


-- Smart constructors
lit :: (LitExpr :<: f) => Int -> Fix f
lit = inject . Lit

add :: (ArithExpr :<: f) => Fix f -> Fix f -> Fix f
add x y = inject $ Add x y

var :: (VarExpr :<: f) => Id -> Fix f
var = inject . Var

assign :: (VarExpr :<: f) => Id -> Fix f -> Fix f
assign v x = inject $ Assign v x

incr :: (IncrExpr :<: f) => Int -> Term f ()
incr i = inject' (Incr i (Pure ()))

recall :: (RecallExpr :<: f) => Term f Int
recall = inject' (Recall Pure)
{-seq_ :: (SeqExpr :<: f) => Fix f -> Fix f -> Fix f-}
{-seq_ e1 e2 = inject $ Seq e1 e2-}

-- Interpreter
instance Applicative m => Eval LitExpr m Int where
  evalAlgebra (Lit i) = pure i

instance Applicative m => Eval ArithExpr m Int where
  evalAlgebra (Add x y) = (+) <$> x <*> y
  evalAlgebra (Sub x y) = (-) <$> x <*> y
  evalAlgebra (Mul x y) = (*) <$> x <*> y

{-instance MonadState (Map.Map Id v) Maybe => Eval VarExpr (StateT (Map.Map Id v) Maybe) v where-}
instance Eval VarExpr (StateT (Map.Map Id v) Maybe) v where
  evalAlgebra (Var id) = get >>= lift . Map.lookup id
  {-evalAlgebra (Assign id e) = do-}
    {-e' <- e-}
    {-modify $ Map.insert id e'-}
    {-return e'-}


evalExpr :: Expr -> Map.Map Id Int -> Maybe (Int, Map.Map Id Int)
evalExpr = runStateT . eval



tick :: Term (IncrExpr :+: RecallExpr) ()
tick = do
   {-y <- recall-}
   incr 1
   {-return y-}


newtype Mem = Mem Int deriving Show

class Functor f => Run f where
   runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))

instance Run IncrExpr where
   runAlgebra (Incr k r) (Mem i) = r (Mem (i+k))
instance Run RecallExpr  where
   runAlgebra (Recall r) (Mem i) = r i (Mem i)

instance (Run f, Run g) => Run (f :+: g) where
  runAlgebra = either runAlgebra runAlgebra . runLift

run :: Run f => Term f a -> Mem -> (a, Mem)
run = foldTerm (,) runAlgebra

{-program = seq_  (assign "a" (lit 2))  ( (assign "b" (lit 2)))-}
