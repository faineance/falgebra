{-# LANGUAGE UndecidableInstances #-}
module Expr where
import Protolude hiding ((:+:))
import Algebra
import Parser
import Core.Var
import Core.Arith
import Core.Lit
-- Expression types
--
type Expr = Fix (LitExpr :+: ArithExpr)




{-data IncrExpr t   = Incr Int t         deriving Functor-}

{-data RecallExpr t = Recall (Int -> t)  deriving Functor-}


{-incr :: (IncrExpr :<: f) => Int -> Term f ()-}
{-incr i = inject' (Incr i (Pure ()))-}

{-recall :: (RecallExpr :<: f) => Term f Int-}
{-recall = inject' (Recall Pure)-}
{-seq_ :: (SeqExpr :<: f) => Fix f -> Fix f -> Fix f-}
{-seq_ e1 e2 = inject $ Seq e1 e2-}




{-evalExpr :: Expr -> Map.Map Text Int -> Maybe (Int, Map.Map Text Int)-}
{-evalExpr = runStateT . eval-}



{-tick :: Term (IncrExpr :+: RecallExpr) ()-}
{-tick = do-}
   {-{-y <- recall-}-}
   {-incr 1-}
{-   [>}-return y-}


{-newtype Mem = Mem Int deriving Show-}

{-class Functor f => Run f where-}
   {-runAlgebra :: f (Mem -> (a, Mem)) -> (Mem -> (a, Mem))-}

{-instance Run IncrExpr where-}
   {-runAlgebra (Incr k r) (Mem i) = r (Mem (i+k))-}
{-instance Run RecallExpr  where-}
   {-runAlgebra (Recall r) (Mem i) = r i (Mem i)-}

{-instance (Run f, Run g) => Run (f :+: g) where-}
  {-runAlgebra = either runAlgebra runAlgebra . runLift-}

{-run :: Run f => Term f a -> Mem -> (a, Mem)-}
{-run = foldTerm (,) runAlgebra-}

{-program = seq_  (assign "a" (lit 2))  ( (assign "b" (lit 2)))-}
