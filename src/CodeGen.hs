{-# LANGUAGE RecursiveDo #-}
module CodeGen where
import Protolude
import Algebra
import Expr
import qualified LLVM.AST                   as LLVM
import qualified LLVM.AST.Constant          as LLVM
import qualified LLVM.AST.Float             as LLVM
import qualified LLVM.AST.Type              as LLVM
import qualified LLVM.Pretty                as LLVMPretty
import qualified LLVM.IRBuilder.Instruction as LLVMIR
import qualified LLVM.IRBuilder.Constant as LLVMIR
import qualified LLVM.IRBuilder.Module      as LLVMIR
import qualified LLVM.IRBuilder.Monad       as LLVMIR



instance LLVMIR.MonadIRBuilder m => Eval LitExpr m LLVM.Operand where
  evalAlgebra (Lit i) = LLVMIR.int32 (toInteger i)

instance LLVMIR.MonadIRBuilder m => Eval ArithExpr m LLVM.Operand where
  {-evalAlgebra _ = pure $ LLVM.ConstantOperand $ ( LLVM.Int 32 (toInteger i))-}
  evalAlgebra op = case op of
    (Add a b) -> do
      a' <- a
      b' <- b
      LLVMIR.add a' b' `LLVMIR.named` "x"
    (Sub a b) -> do
      a' <- a
      b' <- b
      LLVMIR.sub a' b' `LLVMIR.named` "x"
    (Mul a b) -> do
      a' <- a
      b' <- b
      LLVMIR.mul a' b' `LLVMIR.named` "x"

instance LLVMIR.MonadIRBuilder m => Eval VarExpr m LLVM.Operand where
  evalAlgebra (Var id) = pure $ (LLVM.LocalReference LLVM.i32  (LLVM.mkName (toS id)))
  evalAlgebra (Assign id e) = e `LLVMIR.named` "y"


evalExpr :: Expr -> (LLVM.Operand, [LLVM.BasicBlock])
evalExpr e = LLVMIR.runIRBuilder LLVMIR.emptyIRBuilder (eval e)

example = LLVMIR.buildModule "exampleModule" $ mdo

  LLVMIR.function "main" [] LLVM.i32 $ \[] -> mdo

    entry <- LLVMIR.block `LLVMIR.named` "entry"; do
      c <- eval $ (add (var "b") (lit 2)  :: Expr)
      LLVMIR.ret c
