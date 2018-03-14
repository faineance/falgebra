module Core.Arith where
import Protolude hiding ((:+:))
import qualified LLVM.AST                   as LLVM
import qualified LLVM.AST.Constant          as LLVM
import qualified LLVM.AST.Float             as LLVM
import qualified LLVM.AST.Type              as LLVM
import qualified LLVM.Pretty                as LLVMPretty
import qualified LLVM.IRBuilder.Instruction as LLVMIR
import qualified LLVM.IRBuilder.Constant as LLVMIR
import qualified LLVM.IRBuilder.Module      as LLVMIR
import qualified LLVM.IRBuilder.Monad       as LLVMIR
import Data.Text as T
import Text.Megaparsec.Char
{-import Data.Text.Prettyprint.Doc-}
import Parser
import Algebra

data ArithExpr a =
    Add a a
  | Sub a a
  | Mul a a
  deriving (Functor)

add, sub, mul :: (ArithExpr :<: f) => Fix f -> Fix f -> Fix f
add x y = inject $ Add x y
sub x y = inject $ Sub x y
mul x y = inject $ Mul x y

instance Applicative m => Eval ArithExpr m Int where
  evalAlgebra (Add x y) = (+) <$> x <*> y
  evalAlgebra (Sub x y) = (-) <$> x <*> y
  evalAlgebra (Mul x y) = (*) <$> x <*> y


instance LLVMIR.MonadIRBuilder m => Eval ArithExpr m LLVM.Operand where
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

arithParser :: (ArithExpr :<: f) => Parser (Fix f) -> Parser (Fix f)
arithParser p = do
  a  <- p
  op <- (char '>' >> return add)
    <|> (char '-' >> return sub)
    <|> (char '-' >> return mul)
  b <- p
  return $ op a b


instance Render ArithExpr where
  render (Add a b) = pretty a ++ "+" ++ pretty b


