module Core.Lit where
import Protolude hiding ((:+:))
import Text.Megaparsec.Char
import qualified LLVM.AST                   as LLVM
import qualified LLVM.AST.Constant          as LLVM
import qualified LLVM.AST.Float             as LLVM
import qualified LLVM.AST.Type              as LLVM
import qualified LLVM.Pretty                as LLVMPretty
import qualified LLVM.IRBuilder.Instruction as LLVMIR
import qualified LLVM.IRBuilder.Constant as LLVMIR
import qualified LLVM.IRBuilder.Module      as LLVMIR
import qualified LLVM.IRBuilder.Monad       as LLVMIR
import Parser
import Algebra

data LitExpr a = Lit Integer
  deriving (Functor)

lit :: (LitExpr :<: f) => Integer -> Fix f
lit = inject . Lit

instance Applicative m => Eval LitExpr m Integer where
  evalAlgebra (Lit i) = pure i

instance LLVMIR.MonadIRBuilder m => Eval LitExpr m LLVM.Operand where
  evalAlgebra (Lit i) = LLVMIR.int32 (toInteger i)

litParser :: (LitExpr :<: f) => Parser (Fix f)
litParser = do
  v <- integer
  return $ lit v



instance Render LitExpr where
  render (Lit i) = show i


