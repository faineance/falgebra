module Stack where

import Protolude hiding ((:+:))
import Common
import Algebra

type Bytecode = Fix (Memory :+: Stack :+: ALU :+: Null)

data Stack a =
    Push Int a
  | Pop a
  deriving (Functor)

data ALU a =
    Add a
  | Sub a
  | Mul a
  deriving (Functor)

data Memory a =
    Store Id a
  | Load Id a
  deriving (Functor)

data Null a = Null deriving (Functor)

-- Smart constructors
push :: (Stack :<: f) => Int -> Fix f -> Fix f
push n = inject . Push n

add, sub, mul :: (ALU :<: f) => Fix f -> Fix f
add = inject . Add
sub = inject . Sub
mul = inject . Mul

load :: (Memory :<: f) => Id -> Fix f -> Fix f
load v = inject . Load v

store :: (Memory :<: f) => Id -> Fix f -> Fix f
store v = inject . Load v


null :: (Null :<: f) =>  Fix f
null = inject Null


