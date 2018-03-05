module Algebra where
import Protolude hiding ((:+:))
import Data.Bifunctor

-- Coproduct
newtype Lift p f g a = Lift { runLift :: p (f a) (g a) }
type (:*:) = Lift (,)
type (:+:) = Lift Either
inl = Lift . Left
inr = Lift . Right

instance (Bifunctor p, Functor f, Functor g) => Functor (Lift p f g) where
  fmap f = Lift . bimap (fmap f) (fmap f) . runLift

-- Functor subtyping
class (Functor f, Functor g) => f :<: g where
  inj :: f a -> g a

instance Functor f => f :<: f where
  inj = identity

instance {-# OVERLAPPABLE #-}(Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = inl . inj

instance (Functor f, Functor g) => f :<: (g :+: f) where
  inj = inr

inject :: (g :<: f) => g (Fix f) -> Fix f
inject = In . inj

-- Fixed-points
newtype Fix f = In { out :: f (Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata f = f . fmap (cata f) . out


type Algebra f a = f a -> a

class Compile f carrier  where
  compileAlgebra :: Algebra f (carrier -> carrier)

instance (Compile f code, Compile g code) => Compile (f :+: g) code where
  compileAlgebra = either compileAlgebra compileAlgebra . runLift

compile :: (Functor f, Compile f code) => Fix f -> code -> code
compile = cata compileAlgebra

class Eval f m v where
  evalAlgebra :: Algebra f (m v)

instance (Eval f m v, Eval g m v) => Eval (f :+: g) m v where
  evalAlgebra = either evalAlgebra evalAlgebra . runLift

eval :: (Functor f, Eval f m v) => Fix f -> m v
eval = cata evalAlgebra
