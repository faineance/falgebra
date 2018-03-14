module Algebra where
import Protolude hiding ((:+:))

import Data.Text.Prettyprint.Doc
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


-- | Monads for free
data Term f a = Pure a | Impure (f (Term f a)) deriving (Functor)

instance Functor f => Applicative (Term f) where
   pure x                    = Pure x
   (Pure f)   <*> (Pure x)   = Pure (f x)
   (Pure f)   <*> (Impure t) = undefined -- help me !
   (Impure f) <*> (Pure   t) = undefined -- help me !
   (Impure f) <*> (Impure t) = undefined -- help me !

instance Functor f => Monad (Term f) where
   return x         = Pure x
   (Pure x)   >>= f = f x
   (Impure t) >>= f = Impure (fmap (>>= f) t)


inject' :: (g :<: f) => g (Term f a) -> Term f a
inject' = Impure . inj


foldTerm :: Functor f => (a -> b) -> (f b -> b) -> Term f a -> b
foldTerm pure imp (Pure x)   = pure x
foldTerm pure imp (Impure t) = imp (fmap (foldTerm pure imp) t)


-- | Pretty Printing
--
class Render f where
   render :: Render g  => f (Fix g) -> [Char]

pretty :: Render f => Fix f -> [Char]
pretty (In t) = render t

instance (Render f , Render g) => Render (f :+: g) where
  render = either render render . runLift
