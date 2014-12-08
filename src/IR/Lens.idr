module IR.Lens

import Control.Category

data Store s a = MkStore (s -> a) s

class Functor w => Comonad (w : Type -> Type) where
  extract : w a -> a
  extend : (w a -> b) -> w a -> w b

class Comonad w => VerifiedComonad (w : Type -> Type) where
  comonadLaw1 : (wa : w a) ->
                extend extract wa = wa
  comonadLaw2 : (f : w a -> b) ->
                (wa : w a) ->
                extract (extend f wa) = f wa
  comonadLaw3 : (f : w b -> c) ->
                (g : w a -> b) ->
                (wa : w a) ->
                extend f (extend g wa) = extend (\d => f (extend g d)) wa

instance Functor (Store s) where
  map f (MkStore g a) = MkStore (f . g) a

instance Comonad (Store s) where
  extract (MkStore f a) = f a
  extend f (MkStore g a) = MkStore (\b => f (MkStore g b)) a

instance VerifiedComonad (Store s) where
  comonadLaw1 (MkStore f a) = ?storeIdentityProof
  comonadLaw2 f (MkStore g a) = Refl
  comonadLaw3 f g (MkStore h a) = Refl

-- TODO: This is evil.
-- Supposedly (jonsterling) this definition used to work without the believe_me.
eta : (f : a -> b) -> f = (\c => f c)
eta g = believe_me Refl {g}

storeIdentityProof = proof
  intros
  rewrite eta f
  trivial

pos : Store s a -> s
pos (MkStore _ s) = s

peek : s -> Store s a -> a
peek s (MkStore f _) = f s

peeks : (s -> s) -> Store s a -> a
peeks f (MkStore g s) = g (f s)

data Lens a b = MkLens (a -> Store b a)

instance Category Lens where
  id = MkLens (MkStore id)
  (.) (MkLens f) (MkLens g) = MkLens (\a => case g a of
    MkStore ba b => case f b of
      MkStore cb c => MkStore (Prelude.Basics.(.) ba cb) c)

lens : (a -> b) -> (b -> a -> a) -> Lens a b
lens f g = MkLens (\a => MkStore (\b => g b a) (f a))

iso : (a -> b) -> (b -> a) -> Lens a b
iso f g = MkLens (\a => MkStore g (f a))

getL : Lens a b -> a -> b
getL (MkLens f) a = pos (f a)

setL : Lens a b -> b -> a -> a
setL (MkLens f) b = peek b . f

modL : Lens a b -> (b -> b) -> a -> a
modL (MkLens f) g = peeks g . f

infixr 0 ^$
(^$) : Lens a b -> a -> b
(^$) = getL

infixr 4 ^=
(^=) : Lens a b -> b -> a -> a
(^=) = setL

infixr 4 ^%=
(^%=) : Lens a b -> (b -> b) -> a -> a
(^%=) = modL
