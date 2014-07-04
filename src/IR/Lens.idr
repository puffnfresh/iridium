module IR.Lens

import Control.Category

data Store s a = MkStore (s -> a) s

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
