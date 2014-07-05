module IR.Layout

import IR
import IR.StackSet

column : Rectangle -> (n : Nat) -> Vect n Rectangle
column r n = column' n r n
  where column' _ _ Z = []
        column' n (MkRectangle x y w h) (S m) =
          let w' = w / fromInteger (toIntegerNat n)
          in MkRectangle (x + w' * fromInteger (toIntegerNat m)) y w' h :: column' n r m

Layout : Type -> Type
Layout wid = Rectangle -> (s : Stack wid) -> Vect (stackLength s) (wid, Rectangle)

fullLayout : Layout wid
fullLayout rect s = zip (integrate s) (replicate (stackLength s) rect)

columnLayout : Layout wid
columnLayout rect s = zip (integrate s) (column rect (stackLength s))

mirrorLayout : Layout wid -> Layout wid
mirrorLayout l (MkRectangle x' y' w' h') s = map (\(wid, MkRectangle x y w h) => (wid, MkRectangle ((y - y') / h' * w' + x') ((x - x') / w' * h' + y') (h / h' * w') (w / w' * h'))) (l (MkRectangle x' y' w' h') s)
