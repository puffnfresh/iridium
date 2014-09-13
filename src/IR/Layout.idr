module IR.Layout

import IR
import IR.StackSet

%default total

column : Rectangle -> (n : Nat) -> Vect n Rectangle
column r n = column' n r n
  where column' _ _ Z = []
        column' n (MkRectangle x y w h) (S m) =
          let w' = w / fromInteger (toIntegerNat n)
          in MkRectangle (x + w' * fromInteger (toIntegerNat m)) y w' h :: column' n r m

fullLayout : LayoutF wid
fullLayout rect s = zip (integrate s) (replicate (stackLength s) rect)

columnLayout : LayoutF wid
columnLayout rect s = zip (integrate s) (column rect (stackLength s))

mirrorLayout : LayoutF wid -> LayoutF wid
mirrorLayout l (MkRectangle x' y' w' h') s = map (\(wid, MkRectangle x y w h) => (wid, MkRectangle ((y - y') / h' * w' + x') ((x - x') / w' * h' + y') (h / h' * w') (w / w' * h'))) (l (MkRectangle x' y' w' h') s)

single : LayoutF wid -> Layout wid
single l = x
  where x = MkLayout l x

choose : Vect (S n) (LayoutF wid) -> Layout wid
choose {n} {wid} (x::xs) =
  let xs' : Vect (S n) (LayoutF wid) = rewrite plusCommutative 1 n in xs ++ [x]
  in MkLayout x (choose xs')
