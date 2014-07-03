module IR.Layout

import IR
import IR.StackSet

column : Rectangle -> (n : Nat) -> Vect n Rectangle
column r n = column' n r n
  where column' _ _ Z = []
        column' n (MkRectangle x y w h) (S m) =
          let w' = w / fromInteger (toIntegerNat n)
          in MkRectangle (x + w' * fromInteger (toIntegerNat m)) y w' h :: column' n r m

columnLayout : Rectangle -> (s : Stack wid) -> Vect (stackLength s) (wid, Rectangle)
columnLayout rect s = zip (integrate s) (column rect (stackLength s))
