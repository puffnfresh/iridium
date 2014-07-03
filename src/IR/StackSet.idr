module IR.StackSet

import IR

stackLength : Stack wid -> Nat
stackLength (MkStack _ ys zs) = S (length ys + length zs)

integrate : (s : Stack wid) -> Vect (stackLength s) wid
integrate (MkStack x ys zs) =
  rewrite plusSuccRightSucc (length ys) (length zs)
  in reverse (fromList ys) ++ x :: fromList zs
