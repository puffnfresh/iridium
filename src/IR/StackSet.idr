module IR.StackSet

import IR
import IR.Lens

manage : Eq wid => wid -> Workspace wid -> Workspace wid
manage wid (MkWorkspace l Nothing) = MkWorkspace l (Just (MkStack wid [] []))
manage wid (MkWorkspace l (Just (MkStack wid' a b))) = MkWorkspace l (Just (MkStack wid a (wid' :: b)))

integrate : (s : Stack wid) -> Vect (stackLength s) wid
integrate (MkStack x ys zs) =
  rewrite plusSuccRightSucc (length ys) (length zs)
  in reverse (fromList ys) ++ x :: fromList zs

member : Eq wid => wid -> StackSet wid sid -> Bool
member wid s =
  maybe False (\stack => elem wid (integrate stack)) (workspaceStack' . screenWorkspace' . stackSetCurrent' ^$ s)

filter : (wid -> Bool) -> Stack wid -> Maybe (Stack wid)
filter p (MkStack f ls rs) = case filter p (f::rs) of
  f'::rs' => Just (MkStack f' (filter p ls) rs')
  [] => case filter p ls of
    f'::ls' => Just (MkStack f' ls' [])
    [] => Nothing

-- TODO: Use more than stackSetCurrent'
delete : Eq wid => wid -> StackSet wid sid -> StackSet wid sid
delete wid = workspaceStack' . screenWorkspace' . stackSetCurrent' ^%= (>>= filter (/= wid))

insertUp : Eq wid => wid -> StackSet wid sid -> StackSet wid sid
insertUp wid s = if member wid s then s else insert
  where insert = workspaceStack' . screenWorkspace' . stackSetCurrent' ^%= Just . maybe (MkStack wid [] []) (\(MkStack t l r) => MkStack wid l (t::r)) $ s

modify' : (Stack wid -> Stack wid) -> StackSet wid sid -> StackSet wid sid
modify' f = workspaceStack' . screenWorkspace' . stackSetCurrent' ^%= map f

reverseStack : Stack wid -> Stack wid
reverseStack (MkStack t ls rs) = MkStack t rs ls

focusUp' : Stack wid -> Stack wid
focusUp' (MkStack t [] []) = MkStack t [] []
focusUp' (MkStack t (l::ls) rs) = MkStack l ls (t::rs)
focusUp' (MkStack t [] rs) =
  let (x::xs) = reverse (t::fromList rs)
  in MkStack x (toList xs) []

focusDown' : Stack wid -> Stack wid
focusDown' = reverseStack . focusUp' . reverseStack

focusDown : StackSet wid sid -> StackSet wid sid
focusDown = modify' focusDown'

focusUp : StackSet wid sid -> StackSet wid sid
focusUp = modify' focusUp'

swapUp' : Stack wid -> Stack wid
swapUp' (MkStack t (l::ls) rs) = MkStack t ls (l::rs)
swapUp' (MkStack t []     rs) = MkStack t (reverse rs) []

swapUp : StackSet wid sid -> StackSet wid sid
swapUp = modify' swapUp'

swapDown : StackSet wid sid -> StackSet wid sid
swapDown = modify' (reverseStack . swapUp' . reverseStack)

windows : (StackSet wid sid -> StackSet wid sid) -> { [IR wid sid, STATE (IRState wid sid)] } Eff ()
windows f = do
  update (irStateStackSet' ^%= f)
  refresh
