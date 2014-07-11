module IR.Event

import IR.Lens

KeyCode : Type
KeyCode = Int

record Key : Type where
  MkKey : (keyCode : KeyCode) ->
          (keyHasAlt : Bool) ->
          (keyHasCmd : Bool) ->
          (keyHasCtrl : Bool) ->
          (keyHasShift : Bool) ->
          Key

instance Eq Key where
  (==) (MkKey a b c d e) (MkKey a' b' c' d' e') = a == a' && b == b' && c == c' && d == d' && e == e'

-- Should be the Monoid instance:
infixl 3 <!>
(<!>) : Ordering -> Ordering -> Ordering
(<!>) EQ r = r
(<!>) l  r = l

-- Should be the Ord instance:
compareBool : Bool -> Bool -> Ordering
compareBool False False = EQ
compareBool False True = LT
compareBool True False = GT
compareBool True True = EQ

instance Ord Key where
  compare (MkKey a b c d e) (MkKey a' b' c' d' e') = compare a a'
                                                 <!> compareBool b b'
                                                 <!> compareBool c c'
                                                 <!> compareBool d d'
                                                 <!> compareBool e e'

keyCode' : Lens Key KeyCode
keyCode' = lens (\(MkKey x _ _ _ _) => x) (\x, (MkKey _ a b c d) => MkKey x a b c d)

keyHasAlt' : Lens Key Bool
keyHasAlt' = lens (\(MkKey _ x _ _ _) => x) (\x, (MkKey a _ b c d) => MkKey a x b c d)

keyHasCmd' : Lens Key Bool
keyHasCmd' = lens (\(MkKey _ _ x _ _) => x) (\x, (MkKey a b _ c d) => MkKey a b x c d)

keyHasCtrl' : Lens Key Bool
keyHasCtrl' = lens (\(MkKey _ _ _ x _) => x) (\x, (MkKey a b c _ d) => MkKey a b c x d)

keyHasShift' : Lens Key Bool
keyHasShift' = lens (\(MkKey _ _ _ _ x) => x) (\x, (MkKey a b c d _) => MkKey a b c d x)

data Event = KeyEvent Key
           | RefreshEvent
           | IgnoredEvent

eventFromPtr : Ptr -> IO Event
eventFromPtr p = do
  t <- mkForeign (FFun "irEventType" [FPtr] FInt) p
  c <- mkForeign (FFun "irEventKeyCode" [FPtr] FInt) p
  alt <- map (/= 0) (mkForeign (FFun "irEventKeyAlternate" [FPtr] FInt) p)
  cmd <- map (/= 0) (mkForeign (FFun "irEventKeyCommand" [FPtr] FInt) p)
  ctrl <- map (/= 0) (mkForeign (FFun "irEventKeyControl" [FPtr] FInt) p)
  shift <- map (/= 0) (mkForeign (FFun "irEventKeyShift" [FPtr] FInt) p)
  mkForeign (FFun "irEventFree" [FPtr] FUnit) p
  return (case t of
    0 => KeyEvent (MkKey c alt cmd ctrl shift)
    1 => RefreshEvent
    _ => IgnoredEvent)
