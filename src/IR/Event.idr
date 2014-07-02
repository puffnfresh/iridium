module IR.Event

KeyCode : Type
KeyCode = Int

record Key : Type where
  MkKey : (keyCode : KeyCode) ->
          (keyHasAlt : Bool) ->
          (keyHasCmd : Bool) ->
          (keyHasCtrl : Bool) ->
          (keyHasShift : Bool) ->
          Key

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
