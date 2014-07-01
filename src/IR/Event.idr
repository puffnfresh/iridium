module IR.Event

KeyCode : Type
KeyCode = Int

data Event = KeyEvent KeyCode
           | RefreshEvent
           | IgnoredEvent

eventFromPtr : Ptr -> IO Event
eventFromPtr p = do
  t <- mkForeign (FFun "irEventType" [FPtr] FInt) p
  mkForeign (FFun "irEventFree" [FPtr] FUnit) p
  return (case t of
    1 => KeyEvent 0
    2 => RefreshEvent
    _ => IgnoredEvent)
