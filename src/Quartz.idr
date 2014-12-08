module Main

import IR
import IR.Event
import IR.Layout
import IR.Lens
import IR.StackSet

%flag C "Cocoa"
%flag C "-framework"
%include C "cbits/quartz.h"
%link C "src/quartz.o"
%include C "cbits/ir.h"
%link C "src/ir.o"

%default total

partial
putErrLn : String -> IO ()
putErrLn s = fwrite stderr (s ++ "\n")

quartzInit : IO Bool
quartzInit = map (/= 0) (mkForeign (FFun "quartzInit" [] FInt))

quartzSpacesCount : IO Int
quartzSpacesCount = mkForeign (FFun "quartzSpacesCount" [] FInt)

QuartzWindow : Type
QuartzWindow = Int

QuartzSpace : Type
QuartzSpace = Int

QuartzState : Type
QuartzState = IRState QuartzWindow QuartzSpace

QUARTZ : EFFECT
QUARTZ = IR QuartzWindow QuartzSpace

quartzGetWindows : IO (List QuartzWindow, QuartzWindow)
quartzGetWindows = do
  p <- mkForeign (FFun "quartzWindows" [] FPtr)
  l <- mkForeign (FFun "quartzWindowsLength" [FPtr] FInt) p
  wids <- traverse (\a => mkForeign (FFun "quartzWindowId" [FPtr, FInt] FInt) p a) [0..l-1]
  focused <- mkForeign (FFun "quartzWindowsFocusedId" [FPtr] FInt) p
  mkForeign (FFun "quartzWindowsFree" [FPtr] FUnit) p
  return (wids, focused)

quartzTileWindow : QuartzWindow -> Rectangle -> IO ()
quartzTileWindow wid r =
  mkForeign (FFun "quartzWindowSetRect" [FInt, FFloat, FFloat, FFloat, FFloat] FUnit) wid (rectX r) (rectY r) (rectW r) (rectH r)

quartzFocusWindow : QuartzWindow -> IO ()
quartzFocusWindow wid =
  mkForeign (FFun "quartzWindowSetFocus" [FInt] FUnit) wid

quartzRefresh : QuartzState -> IO QuartzState
quartzRefresh s = do
  (wids, focused) <- quartzGetWindows
  let stack = workspaceStack' . screenWorkspace' . stackSetCurrent' . irStateStackSet' ^$ s
  let wids' = fromMaybe [] (map (\s => toList (integrate s)) stack)
  let deleted = wids' \\ wids
  let inserted = wids \\ wids'
  return (irStateStackSet' ^%= (\ss => focusWindow focused (foldr insertUp (foldr delete ss deleted) inserted)) $ s)

quartzGetFrame : Ptr -> Int -> IO Rectangle
quartzGetFrame p i = do
  r <- mkForeign (FFun "quartzScreensFrame" [FPtr, FInt] FPtr) p i
  x <- mkForeign (FFun "irFrameX" [FPtr] FFloat) r
  y <- mkForeign (FFun "irFrameY" [FPtr] FFloat) r
  w <- mkForeign (FFun "irFrameW" [FPtr] FFloat) r
  h <- mkForeign (FFun "irFrameH" [FPtr] FFloat) r
  return (MkRectangle x y w h)

quartzGetFrames : IO (n ** Vect (S n) Rectangle)
quartzGetFrames = do
  p <- mkForeign (FFun "quartzScreens" [] FPtr)
  l <- mkForeign (FFun "quartzScreensLength" [FPtr] FInt) p
  mainFrame <- quartzGetFrame p 0
  frames <- traverse (quartzGetFrame p) [1..l-1]
  mkForeign (FFun "quartzScreensFree" [FPtr] FUnit) p
  return (length frames ** mainFrame :: fromList frames)

quartzGrabKeys : List Key -> IO ()
quartzGrabKeys keys =
  let grabKey = \key => do
        let c = keyCode' ^$ key
        let f = \b => if b then 1 else 0
        let alt = f $ keyHasAlt' ^$ key
        let cmd = f $ keyHasCmd' ^$ key
        let ctrl = f $ keyHasCtrl' ^$ key
        let shift = f $ keyHasShift' ^$ key
        mkForeign (FFun "quartzGrabKey" [FInt, FInt, FInt, FInt, FInt] FUnit) c alt cmd ctrl shift
  in traverse_ grabKey keys

instance Handler (IREffect QuartzWindow QuartzSpace) IO where
  handle () GetEvent k = do
    p <- mkForeign (FFun "quartzEvent" [] FPtr)
    e <- eventFromPtr p
    k e ()
  handle () (GrabKeys keys) k = do
    quartzGrabKeys keys
    k () ()
  handle () (RefreshState s) k = do
    s' <- quartzRefresh s
    k s' ()
  handle () (TileWindow wid r) k = do
    quartzTileWindow wid r
    k () ()
  handle () (SetFocus wid) k = do
    quartzFocusWindow wid
    k () ()
  handle () GetWindows k = do
    (wids, _) <- quartzGetWindows
    k wids ()
  handle () GetFrames k = do
    f <- quartzGetFrames
    k f ()

initialQuartzState : IO (IRState QuartzWindow QuartzSpace)
initialQuartzState = do
  (_ ** frame :: _) <- quartzGetFrames
  (wids, _) <- quartzGetWindows
  let workspace : Workspace QuartzWindow = foldr manage (MkWorkspace (choose [columnLayout, mirrorLayout columnLayout, fullLayout]) Nothing) wids
  return (MkIRState (MkStackSet (MkScreen workspace 0 frame) [] []))

SpacebarKeyCode : Int
SpacebarKeyCode = 0x31

JKeyCode : Int
JKeyCode = 0x26

KKeyCode : Int
KKeyCode = 0x28

CmdOptionSpacebar : Key
CmdOptionSpacebar = MkKey SpacebarKeyCode True True False False

CmdOptionJKey : Key
CmdOptionJKey = MkKey JKeyCode True True False False

CmdOptionShiftJKey : Key
CmdOptionShiftJKey = MkKey JKeyCode True True False True

CmdOptionKKey : Key
CmdOptionKKey = MkKey KKeyCode True True False False

CmdOptionShiftKKey : Key
CmdOptionShiftKKey = MkKey KKeyCode True True False True

quartzConf : IRConf QuartzWindow QuartzSpace
quartzConf =
  MkIRConf (fromList [
    (CmdOptionSpacebar, update nextLayout >>= \_ => refresh)
  , (CmdOptionJKey, windows focusDown)
  , (CmdOptionKKey, windows focusUp)
  , (CmdOptionShiftJKey, windows swapDown)
  , (CmdOptionShiftKKey, windows swapUp)
  ])

partial
main : IO ()
main = do
  putStrLn "iridium started"
  a <- quartzInit
  if not a
  then do
    putErrLn "iridium doesn't have Accessibility permission."
    putErrLn "You can enable this under Privacy in Security & Privacy in System Preferences."
  else runInit [(), !initialQuartzState, quartzConf] runIR
