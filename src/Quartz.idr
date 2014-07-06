module Main

import IR
import IR.Event
import IR.Layout
import IR.Lens
import IR.Workspace

%flag C "-framework Cocoa"
%include C "cbits/quartz.h"
%link C "src/quartz.o"
%include C "cbits/ir.h"
%link C "src/ir.o"

%default total

%assert_total
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

quartzGetWindows : IO (List QuartzWindow)
quartzGetWindows = do
  p <- mkForeign (FFun "quartzWindows" [] FPtr)
  l <- mkForeign (FFun "quartzWindowsLength" [FPtr] FInt) p
  wids <- traverse (\a => mkForeign (FFun "quartzWindowId" [FPtr, FInt] FInt) p a) [0..l-1]
  mkForeign (FFun "quartzWindowsFree" [FPtr] FUnit) p
  return wids

quartzTileWindow : QuartzWindow -> Rectangle -> IO ()
quartzTileWindow wid r =
  mkForeign (FFun "quartzWindowSetRect" [FInt, FFloat, FFloat, FFloat, FFloat] FUnit) wid (rectX r) (rectY r) (rectW r) (rectH r)

quartzUpdate : Rectangle -> Workspace QuartzWindow -> IO ()
quartzUpdate _ (MkWorkspace _ Nothing) =
  return ()
quartzUpdate frame (MkWorkspace l (Just stack)) =
  traverse_ (uncurry quartzTileWindow) (toList ((layoutPure' ^$ l) frame stack))

quartzRefresh : QuartzState -> IO QuartzState
quartzRefresh s = do
  wids <- quartzGetWindows
  let workspace : Workspace QuartzWindow = foldr manage (MkWorkspace (workspaceLayout' . screenWorkspace' . stackSetCurrent' . irStateStackSet' ^$ s) Nothing) wids
  quartzUpdate (screenDetail (stackSetCurrent (irStateStackSet s))) workspace
  return (screenWorkspace' . stackSetCurrent' . irStateStackSet' ^= workspace $ s)

quartzNextLayout : QuartzState -> QuartzState
quartzNextLayout = workspaceLayout' . screenWorkspace' . stackSetCurrent' . irStateStackSet' ^%= getL layoutNext'

quartzGetFrames : IO (n ** Vect (S n) Rectangle)
quartzGetFrames = do
  p <- mkForeign (FFun "quartzMainFrame" [] FPtr)
  x <- mkForeign (FFun "irFrameX" [FPtr] FFloat) p
  y <- mkForeign (FFun "irFrameY" [FPtr] FFloat) p
  w <- mkForeign (FFun "irFrameW" [FPtr] FFloat) p
  h <- mkForeign (FFun "irFrameH" [FPtr] FFloat) p
  mkForeign (FFun "irFrameFree" [FPtr] FUnit) p
  return (0 ** [MkRectangle x y w h])

instance Handler (IREffect QuartzWindow QuartzSpace) IO where
  handle () GetEvent k = do
    p <- mkForeign (FFun "quartzEvent" [] FPtr)
    e <- eventFromPtr p
    k e ()
  handle () (HandleEvent s (KeyEvent key)) k = do
    if (keyHasCmd' ^$ key) && (keyHasAlt' ^$ key)
    then handle () (HandleEvent (quartzNextLayout s) RefreshEvent) k
    else k s ()
  handle () (HandleEvent s RefreshEvent) k = do
    s' <- quartzRefresh s
    k s' ()
  handle () (HandleEvent s IgnoredEvent) k = do
    k s ()
  handle () (TileWindow wid r) k = do
    quartzTileWindow wid r
    k () ()
  handle () GetWindows k = do
    wids <- quartzGetWindows
    k wids ()
  handle () GetFrames k = do
    f <- quartzGetFrames
    k f ()

initialQuartzState : IO (IRState QuartzWindow QuartzSpace)
initialQuartzState = do
  (_ ** frame :: _) <- quartzGetFrames
  wids <- quartzGetWindows
  let workspace : Workspace QuartzWindow = foldr manage (MkWorkspace (choose [columnLayout, mirrorLayout columnLayout, fullLayout]) Nothing) wids
  return (MkIRState (MkStackSet (MkScreen workspace 0 frame) [] []))

partial
main : IO ()
main = do
  putStrLn "iridium started"
  a <- quartzInit
  if not a
  then do
    putErrLn "iridium doesn't have Accessibility permission."
    putErrLn "You can enable this under Privacy in Security & Privacy in System Preferences."
  else do
    s <- initialQuartzState
    runInit [(), s] runIR
