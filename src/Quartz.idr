module Main

import Effect.State
import IR
import IR.Event

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
quartzInit = map (== 1) (mkForeign (FFun "quartzInit" [] FInt))

quartzSpacesCount : IO Int
quartzSpacesCount = mkForeign (FFun "quartzSpacesCount" [] FInt)

instance Handler IREffect IO where
  handle () GetEvent k = do
    p <- mkForeign (FFun "quartzEvent" [] FPtr)
    e <- eventFromPtr p
    k e ()
  handle () (HandleEvent IgnoredEvent) k = do
    k () ()
  handle () (HandleEvent _) k = do
    putStrLn "TODO"
    k () ()
  handle () GetFrames k = do
    p <- mkForeign (FFun "quartzMainFrame" [] FPtr)
    x <- mkForeign (FFun "irFrameX" [FPtr] FFloat) p
    y <- mkForeign (FFun "irFrameY" [FPtr] FFloat) p
    w <- mkForeign (FFun "irFrameW" [FPtr] FFloat) p
    h <- mkForeign (FFun "irFrameH" [FPtr] FFloat) p
    mkForeign (FFun "irFrameFree" [FPtr] FUnit) p
    k (0 ** [MkFrame x y w h]) ()

QuartzState : Type
QuartzState = IRState Int Int

instance Default QuartzState where
  default = MkIRState (MkStackSet (MkScreen (MkWorkspace Nothing) 0 (MkFrame 0 0 0 0)) [] [])

initialQuartzState : { [IR, STATE QuartzState] } Eff IO ()
initialQuartzState = do
  (_ ** frames) <- getFrames
  put (MkIRState (MkStackSet (MkScreen (MkWorkspace Nothing) 0 (head frames)) [] []))

partial
main : IO ()
main = do
  a <- quartzInit
  if not a
  then do
    putErrLn "iridium doesn't have Accessibility permission."
    putErrLn "You can enable this under Privacy in Security & Privacy in System Preferences."
  else run $ do
    initialQuartzState
    runIR
