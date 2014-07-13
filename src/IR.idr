module IR

import Control.Monad.Identity
import Data.SortedMap
import Effect.State
import IR.Event
import IR.Lens
import IR.Reader

%default total

record Rectangle : Type where
  MkRectangle : (rectX : Float) ->
                (rectY : Float) ->
                (rectW : Float) ->
                (rectH : Float) ->
                Rectangle

rectX' : Lens Rectangle Float
rectX' = lens (\(MkRectangle x _ _ _) => x) (\x, (MkRectangle _ a b c) => MkRectangle x a b c)

rectY' : Lens Rectangle Float
rectY' = lens (\(MkRectangle _ x _ _) => x) (\x, (MkRectangle a _ b c) => MkRectangle a x b c)

rectW' : Lens Rectangle Float
rectW' = lens (\(MkRectangle _ _ x _) => x) (\x, (MkRectangle a b _ c) => MkRectangle a b x c)

rectH' : Lens Rectangle Float
rectH' = lens (\(MkRectangle _ _ _ x) => x) (\x, (MkRectangle a b c _) => MkRectangle a b c x)

record Stack : Type -> Type where
  MkStack : (stackFocus : wid) ->
            (stackUp : List wid) ->
            (stackDown : List wid) ->
            Stack wid

stackFocus' : Lens (Stack wid) wid
stackFocus' = lens (\(MkStack x _ _) => x) (\x, (MkStack _ a b) => MkStack x a b)

stackUp' : Lens (Stack wid) (List wid)
stackUp' = lens (\(MkStack _ x _) => x) (\x, (MkStack a _ b) => MkStack a x b)

stackDown' : Lens (Stack wid) (List wid)
stackDown' = lens (\(MkStack _ _ x) => x) (\x, (MkStack a b _) => MkStack a b x)

stackLength : Stack wid -> Nat
stackLength (MkStack _ ys zs) = S (length ys + length zs)

LayoutF : Type -> Type
LayoutF wid = Rectangle -> (s : Stack wid) -> Vect (stackLength s) (wid, Rectangle)

record Layout : Type -> Type where
  MkLayout : (layoutPure : LayoutF wid) ->
             (layoutNext : Inf (Layout wid)) ->
             Layout wid

layoutPure' : Lens (Layout wid) (LayoutF wid)
layoutPure' = lens (\(MkLayout x _) => x) (\x, (MkLayout _ a) => MkLayout x a)

layoutNext' : Lens (Layout wid) (Layout wid)
layoutNext' = lens (\(MkLayout _ x) => x) (\x, (MkLayout a _) => MkLayout a x)

record Workspace : Type -> Type where
  MkWorkspace : (workspaceLayout : Layout wid) ->
                (workspaceStack : Maybe (Stack wid)) ->
                Workspace wid

workspaceLayout' : Lens (Workspace wid) (Layout wid)
workspaceLayout' = lens (\(MkWorkspace x _) => x) (\x, (MkWorkspace _ a) => MkWorkspace x a)

workspaceStack' : Lens (Workspace wid) (Maybe (Stack wid))
workspaceStack' = lens (\(MkWorkspace _ x) => x) (\x, (MkWorkspace a _) => MkWorkspace a x)

record Screen : Type -> Type -> Type where
  MkScreen : (screenWorkspace : Workspace wid) ->
             (screenId : sid) ->
             (screenDetail : Rectangle) ->
             Screen wid sid

screenWorkspace' : Lens (Screen wid sid) (Workspace wid)
screenWorkspace' = lens (\(MkScreen x _ _) => x) (\x, (MkScreen _ a b) => MkScreen x a b)

screenDetail' : Lens (Screen wid sid) Rectangle
screenDetail' = lens (\(MkScreen _ _ x) => x) (\x, (MkScreen a b _) => MkScreen a b x)

record StackSet : Type -> Type -> Type where
  MkStackSet : (stackSetCurrent : Screen wid sid) ->
               (stackSetVisible : List (Screen wid sid)) ->
               (stackSetHidden  : List (Workspace wid)) ->
               StackSet wid sid

stackSetCurrent' : Lens (StackSet wid sid) (Screen wid sid)
stackSetCurrent' = lens (\(MkStackSet x _ _) => x) (\x, (MkStackSet _ a b) => MkStackSet x a b)

record IRState : Type -> Type -> Type where
  MkIRState : (irStateStackSet : StackSet wid sid) ->
              IRState wid sid

irStateStackSet' : Lens (IRState wid sid) (StackSet wid sid)
irStateStackSet' = lens (\(MkIRState x) => x) (\x, (MkIRState _) => MkIRState x)

data IREffect : Type -> Type -> Effect where
  GetEvent : { () } (IREffect wid sid) Event
  GetFrames : { () } (IREffect wid sid) (n ** Vect (S n) Rectangle)
  GetWindows : { () } (IREffect wid sid) (List wid)
  GrabKeys : List Key -> { () } (IREffect wid sid) ()
  RefreshState : IRState wid sid -> { () } (IREffect wid sid) (IRState wid sid)
  SetFocus : wid -> { () } (IREffect wid sid) ()
  TileWindow : wid -> Rectangle -> { () } (IREffect wid sid) ()

IR : Type -> Type -> EFFECT
IR wid sid = MkEff () (IREffect wid sid)

record IRConf : Type -> Type -> Type where
  MkIRConf : (irConfKeyActions : SortedMap Key ({ [IR wid sid, STATE (IRState wid sid)] } Eff ())) ->
             IRConf wid sid

irConfKeyActions' : Lens (IRConf wid sid) (SortedMap Key ({ [IR wid sid, STATE (IRState wid sid)] } Eff ()))
irConfKeyActions' = lens (\(MkIRConf x) => x) (\x, (MkIRConf _) => MkIRConf x)

getEvent : { [IR wid sid] } Eff Event
getEvent = call GetEvent

grabKeys : List Key -> { [IR wid sid] } Eff ()
grabKeys k = call (GrabKeys k)

setFocus : wid -> { [IR wid sid] } Eff ()
setFocus wid = call (SetFocus wid)

tileWindow : wid -> Rectangle -> { [IR wid sid] } Eff ()
tileWindow wid rect = call (TileWindow wid rect)

runLayout : { [IR wid sid, STATE (IRState wid sid)] } Eff ()
runLayout = do
  s <- get
  let screen = stackSetCurrent' . irStateStackSet' ^$ s
  let frame : Rectangle = screenDetail' ^$ screen
  -- Idris bug: maybe doesn't work here, have to use fromMaybe
  let maybeStack = workspaceStack' . screenWorkspace' ^$ screen
  let l = layoutPure' . workspaceLayout' . screenWorkspace' ^$ screen
  -- Idris bug: there's a useless `Applicative m` constraint, supply Identity to get this to compile:
  -- https://github.com/idris-lang/Idris-dev/pull/1364
  case maybeStack of
    Just stack => do
      mapVE (uncurry tileWindow) (l frame stack)
      setFocus (stackFocus' ^$ stack)
      return ()
    Nothing => return ()

refresh : { [IR wid sid, STATE (IRState wid sid)] } Eff ()
refresh = do
  s <- get
  s' <- call (RefreshState s)
  put s'
  runLayout

getFrames : { [IR wid sid] } Eff (n ** Vect (S n) Rectangle)
getFrames = call GetFrames

getWindows : { [IR wid sid] } Eff (List wid)
getWindows = call GetWindows

nextLayout : IRState wid sid -> IRState wid sid
nextLayout = workspaceLayout' . screenWorkspace' . stackSetCurrent' . irStateStackSet' ^%= getL layoutNext'

handleEvent : Event -> { [IR wid sid, STATE (IRState wid sid), READER (IRConf wid sid)] } Eff ()
handleEvent RefreshEvent = refresh
handleEvent (KeyEvent key) = do
  conf <- ask
  -- Idris bug: can't inline this let
  let m = lookup key (irConfKeyActions' ^$ conf)
  fromMaybe (return ()) m
  return ()
handleEvent IgnoredEvent = return ()

partial
runIR' : { [IR wid sid, STATE (IRState wid sid), READER (IRConf wid sid)] } Eff ()
runIR' = do
  e <- getEvent
  handleEvent e
  runIR'

partial
runIR : { [IR wid sid, STATE (IRState wid sid), READER (IRConf wid sid)] } Eff ()
runIR = do
  conf <- ask
  grabKeys (map fst (toList (irConfKeyActions' ^$ conf)))
  runLayout
  runIR'
