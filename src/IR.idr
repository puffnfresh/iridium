module IR

import Effect.State
import IR.Event
import IR.Lens

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

record Workspace : Type -> Type where
  MkWorkspace : (workspaceStack : Maybe (Stack wid)) ->
                Workspace wid

workspaceStack' : Lens (Workspace wid) (Maybe (Stack wid))
workspaceStack' = lens (\(MkWorkspace x) => x) (\x, (MkWorkspace _) => MkWorkspace x)

record Screen : Type -> Type -> Type where
  MkScreen : (screenWorkspace : Workspace wid) ->
             (screenId : sid) ->
             (screenDetail : Rectangle) ->
             Screen wid sid

screenWorkspace' : Lens (Screen wid sid) (Workspace wid)
screenWorkspace' = lens (\(MkScreen x _ _) => x) (\x, (MkScreen _ a b) => MkScreen x a b)

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
  HandleEvent : IRState wid sid -> Event -> { () } (IREffect wid sid) (IRState wid sid)
  GetFrames : { () } (IREffect wid sid) (n ** Vect (S n) Rectangle)
  GetWindows : { () } (IREffect wid sid) (List wid)
  TileWindow : wid -> Rectangle -> { () } (IREffect wid sid) ()

IR : Type -> Type -> EFFECT
IR wid sid = MkEff () (IREffect wid sid)

getEvent : { [IR wid sid] } Eff e Event
getEvent = call GetEvent

handleEvent : IRState wid sid -> Event -> { [IR wid sid] } Eff e (IRState wid sid)
handleEvent s e = call (HandleEvent s e)

getFrames : { [IR wid sid] } Eff e (n ** Vect (S n) Rectangle)
getFrames = call GetFrames

getWindows : { [IR wid sid] } Eff e (List wid)
getWindows = call GetWindows

tileWindow : wid -> Rectangle -> { [IR wid sid] } Eff e ()
tileWindow wid rect = call (TileWindow wid rect)

partial
runIR : { [IR wid sid, STATE (IRState wid sid)] } Eff IO ()
runIR = do
  e <- getEvent
  s <- get
  s' <- handleEvent s e
  put s'
  runIR
