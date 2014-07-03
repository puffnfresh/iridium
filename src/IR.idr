module IR

import Effect.State
import IR.Event

%default total

record Rectangle : Type where
  MkRectangle : (rectX : Float) ->
                (rectY : Float) ->
                (rectW : Float) ->
                (rectH : Float) ->
                Rectangle

record Stack : Type -> Type where
  MkStack : (stackFocus : wid) ->
            (stackUp : List wid) ->
            (stackDown : List wid) ->
            Stack wid

record Workspace : Type -> Type where
  MkWorkspace : (workspaceStack : Maybe (Stack wid)) ->
                Workspace wid

record Screen : Type -> Type -> Type where
  MkScreen : (screenWorkspace : Workspace wid) ->
             (screenId : sid) ->
             (screenDetail : Rectangle) ->
             Screen wid sid

record StackSet : Type -> Type -> Type where
  MkStackSet : (stackSetCurrent : Screen wid sid) ->
               (stackSetVisible : List (Screen wid sid)) ->
               (stackSetHidden  : List (Workspace wid)) ->
               StackSet wid sid

record IRState : Type -> Type -> Type where
  MkIRState : (irStateStackSet : StackSet wid sid) ->
              IRState wid sid

data IREffect : Type -> Effect where
  GetEvent : { () } (IREffect wid) Event
  HandleEvent : Event -> { () } (IREffect wid) ()
  GetFrames : { () } (IREffect wid) (n ** Vect (S n) Rectangle)
  GetWindows : { () } (IREffect wid) (List wid)
  TileWindow : wid -> Rectangle -> { () } (IREffect wid) ()

IR : Type -> EFFECT
IR wid = MkEff () (IREffect wid)

getEvent : { [IR wid] } Eff e Event
getEvent = call GetEvent

handleEvent : Event -> { [IR wid] } Eff e ()
handleEvent e = call (HandleEvent e)

getFrames : { [IR wid] } Eff e (n ** Vect (S n) Rectangle)
getFrames = call GetFrames

getWindows : { [IR wid] } Eff e (List wid)
getWindows = call GetWindows

tileWindow : wid -> Rectangle -> { [IR wid] } Eff e ()
tileWindow wid rect = call (TileWindow wid rect)

partial
runIR : { [IR wid, STATE (IRState a b)] } Eff IO ()
runIR = do
  e <- getEvent
  handleEvent e
  runIR
