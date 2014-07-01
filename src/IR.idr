module IR

import Effect.State

%default total

record Frame : Type where
  MkFrame : (frameX : Float) ->
            (frameY : Float) ->
            (frameW : Float) ->
            (frameH : Float) ->
            Frame

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
             (screenDetail : Frame) ->
             Screen wid sid

record StackSet : Type -> Type -> Type where
  MkStackSet : (stackSetCurrent : Screen wid sid) ->
               (stackSetVisible : List (Screen wid sid)) ->
               (stackSetHidden  : List (Workspace wid)) ->
               StackSet wid sid

record IRState : Type -> Type -> Type where
  MkIRState : (irStateStackSet : StackSet wid sid) ->
              IRState wid sid

data IREffect : Effect where
  GetEvent : { () } IREffect ()
  GetFrames : { () } IREffect (n ** Vect (S n) Frame)

IR : EFFECT
IR = MkEff () IREffect

getEvent : { [IR] } Eff e ()
getEvent = call GetEvent

getFrames : { [IR] } Eff e (n ** Vect (S n) Frame)
getFrames = call GetFrames

partial
runIR : { [IR, STATE (IRState a b)] } Eff IO ()
runIR = do
  getEvent
  runIR
