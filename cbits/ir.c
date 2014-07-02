#include <stdlib.h>
#include "ir.h"

void irFrameFree(IRFrame *frame) {
  free(frame);
}

double irFrameX(IRFrame *frame) {
  return frame->x;
}

double irFrameY(IRFrame *frame) {
  return frame->y;
}

double irFrameW(IRFrame *frame) {
  return frame->w;
}

double irFrameH(IRFrame *frame) {
  return frame->h;
}

void irEventFree(IREvent *event) {
  free(event);
}

int irEventType(IREvent *event) {
  return event->type;
}

int irEventKeyCode(IREvent *event) {
  return event->keyCode;
}

int irEventKeyAlternate(IREvent *event) {
  return event->modifiers & IRKeyModifierAlternate;
}

int irEventKeyCommand(IREvent *event) {
  return event->modifiers & IRKeyModifierCommand;
}

int irEventKeyControl(IREvent *event) {
  return event->modifiers & IRKeyModifierControl;
}

int irEventKeyShift(IREvent *event) {
  return event->modifiers & IRKeyModifierShift;
}
