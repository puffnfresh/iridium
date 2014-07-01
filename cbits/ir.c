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
