typedef struct {
  double x;
  double y;
  double w;
  double h;
} IRFrame;

void irFrameFree(IRFrame *frame);
double irFrameX(IRFrame *frame);
double irFrameY(IRFrame *frame);
double irFrameW(IRFrame *frame);
double irFrameH(IRFrame *frame);
