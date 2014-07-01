typedef struct {
  double x;
  double y;
  double w;
  double h;
} IRFrame;

typedef enum {
  IRKeyDownEventType,
  IRRefreshEventType,
  IRIgnoredEventType
} IREventType;

typedef struct {
  IREventType type;
  unsigned short keyCode;
} IREvent;

void irFrameFree(IRFrame *frame);
double irFrameX(IRFrame *frame);
double irFrameY(IRFrame *frame);
double irFrameW(IRFrame *frame);
double irFrameH(IRFrame *frame);

void irEventFree(IREvent *event);
int irEventType(IREvent *event);
int irEventKeyCode(IREvent *event);
