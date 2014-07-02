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

typedef enum {
  IRKeyModifierAlternate = 1 << 0,
  IRKeyModifierCommand = 1 << 1,
  IRKeyModifierControl = 1 << 2,
  IRKeyModifierShift = 1 << 3
} IRKeyModifier;

typedef struct {
  IREventType type;
  unsigned short keyCode;
  IRKeyModifier modifiers;
} IREvent;

void irFrameFree(IRFrame *frame);
double irFrameX(IRFrame *frame);
double irFrameY(IRFrame *frame);
double irFrameW(IRFrame *frame);
double irFrameH(IRFrame *frame);

void irEventFree(IREvent *event);
int irEventType(IREvent *event);
int irEventKeyCode(IREvent *event);
int irEventKeyAlternate(IREvent *event);
int irEventKeyCommand(IREvent *event);
int irEventKeyControl(IREvent *event);
int irEventKeyShift(IREvent *event);
