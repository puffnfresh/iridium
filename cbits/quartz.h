typedef struct QuartzWindows {
  int length;
  int *wids;
  int focused;
} QuartzWindows;

typedef struct QuartzScreens {
  int length;
  IRFrame *frames;
} QuartzScreens;

typedef enum QuartzEventType {
  QuartzApplicationDeactivateEvent
} QuartzEventType;

int quartzInit();
int quartzSpacesCount();
void *quartzEvent();
void *quartzScreens();
void quartzScreensFree(QuartzScreens *screens);
int quartzScreensLength(QuartzScreens *screens);
void *quartzScreensFrame(QuartzScreens *screens, int index);
void quartzGrabKey(int keyCode, int alternative, int command, int control, int shift);

void quartzWindowsFree(QuartzWindows *windows);
void *quartzWindows();
int quartzWindowId(QuartzWindows *windows, int index);
int quartzWindowsLength(QuartzWindows *windows);
int quartzWindowsFocusedId(QuartzWindows *windows);

void quartzWindowSetRect(int wid, double x, double y, double w, double h);
void quartzWindowSetFocus(int wid);
