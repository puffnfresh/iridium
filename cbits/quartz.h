typedef struct QuartzWindows {
  int length;
  int *wids;
} QuartzWindows;

typedef enum {
  QuartzApplicationDeactivateEvent
} QuartzEventType;

int quartzInit();
int quartzSpacesCount();
void *quartzEvent();
void *quartzMainFrame();

void quartzWindowsFree(QuartzWindows *windows);
void *quartzWindows();
int quartzWindowId(QuartzWindows *windows, int index);
int quartzWindowsLength(QuartzWindows *windows);

void quartzWindowSetRect(int wid, double x, double y, double w, double h);
