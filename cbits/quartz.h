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
int quartzWindowId(QuartzWindows *windows, int index);
int quartzWindowsLength(QuartzWindows *windows);
void *quartzFindWindow(int wid);
void *quartzWindows();
