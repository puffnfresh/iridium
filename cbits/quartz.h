typedef enum {
  QuartzApplicationDeactivateEvent
} QuartzEventType;

unsigned char quartzInit();
int quartzSpacesCount();
void *quartzEvent();
void *quartzMainFrame();
