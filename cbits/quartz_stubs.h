#include <Cocoa/Cocoa.h>

typedef void *CGSConnectionID;
extern CGSConnectionID _CGSDefaultConnection(void);
#define CGSDefaultConnection _CGSDefaultConnection()

typedef enum _CGSSpaceSelector {
  kCGSSpaceCurrent = 5,
  kCGSSpaceOther = 6,
  kCGSSpaceAll = 7
} CGSSpaceSelector;

extern CFArrayRef CGSCopySpaces(const CGSConnectionID, CGSSpaceSelector);

// Private API: Stable identifier for a window.
extern AXError _AXUIElementGetWindow(AXUIElementRef, CGWindowID*);
