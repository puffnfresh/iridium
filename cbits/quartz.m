#include <Cocoa/Cocoa.h>
#include "ir.h"
#include "quartz.h"
#include "quartz_stubs.h"

@interface QuartzGrabbedKey : NSObject

@property unsigned short keyCode;
@property NSUInteger modifierFlags;

- (id)initWithKeyCode:(unsigned short)aKeyCode modifierFlags:(NSUInteger)aModifierFlags;

@end

@implementation QuartzGrabbedKey

- (id)initWithKeyCode:(unsigned short)aKeyCode modifierFlags:(NSUInteger)aModifierFlags {
  if (self = [super init]) {
    self.keyCode = aKeyCode;
    self.modifierFlags = aModifierFlags;
  }
  return self;
}

@end

NSMutableArray *quartzGrabbedKeys;

void quartzPostDeactivateEvent() {
  [NSApp postEvent:[NSEvent otherEventWithType:NSApplicationDefined location:NSMakePoint(0,0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:QuartzApplicationDeactivateEvent data1:0 data2:0] atStart:NO];
}

void quartzApplicationDeactivateCallback(NSNotification *notification) {
  quartzPostDeactivateEvent();
}

CGEventRef quartzEventCallback(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *ref) {
  switch (type) {
  case kCGEventKeyDown:
    {
      NSEvent *ee = [NSEvent eventWithCGEvent:event];
      for (QuartzGrabbedKey *grabbedKey in quartzGrabbedKeys) {
        unsigned long modifierFlags = [ee modifierFlags];
        if (grabbedKey.keyCode == [ee keyCode] && (grabbedKey.modifierFlags & modifierFlags) == grabbedKey.modifierFlags) {
          [NSApp postEvent:ee atStart:NO];
          return NULL;
        }
      }
    }
    break;
  default:
    [NSApp postEvent:[NSEvent eventWithCGEvent:event] atStart:NO];
  }
  return event;
}

void quartzObserverCallback(AXObserverRef observer, AXUIElementRef element, CFStringRef notificationName, void *data) {
  quartzPostDeactivateEvent();
}

void quartzAttachEvents() {
  CGEventMask eventMask = CGEventMaskBit(kCGEventKeyDown);
  CFMachPortRef eventTap = CGEventTapCreate(kCGHIDEventTap, kCGHeadInsertEventTap, kCGEventTapOptionDefault, eventMask, quartzEventCallback, NULL);
  CFRunLoopSourceRef runLoopSource = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, eventTap, 0);
  CFRunLoopAddSource(CFRunLoopGetCurrent(), runLoopSource, kCFRunLoopDefaultMode);
  CGEventTapEnable(eventTap, YES);
  CFRelease(runLoopSource);
  CFRelease(eventTap);

  for (NSRunningApplication *runningApp in [[NSWorkspace sharedWorkspace] runningApplications]) {
    AXUIElementRef axElementRef = AXUIElementCreateApplication(runningApp.processIdentifier);

    AXObserverRef axObserver;
    AXObserverCreate(runningApp.processIdentifier, quartzObserverCallback, &axObserver);

    AXObserverAddNotification(axObserver, axElementRef, kAXWindowMiniaturizedNotification, NULL);
    AXObserverAddNotification(axObserver, axElementRef, kAXWindowMovedNotification, NULL);
    AXObserverAddNotification(axObserver, axElementRef, kAXWindowResizedNotification, NULL);
    //AXObserverAddNotification(axObserver, axElementRef, kAXFocusedWindowChangedNotification, NULL);

    CFRunLoopAddSource(CFRunLoopGetCurrent(), AXObserverGetRunLoopSource(axObserver), kCFRunLoopDefaultMode);
  }

  NSNotificationCenter *center =  [[NSWorkspace sharedWorkspace] notificationCenter];
  [center addObserverForName:NSWorkspaceDidDeactivateApplicationNotification object:nil queue:nil usingBlock:^(NSNotification *notiication) {
    quartzApplicationDeactivateCallback(notiication);
  }];
}

int quartzInit() {
  [NSApplication sharedApplication];
  [NSApp finishLaunching];
  Boolean success = AXIsProcessTrusted();

  quartzGrabbedKeys = [NSMutableArray arrayWithCapacity:0];

  // [NSScreen screensHaveSeparateSpaces];

  if (success) {
    quartzAttachEvents();
  }

  return success;
}

int quartzSpacesCount() {
  CFArrayRef spaces = CGSCopySpaces(CGSDefaultConnection, kCGSSpaceAll);
  int count = CFArrayGetCount(spaces) - 1;
  CFRelease(spaces);
  return count;
}

int quartzCurrentSpaceId() {
  CFArrayRef currentSpace = CGSCopySpaces(CGSDefaultConnection, kCGSSpaceCurrent);
  uint64_t currentSpaceId = [(id)CFArrayGetValueAtIndex(currentSpace, 0) intValue];
  CFRelease(currentSpace);
  return currentSpaceId;
}

void *quartzFindWindow(int wid) {
  for (NSRunningApplication *runningApp in [[NSWorkspace sharedWorkspace] runningApplications]) {
    AXUIElementRef axElementRef = AXUIElementCreateApplication(runningApp.processIdentifier);
    CFArrayRef windowsArrayRef;
    AXUIElementCopyAttributeValues(axElementRef, kAXWindowsAttribute, 0, 100, &windowsArrayRef);

    NSArray *windowRefs = CFBridgingRelease(windowsArrayRef);
    for (NSUInteger index = 0; index < windowRefs.count; ++index) {
      AXUIElementRef windowRef = (__bridge AXUIElementRef)windowRefs[index];

      CGWindowID windowRefId;
      _AXUIElementGetWindow(windowRef, &windowRefId);

      // TODO: Should we ignore Dash windows?

      if (wid == windowRefId) {
        CFRelease(axElementRef);
        return (void *)windowRef;
      }
    }

    CFRelease(axElementRef);
  }

  return NULL;
}

void quartzWindowSetRect(int wid, double x, double y, double w, double h) {
  AXUIElementRef window = quartzFindWindow(wid);

  if (window == NULL)
    return;

  CGPoint point = CGPointMake(x, y);
  AXValueRef pointRef = AXValueCreate(kAXValueCGPointType, &point);
  AXUIElementSetAttributeValue(window, kAXPositionAttribute, pointRef);
  CFRelease(pointRef);

  CGSize size = CGSizeMake(w, h);
  AXValueRef sizeRef = AXValueCreate(kAXValueCGSizeType, &size);
  AXUIElementSetAttributeValue(window, kAXSizeAttribute, sizeRef);
  CFRelease(sizeRef);

  CFRelease(window);
}

void quartzWindowSetFocus(int wid) {
  AXUIElementRef window = quartzFindWindow(wid);

  if (window == NULL)
    return;

  AXUIElementSetAttributeValue(window, kAXMainAttribute, kCFBooleanTrue);

  CFTypeRef applicationRef;
  AXUIElementCopyAttributeValue(window, kAXParentAttribute, &applicationRef);
  AXUIElementSetAttributeValue(applicationRef, kAXFrontmostAttribute, kCFBooleanTrue);
  CFRelease(applicationRef);

  CFRelease(window);
}

void quartzWindowFree(void *window) {
  CFRelease(window);
}

#define QUARTZ_WINDOWS_LENGTH 100

void *quartzWindows() {
  QuartzWindows *windows = malloc(sizeof(QuartzWindows));
  windows->length = 0;
  windows->wids = malloc(sizeof(int) * QUARTZ_WINDOWS_LENGTH);

  for (NSRunningApplication *runningApp in [[NSWorkspace sharedWorkspace] runningApplications]) {
    AXUIElementRef axElementRef = AXUIElementCreateApplication(runningApp.processIdentifier);
    CFArrayRef windowsArrayRef;
    AXUIElementCopyAttributeValues(axElementRef, kAXWindowsAttribute, 0, QUARTZ_WINDOWS_LENGTH, &windowsArrayRef);

    CFBooleanRef frontMostRef;
    AXUIElementCopyAttributeValue(axElementRef, kAXFrontmostAttribute, (const void **)&frontMostRef);

    NSArray *windowRefs = CFBridgingRelease(windowsArrayRef);
    for (NSUInteger index = 0; index < windowRefs.count; ++index) {
      AXUIElementRef windowRef = (__bridge AXUIElementRef)windowRefs[index];

      CFTypeRef subroleRef;
      AXUIElementCopyAttributeValue(windowRef, kAXSubroleAttribute, &subroleRef);
      NSString *subrole = CFBridgingRelease(subroleRef);

      CGWindowID wid;
      _AXUIElementGetWindow(windowRef, &wid);

      CFBooleanRef mainRef;
      AXUIElementCopyAttributeValue(windowRef, kAXMainAttribute, (const void **)&mainRef);

      if (CFBooleanGetValue(frontMostRef) && CFBooleanGetValue(mainRef)) {
        windows->focused = wid;
      }

      if ([subrole isEqualToString:(__bridge NSString *)kAXStandardWindowSubrole]) {
        if (windows->length < QUARTZ_WINDOWS_LENGTH) {
          windows->wids[windows->length] = wid;
          windows->length++;
        }
      }
    }

    CFRelease(axElementRef);
  }

  return windows;
}

void quartzWindowsFree(QuartzWindows *windows) {
  free(windows->wids);
  free(windows);
}

int quartzWindowId(QuartzWindows *windows, int index) {
  return windows->wids[index];
}

int quartzWindowsLength(QuartzWindows *windows) {
  return windows->length;
}

int quartzWindowsFocusedId(QuartzWindows *windows) {
  return windows->focused;
}

void *quartzScreens() {
  NSArray *frames = [NSScreen screens];

  QuartzScreens *screens = malloc(sizeof(QuartzScreens));

  NSUInteger count = [frames count];
  screens->length = count;
  screens->frames = malloc(sizeof(IRFrame) * count);

  unsigned int i = 0;
  for (NSScreen *screen in frames) {
    NSRect frame = [screen frame];
    NSRect visibleFrame = [screen visibleFrame];

    // Menu bar is always up the top.
    screens->frames[i].y = frame.size.height - visibleFrame.size.height;
    screens->frames[i].x = visibleFrame.origin.x;
    screens->frames[i].w = visibleFrame.size.width;
    screens->frames[i].h = visibleFrame.size.height;

    i++;
  }

  return screens;
}

int quartzScreensLength(QuartzScreens *screens) {
  return screens->length;
}

void *quartzScreensFrame(QuartzScreens *screens, int index) {
  return &screens->frames[index];
}

void quartzScreensFree(QuartzScreens *screens) {
  free(screens->frames);
  free(screens);
}

void quartzGrabKey(int keyCode, int alternative, int command, int control, int shift) {
  NSUInteger modifierFlags = 0;
  if (alternative) {
    modifierFlags |= NSAlternateKeyMask;
  }
  if (command) {
    modifierFlags |= NSCommandKeyMask;
  }
  if (control) {
    modifierFlags |= NSControlKeyMask;
  }
  if (shift) {
    modifierFlags |= NSShiftKeyMask;
  }
  QuartzGrabbedKey *key = [[QuartzGrabbedKey alloc] initWithKeyCode:keyCode modifierFlags:modifierFlags];
  [quartzGrabbedKeys addObject:key];
}

void *quartzEvent() {
  NSEvent *event = [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES];
  IREvent *irEvent = malloc(sizeof(irEvent));

  switch ([event type]) {
  case NSKeyDown:
    irEvent->type = IRKeyDownEventType;
    irEvent->keyCode = [event keyCode];
    irEvent->modifiers = 0;
    if ([event modifierFlags] & NSAlternateKeyMask) {
      irEvent->modifiers |= IRKeyModifierAlternate;
    }
    if ([event modifierFlags] & NSCommandKeyMask) {
      irEvent->modifiers |= IRKeyModifierCommand;
    }
    if ([event modifierFlags] & NSControlKeyMask) {
      irEvent->modifiers |= IRKeyModifierControl;
    }
    if ([event modifierFlags] & NSShiftKeyMask) {
      irEvent->modifiers |= IRKeyModifierShift;
    }
    break;
  case NSApplicationDefined:
    switch ([event subtype]) {
    case QuartzApplicationDeactivateEvent:
      // Focused application changed.
      irEvent->type = IRRefreshEventType;
      break;
    }
    break;
  default:
    irEvent->type = IRIgnoredEventType;
    break;
  }

  return irEvent;
}
