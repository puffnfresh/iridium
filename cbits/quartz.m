#include <Cocoa/Cocoa.h>
#include "ir.h"
#include "quartz.h"
#include "quartz_stubs.h"

void quartzApplicationDeactivateCallback(NSNotification *notification) {
  [NSApp postEvent:[NSEvent otherEventWithType:NSApplicationDefined location:NSMakePoint(0,0) modifierFlags:0 timestamp:0 windowNumber:0 context:nil subtype:QuartzApplicationDeactivateEvent data1:0 data2:0] atStart:NO];
}

CGEventRef quartzEventCallback(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *ref) {
  [NSApp postEvent:[NSEvent eventWithCGEvent:event] atStart:NO];
  return event;
}

void quartzAttachEvents() {
  CGEventMask eventMask = CGEventMaskBit(kCGEventKeyDown);
  CFMachPortRef eventTap = CGEventTapCreate(kCGHIDEventTap, kCGHeadInsertEventTap, kCGEventTapOptionDefault, eventMask, quartzEventCallback, NULL);
  CFRunLoopSourceRef runLoopSource = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, eventTap, 0);
  CFRunLoopAddSource(CFRunLoopGetCurrent(), runLoopSource, kCFRunLoopDefaultMode);
  CGEventTapEnable(eventTap, YES);
  CFRelease(runLoopSource);
  CFRelease(eventTap);

  NSNotificationCenter *center =  [[NSWorkspace sharedWorkspace] notificationCenter];
  [center addObserverForName:NSWorkspaceDidDeactivateApplicationNotification object:nil queue:nil usingBlock:^(NSNotification *notiication) {
    quartzApplicationDeactivateCallback(notiication);
  }];
}

Boolean quartzInit() {
  [NSApplication sharedApplication];
  [NSApp finishLaunching];
  Boolean success = AXIsProcessTrusted();

  for (NSRunningApplication *runningApp in [[NSWorkspace sharedWorkspace] runningApplications]) {
    AXUIElementRef axElementRef = AXUIElementCreateApplication(runningApp.processIdentifier);
    CFArrayRef windowsArrayRef;
    AXUIElementCopyAttributeValues(axElementRef, kAXWindowsAttribute, 0, 100, &windowsArrayRef);

    NSArray *windowRefs = CFBridgingRelease(windowsArrayRef);
    for (NSUInteger index = 0; index < windowRefs.count; ++index) {
      AXUIElementRef windowRef = (__bridge AXUIElementRef)windowRefs[index];

      CFTypeRef pointRef;
      AXUIElementCopyAttributeValue(windowRef, kAXPositionAttribute, &pointRef);
      CGPoint point;
      AXValueGetValue(pointRef, kAXValueCGPointType, &point);

      CFTypeRef sizeRef;
      AXUIElementCopyAttributeValue(windowRef, kAXSizeAttribute, &sizeRef);
      CGSize size;
      AXValueGetValue(sizeRef, kAXValueCGSizeType, &size);

      CFTypeRef titleRef;
      AXUIElementCopyAttributeValue(windowRef, kAXTitleAttribute, &titleRef);
      NSString *title = CFBridgingRelease(titleRef);

      CFTypeRef subroleRef;
      AXUIElementCopyAttributeValue(windowRef, kAXSubroleAttribute, &subroleRef);
      NSString *subrole = CFBridgingRelease(subroleRef);

      CGWindowID wid;
      _AXUIElementGetWindow(windowRef, &wid);

      if ([subrole isEqualToString:(__bridge NSString *)kAXStandardWindowSubrole])
        NSLog(@"%d %@ %@ %f %f %f %f", wid, title, subrole, point.x, point.x, size.width, size.height);
    }

    CFRelease(axElementRef);
  }

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

uint64_t quartzCurrentSpaceId() {
  CFArrayRef currentSpace = CGSCopySpaces(CGSDefaultConnection, kCGSSpaceCurrent);
  uint64_t currentSpaceId = [(id)CFArrayGetValueAtIndex(currentSpace, 0) intValue];
  CFRelease(currentSpace);
  return currentSpaceId;
}

void *quartzMainFrame() {
    NSRect frame = [[NSScreen mainScreen] frame];
    NSRect visibleFrame = [[NSScreen mainScreen] visibleFrame];
    IRFrame *irFrame = malloc(sizeof(IRFrame));

    // Menu bar is always up the top.
    irFrame->y = frame.size.height - visibleFrame.size.height;
    irFrame->x = visibleFrame.origin.x;
    irFrame->w = visibleFrame.size.width;
    irFrame->h = visibleFrame.size.height;

    return irFrame;
}

void *quartzEvent() {
  NSEvent *event = [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES];
  IREvent *irEvent = malloc(sizeof(irEvent));

  switch ([event type]) {
  case NSKeyDown:
    irEvent->type = IRKeyDownEventType;
    irEvent->keyCode = [event keyCode];
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
