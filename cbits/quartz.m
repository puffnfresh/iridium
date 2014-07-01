#include <Cocoa/Cocoa.h>
#include "ir.h"
#include "quartz.h"
#include "quartz_stubs.h"

// TODO: Add an NSApplicationDefined event for when a window or space is changed.

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

      if ([subrole isEqualToString:(__bridge NSString *)kAXStandardWindowSubrole])
        NSLog(@"%@ %@ %f %f %f %f", title, subrole, point.x, point.x, size.width, size.height);
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

void quartzBlock() {
  NSEvent *event = [NSApp nextEventMatchingMask:NSAnyEventMask untilDate:[NSDate distantFuture] inMode:NSDefaultRunLoopMode dequeue:YES];
  switch([event type]) {
  case NSKeyDown:
    NSLog(@"KEY DOWN %d %ld", [event keyCode], [event modifierFlags]);
    break;
  default:
    NSLog(@"Unknown event %@", event);
  }
}
