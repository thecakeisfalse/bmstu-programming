#include <ntddk.h>

#define NDEBUG
#include <debug.h>

// Doesn't work, but nobody cares
VOID NTAPI DriverUnload(IN PDRIVER_OBJECT DriverObject) {
  DPRINT1("Unloaded\n");
}

NTSTATUS NTAPI DriverEntry(IN PDRIVER_OBJECT DriverObject,
                           IN PUNICODE_STRING RegisterPath) {
  DriverObject->DriverUnload = DriverUnload;
  DPRINT1("Last Name First Name\n");
  return STATUS_SUCCESS;
}
