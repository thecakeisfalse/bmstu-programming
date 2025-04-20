#include <ntddk.h>
#include <ntifs.h>
#include <ntstrsafe.h>
#include <ndk/exfuncs.h>
#include <ndk/ketypes.h>
#include <pseh/pseh2.h>

// hint: for basic types (aka PBYTE, ULONG, etc)
#include <windef.h>

#include <debug.h>

NTSTATUS NTAPI DriverEntry(IN PDRIVER_OBJECT DriverObject,
                           IN PUNICODE_STRING RegistryPath) {
  ULONG ResultLength = 0;

  NTSTATUS status = ZwQuerySystemInformation(SystemProcessInformation, NULL, 0,
                                             &ResultLength);

  PBYTE ProcessInfoBuffer = NULL; // hint: PBYTE ~ `short` pointer to byte

  while (1) {
    const ULONG ProcessInfoBufferLength = ResultLength;
    if (ProcessInfoBuffer) {
      ExFreePool(ProcessInfoBuffer);
    }

    // hint (about pool allocator): https://stackoverflow.com/a/3816760

    ProcessInfoBuffer = (PBYTE)ExAllocatePool(PagedPool, ResultLength);

    status =
        ZwQuerySystemInformation(SystemProcessInformation, ProcessInfoBuffer,
                                 ProcessInfoBufferLength, &ResultLength);

    if (status != STATUS_INFO_LENGTH_MISMATCH) {
      break;
    }
  }

  PSYSTEM_PROCESS_INFORMATION pSPI =
      (PSYSTEM_PROCESS_INFORMATION)ProcessInfoBuffer;

  DPRINT1("Process list:\n");
  while (pSPI) {
    // hint: DPRINT1 ~ printf

    DPRINT1("%lld: %S (%lld)\n", (LONGLONG)(INT_PTR)pSPI->UniqueProcessId,
            pSPI->ImageName.Buffer,
            (LONGLONG)(INT_PTR)pSPI->InheritedFromUniqueProcessId);

    if (pSPI->NextEntryOffset == 0) {
      break;
    }

    pSPI = (PSYSTEM_PROCESS_INFORMATION)((LPBYTE)pSPI + pSPI->NextEntryOffset);
  }

  ExFreePool(ProcessInfoBuffer);

  return STATUS_SUCCESS;
}
