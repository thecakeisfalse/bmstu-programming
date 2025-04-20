#include <ntddk.h>
#include <ntifs.h>
#include <ndk/ntndk.h>
#include <windef.h>

#define NDEBUG
#include <debug.h>

// PAGE_SIZE -> 2^12

#define GetPte(base, offset)                                                   \
  (PHARDWARE_PTE)(((ULONG)(base) >> 12) + offset + PTE_BASE)

const PHARDWARE_PTE PTE_BASE = (PHARDWARE_PTE)0xc0000000;

const SIZE_T AllPagesCount = 10;
const SIZE_T PhysicalPagesCount = 5;

NTSTATUS NTAPI DriverEntry(IN PDRIVER_OBJECT DriverObject,
                           IN PUNICODE_STRING RegistryPath) {
  PVOID VirtualMemory = NULL;

  SIZE_T AllPagesSize = AllPagesCount * PAGE_SIZE;
  SIZE_T PhysicalPagesSize = PhysicalPagesCount * PAGE_SIZE;

  ZwAllocateVirtualMemory(NtCurrentProcess(), &VirtualMemory, 0, &AllPagesSize,
                          MEM_RESERVE, PAGE_READWRITE);

  ZwAllocateVirtualMemory(NtCurrentProcess(), &VirtualMemory, 0,
                          &PhysicalPagesSize, MEM_COMMIT, PAGE_READWRITE);

  // against CPU caching
  for (SIZE_T i = 0; i < PhysicalPagesCount; ++i) {
    *((PBYTE)VirtualMemory + PAGE_SIZE * i) = i + 1;
  }

  for (SIZE_T i = 0; i < AllPagesCount; ++i) {
    const PHARDWARE_PTE PointerPte = GetPte(VirtualMemory, i);

    DPRINT1("page #%d:\n", i + 1);
    DPRINT1("valid: %s\n", (PointerPte->Valid ? "true" : "false"));
    DPRINT1("dirty: %s\n", (PointerPte->Dirty ? "true" : "false"));
    DPRINT1("phys: 0x%x\n", (PointerPte->PageFrameNumber * PAGE_SIZE));
    DPRINT1("\n");
  }

  ZwFreeVirtualMemory(NtCurrentProcess(), &VirtualMemory, 0, MEM_RELEASE);

  return STATUS_SUCCESS;
}
