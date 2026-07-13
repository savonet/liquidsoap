// SPDX-FileCopyrightText: 2022 - 2024 Savonet team
//
// SPDX-License-Identifier: MIT

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/threads.h>

#if defined(WIN32)
#include <pdh.h>
#include <psapi.h>
#include <tchar.h>
#include <windows.h>

// PROCESS_MEMORY_COUNTERS_EX2 requires Windows 10 1607+ / SDK 10.0.14393.0+
#if !defined(PROCESS_MEMORY_COUNTERS_EX2)
typedef struct _PROCESS_MEMORY_COUNTERS_EX2 {
  DWORD cb;
  DWORD PageFaultCount;
  SIZE_T PeakWorkingSetSize;
  SIZE_T WorkingSetSize;
  SIZE_T QuotaPeakPagedPoolUsage;
  SIZE_T QuotaPagedPoolUsage;
  SIZE_T QuotaPeakNonPagedPoolUsage;
  SIZE_T QuotaNonPagedPoolUsage;
  SIZE_T PagefileUsage;
  SIZE_T PeakPagefileUsage;
  SIZE_T PrivateUsage;
  SIZE_T PrivateWorkingSetSize;
  ULONG64 SharedCommitUsage;
} PROCESS_MEMORY_COUNTERS_EX2;
#endif

CAMLprim value ocaml_mem_usage_mem_usage(value unit) {
  CAMLparam0();
  CAMLlocal1(ret);
  MEMORYSTATUSEX mem_info;
  PROCESS_MEMORY_COUNTERS_EX2 pmc;
  DWORDLONG total_virtual_memory = 0;
  DWORDLONG total_used_virtual_memory = 0;
  DWORDLONG total_physical_memory = 0;
  DWORDLONG total_used_physical_memory = 0;
  SIZE_T process_virtual_memory = 0;
  SIZE_T process_physical_memory = 0;
  SIZE_T process_private_memory = 0;
  SIZE_T process_swapped_memory = 0;

  caml_release_runtime_system();
  mem_info.dwLength = sizeof(MEMORYSTATUSEX);
  if (GlobalMemoryStatusEx(&mem_info)) {
    total_virtual_memory = mem_info.ullTotalPageFile;
    total_used_virtual_memory =
        mem_info.ullTotalPageFile - mem_info.ullAvailPageFile;
    total_physical_memory = mem_info.ullTotalPhys;
    total_used_physical_memory = mem_info.ullTotalPhys - mem_info.ullAvailPhys;
  }

  pmc.cb = sizeof(pmc);
  if (GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS *)&pmc,
                           sizeof(pmc))) {
    process_virtual_memory = pmc.PrivateUsage;
    process_physical_memory = pmc.WorkingSetSize;
    process_private_memory = pmc.PrivateWorkingSetSize;
    // Private committed memory not resident in the working set: the closest
    // per-process approximation of paged-out memory available here.
    if (pmc.PrivateUsage > pmc.PrivateWorkingSetSize)
      process_swapped_memory = pmc.PrivateUsage - pmc.PrivateWorkingSetSize;
  } else {
    // Older Windows rejects the EX2 size: retry with the EX layout.
    PROCESS_MEMORY_COUNTERS_EX pmc_ex;
    pmc_ex.cb = sizeof(pmc_ex);
    if (GetProcessMemoryInfo(GetCurrentProcess(),
                             (PROCESS_MEMORY_COUNTERS *)&pmc_ex,
                             sizeof(pmc_ex))) {
      process_virtual_memory = pmc_ex.PrivateUsage;
      process_physical_memory = pmc_ex.WorkingSetSize;
      if (pmc_ex.PrivateUsage > pmc_ex.WorkingSetSize)
        process_swapped_memory = pmc_ex.PrivateUsage - pmc_ex.WorkingSetSize;
    }
  }
  caml_acquire_runtime_system();

  ret = caml_alloc_tuple(8);
  Store_field(ret, 0, Val_long(total_virtual_memory));
  Store_field(ret, 1, Val_long(total_physical_memory));
  Store_field(ret, 2, Val_long(total_used_virtual_memory));
  Store_field(ret, 3, Val_long(total_used_physical_memory));
  Store_field(ret, 4, Val_long(process_virtual_memory));
  Store_field(ret, 5, Val_long(process_physical_memory));
  Store_field(ret, 6, Val_long(process_private_memory));
  Store_field(ret, 7, Val_long(process_swapped_memory));
  CAMLreturn(ret);
}
#elif defined(__APPLE__)
#include <mach/mach.h>
#include <mach/mach_host.h>
#include <mach/mach_init.h>
#include <mach/mach_types.h>
#include <mach/mach_vm.h>
#include <mach/vm_statistics.h>
#include <stdio.h>
#include <sys/sysctl.h>
#include <unistd.h>

void private_pages(unsigned int *pages_resident,
                   unsigned int *pages_swapped_out) {
  mach_vm_address_t address = 0;
  mach_vm_size_t size = 0;
  uint32_t depth;
  vm_region_submap_info_data_64_t info;
  mach_msg_type_number_t count;
  kern_return_t kr;
  *pages_resident = 0;
  *pages_swapped_out = 0;

  while (1) {
    depth = 2048;
    count = VM_REGION_SUBMAP_INFO_COUNT_64;
    kr = mach_vm_region_recurse(mach_task_self(), &address, &size, &depth,
                                (vm_region_recurse_info_t)&info, &count);

    if (kr != KERN_SUCCESS)
      break;

    if (info.share_mode == SM_PRIVATE) {
      *pages_resident += info.pages_resident;
      *pages_swapped_out += info.pages_swapped_out;
    }

    address += size;
  }
}

CAMLprim value ocaml_mem_usage_mem_usage(value unit) {
  CAMLparam0();
  CAMLlocal1(ret);
  uint64_t total_virtual_memory, total_physical_memory,
      total_used_physical_memory, total_used_virtual_memory,
      process_physical_memory, process_virtual_memory, process_private_memory,
      process_swapped_memory;
  struct xsw_usage vmem_usage = {0};
  size_t size = sizeof(vmem_usage);
  mach_task_basic_info_data_t t_info;
  mach_msg_type_number_t t_info_count = MACH_TASK_BASIC_INFO_COUNT;
  vm_size_t page_size;
  mach_port_t mach_port;
  mach_msg_type_number_t count;
  vm_statistics64_data_t vm_stats;
  int pagesize;
  unsigned int pages_resident, pages_swapped_out;

  caml_release_runtime_system();
  if (sysctlbyname("vm.swapusage", &vmem_usage, &size, NULL, 0) != 0) {
    fprintf(stderr, "Error while getting swap usage.\n");
    vmem_usage.xsu_total = 0;
    vmem_usage.xsu_used = 0;
  }

  if (task_info(mach_task_self(), MACH_TASK_BASIC_INFO, (task_info_t)&t_info,
                &t_info_count)) {
    fprintf(stderr,
            "Unable to get virtual memory currently used by the process.\n");
    process_physical_memory = 0;
    process_virtual_memory = 0;
  } else {
    process_physical_memory = t_info.resident_size;
    process_virtual_memory = t_info.virtual_size;
  }

  // hw.memsize is uint64_t, hw.physmem is 32-bit and overflows >2GB
  {
    uint64_t memsize;
    size_t memsize_len = sizeof(memsize);
    if (sysctlbyname("hw.memsize", &memsize, &memsize_len, NULL, 0) != 0) {
      fprintf(stderr, "Unable to get total physical memory.\n");
      total_physical_memory = 0;
    } else {
      total_physical_memory = memsize;
    }
  }

  mach_port = mach_host_self();
  count = HOST_VM_INFO64_COUNT;

  if (host_page_size(mach_port, &page_size) != KERN_SUCCESS) {
    fprintf(stderr, "Unable to get host page size.\n");
    total_used_physical_memory = 0;
  } else {
    if (host_statistics64(mach_port, HOST_VM_INFO64, (host_info64_t)&vm_stats,
                          &count) != KERN_SUCCESS) {
      fprintf(stderr, "Unable to get host stats.\n");
      total_used_physical_memory = 0;
    } else {
      // Matches Activity Monitor's notion of used memory: anonymous/active
      // pages + wired + compressed. Inactive pages are mostly reclaimable
      // file cache and are not counted.
      total_used_physical_memory =
          ((uint64_t)vm_stats.active_count + (uint64_t)vm_stats.wire_count +
           (uint64_t)vm_stats.compressor_page_count) *
          (uint64_t)page_size;
    }
  }
  mach_port_deallocate(mach_task_self(), mach_port);

  // RAM + swap, matching the Linux implementation's semantics.
  total_virtual_memory = total_physical_memory + vmem_usage.xsu_total;
  total_used_virtual_memory = total_used_physical_memory + vmem_usage.xsu_used;

  pagesize = getpagesize();
  private_pages(&pages_resident, &pages_swapped_out);
  process_private_memory = (uint64_t)pages_resident * pagesize;
  process_swapped_memory = (uint64_t)pages_swapped_out * pagesize;

  caml_acquire_runtime_system();

  ret = caml_alloc_tuple(8);
  Store_field(ret, 0, Val_long(total_virtual_memory));
  Store_field(ret, 1, Val_long(total_physical_memory));
  Store_field(ret, 2, Val_long(total_used_virtual_memory));
  Store_field(ret, 3, Val_long(total_used_physical_memory));
  Store_field(ret, 4, Val_long(process_virtual_memory));
  Store_field(ret, 5, Val_long(process_physical_memory));
  Store_field(ret, 6, Val_long(process_private_memory));
  Store_field(ret, 7, Val_long(process_swapped_memory));
  CAMLreturn(ret);
}
#else
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/sysinfo.h>
#include <sys/types.h>

CAMLprim value ocaml_mem_usage_mem_usage(value unit) {
  CAMLparam0();
  CAMLlocal1(ret);
  struct sysinfo memInfo;
  uint64_t total_virtual_memory, total_physical_memory,
      total_used_virtual_memory, total_used_physical_memory;
  unsigned long long process_virtual_memory = 0, process_physical_memory = 0,
                     process_private_memory = 0, process_swapped_memory = 0,
                     tmp;
  FILE *file;
  char buffer[1024] = "";

  caml_release_runtime_system();
  sysinfo(&memInfo);
  total_physical_memory = memInfo.totalram;
  total_physical_memory *= memInfo.mem_unit;

  total_virtual_memory = memInfo.totalram;
  total_virtual_memory += memInfo.totalswap;
  total_virtual_memory *= memInfo.mem_unit;

  // bufferram is reclaimable cache; sysinfo does not expose the page cache,
  // so this still over-reports "used" compared to /proc/meminfo MemAvailable.
  total_used_physical_memory =
      memInfo.totalram - memInfo.freeram - memInfo.bufferram;
  total_used_physical_memory *= memInfo.mem_unit;

  total_used_virtual_memory =
      memInfo.totalram - memInfo.freeram - memInfo.bufferram;
  total_used_virtual_memory += memInfo.totalswap - memInfo.freeswap;
  total_used_virtual_memory *= memInfo.mem_unit;

  file = fopen("/proc/self/status", "r");
  if (file) {
    while (fscanf(file, " %1023s", buffer) == 1) {
      if (strcmp(buffer, "VmSize:") == 0) {
        if (fscanf(file, " %llu", &process_virtual_memory) != 1)
          process_virtual_memory = 0;
        process_virtual_memory *= 1024;
        continue;
      }

      if (strcmp(buffer, "VmRSS:") == 0) {
        if (fscanf(file, " %llu", &process_physical_memory) != 1)
          process_physical_memory = 0;
        process_physical_memory *= 1024;
        continue;
      }
    }
    fclose(file);
  }

  // smaps_rollup (Linux 4.14+) provides aggregated stats in one entry
  // Falls back to smaps for older kernels
  file = fopen("/proc/self/smaps_rollup", "r");
  if (!file)
    file = fopen("/proc/self/smaps", "r");
  if (file) {
    while (fscanf(file, " %1023s", buffer) == 1) {
      if (strcmp(buffer, "Private_Dirty:") == 0 ||
          strcmp(buffer, "Private_Clean:") == 0) {
        if (fscanf(file, " %llu", &tmp) == 1)
          process_private_memory += tmp * 1024;
        continue;
      }

      if (strcmp(buffer, "Swap:") == 0) {
        if (fscanf(file, " %llu", &tmp) == 1)
          process_swapped_memory += tmp * 1024;
        continue;
      }
    }
    fclose(file);
  }

  caml_acquire_runtime_system();

  ret = caml_alloc_tuple(8);
  Store_field(ret, 0, Val_long(total_virtual_memory));
  Store_field(ret, 1, Val_long(total_physical_memory));
  Store_field(ret, 2, Val_long(total_used_virtual_memory));
  Store_field(ret, 3, Val_long(total_used_physical_memory));
  Store_field(ret, 4, Val_long(process_virtual_memory));
  Store_field(ret, 5, Val_long(process_physical_memory));
  Store_field(ret, 6, Val_long(process_private_memory));
  Store_field(ret, 7, Val_long(process_swapped_memory));
  CAMLreturn(ret);
}
#endif
