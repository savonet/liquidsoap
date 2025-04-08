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

typedef struct _PROCESS_MEMORY_COUNTERS_EX2 {
  DWORD   cb;
  DWORD   PageFaultCount;
  SIZE_T  PeakWorkingSetSize;
  SIZE_T  WorkingSetSize;
  SIZE_T  QuotaPeakPagedPoolUsage;
  SIZE_T  QuotaPagedPoolUsage;
  SIZE_T  QuotaPeakNonPagedPoolUsage;
  SIZE_T  QuotaNonPagedPoolUsage;
  SIZE_T  PagefileUsage;
  SIZE_T  PeakPagefileUsage;
  SIZE_T  PrivateUsage;
  SIZE_T  PrivateWorkingSetSize;
  ULONG64 SharedCommitUsage;
} PROCESS_MEMORY_COUNTERS_EX2;

CAMLprim value ocaml_mem_usage_mem_usage(value unit) {
  CAMLparam0();
  CAMLlocal1(ret);
  MEMORYSTATUSEX mem_info;
  PROCESS_MEMORY_COUNTERS_EX2 pmc;
  DWORDLONG total_virtual_memory;
  DWORDLONG total_used_virtual_memory;
  DWORDLONG total_physical_memory;
  DWORDLONG total_used_physical_memory;
  SIZE_T process_virtual_memory;
  SIZE_T process_physical_memory;
  SIZE_T process_private_memory;
  SIZE_T process_swapped_memory;

  caml_release_runtime_system();
  mem_info.dwLength = sizeof(MEMORYSTATUSEX);
  GlobalMemoryStatusEx(&mem_info);
  total_virtual_memory = mem_info.ullTotalPageFile;
  total_used_virtual_memory =
      mem_info.ullTotalPageFile - mem_info.ullAvailPageFile;
  total_physical_memory = mem_info.ullTotalPhys;
  total_used_physical_memory = mem_info.ullTotalPhys - mem_info.ullAvailPhys;

  GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS *)&pmc,
                       sizeof(pmc));
  process_virtual_memory = pmc.PrivateUsage;
  process_physical_memory = pmc.WorkingSetSize;
  process_private_memory = pmc.PrivateWorkingSetSize;
  process_swapped_memory = pmc.PagefileUsage;
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
#include <sys/mount.h>
#include <sys/sysctl.h>
#include <unistd.h>

void private_pages(unsigned int *pages_resident,
                   unsigned int *pages_swapped_out) {
  mach_vm_address_t address = 0;
  mach_vm_size_t size = 0;
  uint32_t depth = 2048;
  vm_region_submap_info_data_64_t info;
  mach_msg_type_number_t count = VM_REGION_SUBMAP_INFO_COUNT_64;
  kern_return_t kr;
  *pages_resident = 0;
  *pages_swapped_out = 0;

  while (1) {
    kr = mach_vm_region_recurse(mach_task_self(), &address, &size, &depth,
                                (vm_region_recurse_info_t)&info, &count);

    if (kr != KERN_SUCCESS || size == 0)
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
  struct statfs stats;
  uint64_t total_virtual_memory, total_physical_memory,
      total_used_physical_memory, total_used_virtual_memory,
      process_physical_memory, process_virtual_memory, process_private_memory,
      process_swapped_memory;
  struct xsw_usage vmem_usage = {0};
  size_t size = sizeof(vmem_usage);
  struct task_basic_info t_info;
  mach_msg_type_number_t t_info_count = TASK_BASIC_INFO_COUNT;
  vm_size_t page_size;
  mach_port_t mach_port;
  mach_msg_type_number_t count;
  vm_statistics64_data_t vm_stats;
  int pagesize;
  unsigned int pages_resident, pages_swapped_out;

  caml_release_runtime_system();
  if (statfs("/", &stats) != 0) {
    fprintf(stderr, "Error while getting free swap space.\n");
    total_virtual_memory = 0;
  } else {
    total_virtual_memory = (uint64_t)stats.f_bsize * stats.f_bfree;
  }

  if (sysctlbyname("vm.swapusage", &vmem_usage, &size, NULL, 0) != 0) {
    fprintf(stderr, "Error while getting swap usage.\n");
    total_used_virtual_memory = 0;
  } else {
    total_used_virtual_memory = vmem_usage.xsu_used;
  }

  if (task_info(mach_task_self(), TASK_BASIC_INFO, (task_info_t)&t_info,
                &t_info_count)) {
    fprintf(stderr,
            "Unable to get virtual memory currently used by the process.\n");
    process_physical_memory = 0;
    process_virtual_memory = 0;
  } else {
    process_physical_memory = t_info.resident_size;
    process_virtual_memory = t_info.virtual_size;
  }

  mach_port = mach_host_self();
  count = sizeof(vm_stats) / sizeof(natural_t);

  if (host_page_size(mach_port, &page_size) != KERN_SUCCESS) {
    fprintf(stderr, "Unable to get host page size.\n");
    total_physical_memory = 0;
    total_used_physical_memory = 0;
  } else {
    if (host_statistics64(mach_port, HOST_VM_INFO, (host_info64_t)&vm_stats,
                          &count) != KERN_SUCCESS) {
      fprintf(stderr, "Unable to get host stats.\n");
      total_physical_memory = 0;
      total_used_physical_memory = 0;
    } else {
      total_physical_memory = vm_stats.free_count * (int64_t)page_size;

      total_used_physical_memory =
          ((int64_t)vm_stats.active_count + (int64_t)vm_stats.inactive_count +
           (int64_t)vm_stats.wire_count) *
          (int64_t)page_size;
    }
  }

  pagesize = getpagesize();
  private_pages(&pages_resident, &pages_swapped_out);
  process_private_memory = pages_resident * pagesize;
  process_swapped_memory = pages_swapped_out * pagesize;

  caml_acquire_runtime_system();

  ret = caml_alloc_tuple(8);
  ;
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
  unsigned long long process_virtual_memory, process_physical_memory,
      process_private_memory, process_swapped_memory, tmp;
  FILE *file;
  char buffer[1024] = "";

  caml_release_runtime_system();
  sysinfo(&memInfo);
  total_physical_memory = memInfo.totalram;
  total_physical_memory *= memInfo.mem_unit;

  total_virtual_memory = memInfo.totalram;
  total_virtual_memory += memInfo.totalswap;
  total_virtual_memory *= memInfo.mem_unit;

  total_used_virtual_memory = memInfo.totalram - memInfo.freeram;
  total_used_virtual_memory += memInfo.totalswap - memInfo.freeswap;
  total_used_virtual_memory *= memInfo.mem_unit;

  total_used_physical_memory = memInfo.totalram - memInfo.freeram;
  total_used_physical_memory *= memInfo.mem_unit;

  file = fopen("/proc/self/status", "r");
  if (file) {
    while (fscanf(file, " %1023s", buffer) == 1) {
      if (strcmp(buffer, "VmSize:") == 0) {
        if (fscanf(file, " %lld", &process_virtual_memory) != 1)
          process_virtual_memory = 0;
        process_virtual_memory *= 1024;
        continue;
      }

      if (strcmp(buffer, "VmRSS:") == 0) {
        if (fscanf(file, " %lld", &process_physical_memory) != 1)
          process_physical_memory = 0;
        process_physical_memory *= 1024;
        continue;
      }
    }
    fclose(file);
  }

  process_private_memory = 0;
  process_swapped_memory = 0;
  file = fopen("/proc/self/smaps", "r");
  if (file) {
    while (fscanf(file, " %1023s", buffer) == 1) {
      if (strcmp(buffer, "Private_Dirty:") == 0) {
        if (fscanf(file, " %lld", &tmp) != 1)
          tmp = 0;
        process_private_memory += tmp * 1024;
        continue;
      }

      if (strcmp(buffer, "Swap:") == 0) {
        if (fscanf(file, " %lld", &tmp) != 1)
          tmp = 0;
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
