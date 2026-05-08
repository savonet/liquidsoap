#include "config.h"

#ifdef HAS_MAX_ALIGN_T
#include <stdalign.h>
#include <stddef.h>
#define ALIGNMENT_BYTES alignof(max_align_t)
#else
#define ALIGNMENT_BYTES 16
#endif

#if defined(HAS_POSIX_MEMALIGN)
#include <caml/unixsupport.h>
#include <stdlib.h>
#define ALIGNED_ALLOC(data, alignment, len)                                    \
  {                                                                            \
    if (posix_memalign((void **)&data, alignment, len))                        \
      data = NULL;                                                             \
    if (data == NULL)                                                          \
      uerror("aligned_alloc", Nothing);                                        \
  }
#elif defined(HAS_MEMALIGN)
#include <caml/unixsupport.h>
#include <malloc.h>
#define ALIGNED_ALLOC(data, alignment, len)                                    \
  {                                                                            \
    data = memalign(alignment, len);                                           \
    if (data == NULL)                                                          \
      uerror("memalign", Nothing);                                             \
  }
#else
#define ALIGNED_ALLOC(data, alignment, len)                                    \
  {                                                                            \
    data = malloc(len + 0 * alignment);                                        \
    if (data == NULL)                                                          \
      caml_raise_out_of_memory();                                              \
  }
#endif

#ifdef HAS_CAML_INTERNALS
CAMLextern value caml_mm_ba_alloc_dims(int flags, int num_dims, void *data,
                                       ...);
#else
#define caml_mm_ba_alloc_dims caml_ba_alloc_dims
#endif
