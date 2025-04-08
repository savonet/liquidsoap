#define CAML_INTERNALS 1

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdalign.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#include "image_data.h"

#ifndef Bytes_val
#define Bytes_val String_val
#endif

// See: https://github.com/ocaml/ocaml/pull/10788
#ifdef HAS_CAML_INTERNALS
CAMLexport value caml_mm_ba_alloc(int flags, int num_dims, void *data,
                                  intnat *dim) {
  uintnat num_elts, asize, size;
  int i, is_managed;
  value res;
  struct caml_ba_array *b;
  intnat dimcopy[CAML_BA_MAX_NUM_DIMS];

  CAMLassert(num_dims >= 0 && num_dims <= CAML_BA_MAX_NUM_DIMS);
  CAMLassert((flags & CAML_BA_KIND_MASK) <= CAML_BA_CHAR);
  for (i = 0; i < num_dims; i++)
    dimcopy[i] = dim[i];
  num_elts = 1;
  for (i = 0; i < num_dims; i++) {
    if (caml_umul_overflow(num_elts, dimcopy[i], &num_elts))
      caml_raise_out_of_memory();
  }
  if (caml_umul_overflow(
          num_elts, caml_ba_element_size[flags & CAML_BA_KIND_MASK], &size))
    caml_raise_out_of_memory();
  if (data == NULL) {
    data = malloc(size);
    if (data == NULL && size != 0)
      caml_raise_out_of_memory();
    flags |= CAML_BA_MANAGED;
  }
  asize = SIZEOF_BA_ARRAY + num_dims * sizeof(intnat);
  is_managed = ((flags & CAML_BA_MANAGED_MASK) == CAML_BA_MANAGED);
  res = caml_alloc_custom_mem(&caml_ba_ops, asize, is_managed ? size : 0);
  b = Caml_ba_array_val(res);
  b->data = data;
  b->num_dims = num_dims;
  b->flags = flags;
  b->proxy = NULL;
  for (i = 0; i < num_dims; i++)
    b->dim[i] = dimcopy[i];
  return res;
}

CAMLexport value caml_mm_ba_alloc_dims(int flags, int num_dims, void *data,
                                       ...) {
  va_list ap;
  intnat dim[CAML_BA_MAX_NUM_DIMS];
  int i;
  value res;

  CAMLassert(num_dims <= CAML_BA_MAX_NUM_DIMS);
  va_start(ap, data);
  for (i = 0; i < num_dims; i++)
    dim[i] = va_arg(ap, intnat);
  va_end(ap);
  res = caml_mm_ba_alloc(flags, num_dims, data, dim);
  return res;
}
#endif

CAMLprim value caml_data_aligned(value _alignment, value _len) {
  CAMLparam2(_alignment, _len);
  CAMLlocal1(ans);
  int alignment = Int_val(_alignment);
  int len = Int_val(_len);
  unsigned char *data;

  if (alignment < ALIGNMENT_BYTES) {
    alignment = ALIGNMENT_BYTES;
  }

#ifdef HAS_CAML_INTERNALS
  ALIGNED_ALLOC(data, alignment, len);

  ans = caml_mm_ba_alloc_dims(
      CAML_BA_MANAGED | CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, data, len);
#else
  ans = caml_mm_ba_alloc_dims(
      CAML_BA_MANAGED | CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, NULL, len);

  ALIGNED_ALLOC(data, alignment, len);
  free(Caml_ba_data_val(ans));
  Caml_ba_array_val(ans)->data = data;
#endif

  CAMLreturn(ans);
}

CAMLprim value caml_data_of_string(value s) {
  CAMLparam1(s);
  CAMLlocal1(ans);
  long len = caml_string_length(s);
  unsigned char *data = malloc(len);
  if (data == NULL)
    caml_raise_out_of_memory();
  memcpy(data, String_val(s), len);
  ans = caml_mm_ba_alloc_dims(
      CAML_BA_MANAGED | CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, data, len);
  CAMLreturn(ans);
}

CAMLprim value caml_data_to_string(value _data) {
  CAMLparam1(_data);
  CAMLlocal1(ans);
  unsigned char *data = Caml_ba_data_val(_data);
  long len = Caml_ba_array_val(_data)->dim[0];
  ans = caml_alloc_string(len);
  memcpy(Bytes_val(ans), data, len);
  CAMLreturn(ans);
}

CAMLprim value caml_data_copy(value _src) {
  CAMLparam1(_src);
  CAMLlocal1(ans);
  unsigned char *src = Caml_ba_data_val(_src);
  long len = Caml_ba_array_val(_src)->dim[0];
  unsigned char *dst = malloc(len);
  if (dst == NULL)
    caml_raise_out_of_memory();
  memcpy(dst, src, len);
  ans = caml_mm_ba_alloc_dims(
      CAML_BA_MANAGED | CAML_BA_C_LAYOUT | CAML_BA_UINT8, 1, dst, len);
  CAMLreturn(ans);
}

CAMLprim value caml_data_blit_off(value _src, value _soff, value _dst,
                                  value _doff, value _len) {
  CAMLparam5(_src, _soff, _dst, _doff, _len);
  int soff = Int_val(_soff);
  int doff = Int_val(_doff);
  int len = Int_val(_len);
  unsigned char *src = Caml_ba_data_val(_src);
  unsigned char *dst = Caml_ba_data_val(_dst);
  memcpy(dst + doff, src + soff, len);
  CAMLreturn(Val_unit);
}
