#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CHANNELS 12

typedef struct iir {
  uint8_t channels;
  double x1[MAX_CHANNELS];
  double x2[MAX_CHANNELS];
  double y1[MAX_CHANNELS];
  double y2[MAX_CHANNELS];
  double a1;
  double a2;
  double b0;
  double b1;
  double b2;
} iir_t;

#define IIR_val(v) (*(iir_t **)Data_custom_val(v))

static void finalize_iir(value v) {
  iir_t *iir = IIR_val(v);
  free(iir);
}

static struct custom_operations iir_ops = {
    "liquidsoap_iir",         finalize_iir,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value liquidsoap_lufs_create_native(value _channels, value _a1,
                                             value _a2, value _b0, value _b1,
                                             value _b2) {
  CAMLparam5(_a1, _a2, _b0, _b1, _b2);
  CAMLlocal1(ans);
  int channels = Int_val(_channels);

  if (channels > MAX_CHANNELS)
    caml_failwith("LUFS: too many channels! Maximum channels is 12.");

  iir_t *iir = calloc(sizeof(iir_t), 1);
  if (!iir)
    caml_raise_out_of_memory();

  iir->channels = channels;
  iir->a1 = Double_val(_a1);
  iir->a2 = Double_val(_a2);
  iir->b0 = Double_val(_b0);
  iir->b1 = Double_val(_b1);
  iir->b2 = Double_val(_b2);

  ans = caml_alloc_custom(&iir_ops, sizeof(iir_t *), 0, 1);
  IIR_val(ans) = iir;
  CAMLreturn(ans);
}

CAMLprim value liquidsoap_lufs_create_bytecode(value *argv, int argn) {
  return liquidsoap_lufs_create_native(argv[0], argv[1], argv[2], argv[3],
                                       argv[4], argv[5]);
}

static inline void liquidsoap_lufs_process_stage(iir_t *iir, double *x,
                                                 double *y) {
  int i, c;
  size_t buf_len = iir->channels * sizeof(double);

  for (i = 0; i < iir->channels; i++)
    y[i] = iir->b0 * x[i] + iir->b1 * iir->x1[i] + iir->b2 * iir->x2[i] -
           iir->a1 * iir->y1[i] - iir->a2 * iir->y2[i];

  for (c = 0; c < iir->channels; c++) {
    iir->x2[c] = iir->x1[c];
    iir->x1[c] = x[c];
    iir->y2[c] = iir->y1[c];
    iir->y1[c] = y[c];
  }
}

CAMLprim value liquidsoap_lufs_process(value _stage1, value _stage2, value _x,
                                       value _ret) {
  CAMLparam4(_stage1, _stage2, _x, _ret);
  double tmp1[MAX_CHANNELS], tmp2[MAX_CHANNELS];
  double power = 0;
  int samples = Wosize_val(Field(_x, 0)) / Double_wosize;
  iir_t *stage1 = IIR_val(_stage1);
  iir_t *stage2 = IIR_val(_stage2);
  int i, c;

  for (i = 0; i < samples; i++) {
    for (c = 0; c < stage1->channels; c++)
      tmp1[c] = Double_field(Field(_x, c), i);

    liquidsoap_lufs_process_stage(stage1, tmp1, tmp2);
    liquidsoap_lufs_process_stage(stage2, tmp2, tmp1);

    for (c = 0; c < stage1->channels; c++)
      power += tmp1[c] * tmp1[c];
  }

  CAMLreturn(caml_copy_double(power / samples));
}
