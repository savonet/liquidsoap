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
  CAMLparam5(_channels, _a1, _a2, _b0, _b1);
  CAMLxparam1(_b2);
  CAMLlocal1(ans);
  int channels = Int_val(_channels);

  if (channels > MAX_CHANNELS)
    caml_failwith("LUFS: too many channels! Maximum channels is 12.");

  iir_t *iir = calloc(1, sizeof(iir_t));
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

CAMLprim value liquidsoap_lufs_process(value _stage1, value _stage2, value _x) {
  CAMLparam3(_stage1, _stage2, _x);
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

/* ---- True peak measurement (ITU-R BS.1770-4, Annex 2) ---- */

#define TP_TAPS   12
#define TP_PHASES  4

/* Polyphase FIR coefficients from ITU-R BS.1770-4, Annex 2, Table 2.
   Four phases of 12 taps each, for 4x oversampling. */
static const double tp_fir[TP_PHASES][TP_TAPS] = {
  { 0.0017089843750,  0.0109863281250, -0.0196533203125,  0.0332031250000,
   -0.0594482421875,  0.1373291015625,  0.4650878906250,  0.2177734375000,
   -0.1015625000000,  0.0535888671875, -0.0244140625000,  0.0086059570313 },
  { 0.0037841796875, -0.0296630859375,  0.0539550781250, -0.0932617187500,
    0.1665039062500, -0.3999023437500,  0.8686523437500,  0.3940429687500,
   -0.1665039062500,  0.0830078125000, -0.0354003906250,  0.0117187500000 },
  { 0.0117187500000, -0.0354003906250,  0.0830078125000, -0.1665039062500,
    0.3940429687500,  0.8686523437500, -0.3999023437500,  0.1665039062500,
   -0.0932617187500,  0.0539550781250, -0.0296630859375,  0.0037841796875 },
  { 0.0086059570313, -0.0244140625000,  0.0535888671875, -0.1015625000000,
    0.2177734375000,  0.4650878906250,  0.1373291015625, -0.0594482421875,
    0.0332031250000, -0.0196533203125,  0.0109863281250,  0.0017089843750 }
};

typedef struct {
  uint8_t channels;
  int     pos;
  double  history[MAX_CHANNELS][TP_TAPS];
} tp_state_t;

#define TP_val(v) (*(tp_state_t **)Data_custom_val(v))

static void finalize_tp(value v) { free(TP_val(v)); }

static struct custom_operations tp_ops = {
    "liquidsoap_tp",          finalize_tp,
    custom_compare_default,   custom_hash_default,
    custom_serialize_default, custom_deserialize_default};

CAMLprim value liquidsoap_lufs_true_peak_create(value _channels) {
  CAMLparam1(_channels);
  CAMLlocal1(ans);
  int channels = Int_val(_channels);

  if (channels > MAX_CHANNELS)
    caml_failwith("True peak: too many channels! Maximum is 12.");

  tp_state_t *tp = calloc(1, sizeof(tp_state_t));
  if (!tp)
    caml_raise_out_of_memory();

  tp->channels = channels;
  tp->pos      = 0;

  ans = caml_alloc_custom(&tp_ops, sizeof(tp_state_t *), 0, 1);
  TP_val(ans) = tp;
  CAMLreturn(ans);
}

/* Unlike liquidsoap_lufs_process (the original IIR function), we validate that
   the OCaml buffer has at least as many channel sub-arrays as the state expects.
   The original code relies on the OCaml wrapper's assert for this guarantee;
   we add a C-side guard as well because an out-of-bounds Field() access on a
   format change would be a silent memory-safety violation, not a catchable
   OCaml exception. */
CAMLprim value liquidsoap_lufs_true_peak_process(value _state, value _x) {
  CAMLparam2(_state, _x);
  tp_state_t *tp = TP_val(_state);

  if (Wosize_val(_x) < (mlsize_t)tp->channels)
    caml_invalid_argument("true_peak_process: buffer has fewer channels than state");

  int samples    = Wosize_val(Field(_x, 0)) / Double_wosize;
  double peak    = 0.0;
  int i, c, p, t;

  for (i = 0; i < samples; i++) {
    int wpos = tp->pos;

    /* Write incoming sample into circular history buffer */
    for (c = 0; c < tp->channels; c++)
      tp->history[c][wpos] = Double_field(Field(_x, c), i);

    tp->pos = (tp->pos + 1) % TP_TAPS;

    /* Compute all 4 interpolated sub-samples via polyphase FIR */
    for (p = 0; p < TP_PHASES; p++) {
      for (c = 0; c < tp->channels; c++) {
        double y = 0.0;
        for (t = 0; t < TP_TAPS; t++) {
          int idx = (wpos - t + TP_TAPS) % TP_TAPS;
          y += tp_fir[p][t] * tp->history[c][idx];
        }
        double abs_y = y < 0.0 ? -y : y;
        if (abs_y > peak) peak = abs_y;
      }
    }
  }

  CAMLreturn(caml_copy_double(peak));
}
