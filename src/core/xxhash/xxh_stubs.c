#define XXH_INLINE_ALL
#include "xxhash.h"
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#define State_val(v) (*((XXH3_state_t **)Data_custom_val(v)))

static void state_finalize(value _state) { XXH3_freeState(State_val(_state)); }

static struct custom_operations state_ops = {
    "liquidsoap.xxh3_state",    state_finalize,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

CAMLprim value liquidsoap_xxh3_create(value _unit) {
  CAMLparam1(_unit);
  CAMLlocal1(_state);
  XXH3_state_t *state = XXH3_createState();
  if (state == NULL)
    caml_raise_out_of_memory();
  XXH3_64bits_reset(state);
  _state = caml_alloc_custom(&state_ops, sizeof(XXH3_state_t *), 0, 1);
  State_val(_state) = state;
  CAMLreturn(_state);
}

CAMLprim value liquidsoap_xxh3_update(value _state, value _data, value _offset,
                                      value _length) {
  XXH3_64bits_update(State_val(_state), String_val(_data) + Long_val(_offset),
                     Long_val(_length));
  return Val_unit;
}

CAMLprim value liquidsoap_xxh3_digest(value _state) {
  CAMLparam1(_state);
  CAMLreturn(caml_copy_int64(XXH3_64bits_digest(State_val(_state))));
}
