#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <ocaml-ogg.h>

CAMLprim value liq_ocaml_ogg_stream_set_eos(value o_stream_state) {
  CAMLparam1(o_stream_state);
  ogg_stream_state *os = Stream_state_val(o_stream_state);
  os->e_o_s = 1;
  CAMLreturn(Val_unit);
}
