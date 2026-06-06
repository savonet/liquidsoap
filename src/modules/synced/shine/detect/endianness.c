#include <caml/memory.h>
#include <stdint.h>

enum {
  OCAML_SHINE_LITTLE_ENDIAN = 0x0100,
  OCAML_SHINE_BIG_ENDIAN = 0x0001,
};

static const union {
  unsigned char bytes[2];
  uint16_t value;
} host_order = {{0, 1}};

CAMLprim value ocaml_shine_is_big_endian(value unit) {
  CAMLparam0();

  if (host_order.value == OCAML_SHINE_BIG_ENDIAN)
    CAMLreturn(Val_bool(1));

  CAMLreturn(Val_bool(0));
}
