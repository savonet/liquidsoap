#define CAML_INTERNALS
#include <caml/alloc.h>
#include <caml/codefrag.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

/* Finds the fragment the same way Marshal does for a closure it writes. */
CAMLprim value liquidsoap_code_digest(value _closure) {
  CAMLparam1(_closure);
  struct code_fragment *fragment =
      caml_find_code_fragment_by_pc((char *)Code_val(_closure));
  if (fragment == NULL)
    caml_failwith("Code_digest: no code fragment");
  unsigned char *digest = caml_digest_of_code_fragment(fragment);
  if (digest == NULL)
    caml_failwith("Code_digest: code fragment has no digest");
  CAMLreturn(caml_alloc_initialized_string(16, (const char *)digest));
}
