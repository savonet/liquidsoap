// js_of_ocaml build-fs encodes large files with this runtime primitive but
// does not link it in (missing-primitive warning, js_of_ocaml 6.5.0). Its
// output runs in its own scope, so the program's copy is not reachable.
globalThis.caml_jsbytes_of_base64 ??= function (s) {
  return atob(s);
};
