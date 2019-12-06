#include <caml/mlvalues.h>
#include <caml/version.h>

#if OCAML_VERSION >= 41000

CAMLprim value Base_clear_caml_backtrace_pos () {
  Caml_state_field(backtrace_pos) = 0;
  return Val_unit;
}

#else

extern int caml_backtrace_pos;

CAMLprim value Base_clear_caml_backtrace_pos () {
  caml_backtrace_pos = 0;
  return Val_unit;
}

#endif
