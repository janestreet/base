#include <caml/mlvalues.h>
#include <caml/version.h>

#if OCAML_VERSION < 41000

extern int caml_backtrace_pos;

#endif

CAMLprim value Base_clear_caml_backtrace_pos () {
  caml_backtrace_pos = 0;
  return Val_unit;
}
