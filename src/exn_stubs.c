#include <caml/mlvalues.h>

extern int caml_backtrace_pos;

CAMLprim value Base_clear_caml_backtrace_pos (value __attribute__((unused)) unit) {
  caml_backtrace_pos = 0;
  return Val_unit;
}
