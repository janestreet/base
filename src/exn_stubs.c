#define CAML_INTERNALS
#ifndef CAML_NAME_SPACE
#define CAML_NAME_SPACE
#endif
/* If CAML_NAME_SPACE is not defined, then [backtrace_last_exn]
   below expands to a variable reference, so you get a syntax error.
   Surprisingly, this line is only necessary in external dune builds,
   not when building internally (jenga already defines CAML_NAME_SPACE). */
#include <caml/mlvalues.h>
#include <caml/backtrace.h>

CAMLprim value Base_clear_caml_backtrace_pos () {
  caml_backtrace_pos = 0;
  return Val_unit;
}

CAMLprim value Base_caml_exn_is_most_recent_exn (value exn) {
  return Val_bool(Caml_state->backtrace_last_exn == exn);
}
