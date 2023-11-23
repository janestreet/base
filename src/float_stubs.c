#include <caml/alloc.h>
#include <caml/mlvalues.h>

double caml_sse2_float64_min(double x, double y) { return x < y ? x : y; }

double caml_sse2_float64_max(double x, double y) { return x > y ? x : y; }

CAMLprim value caml_sse2_float64_min_bytecode(value x, value y) {
  return caml_copy_double(caml_sse2_float64_min(Double_val(x), Double_val(y)));
}

CAMLprim value caml_sse2_float64_max_bytecode(value x, value y) {
  return caml_copy_double(caml_sse2_float64_max(Double_val(x), Double_val(y)));
}
