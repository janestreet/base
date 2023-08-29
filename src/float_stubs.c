#include <math.h>
#include <stdint.h>

#include <caml/alloc.h>
#include <caml/mlvalues.h>

double caml_float_min_unboxed(double x, double y) { return x < y ? x : y; }

CAMLprim value caml_float_min(value x, value y) {
  return caml_copy_double(caml_float_min_unboxed(Double_val(x), Double_val(y)));
}

double caml_float_max_unboxed(double x, double y) { return x > y ? x : y; }

CAMLprim value caml_float_max(value x, value y) {
  return caml_copy_double(caml_float_max_unboxed(Double_val(x), Double_val(y)));
}
