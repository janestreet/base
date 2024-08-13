#include <math.h>
#include <stdio.h>
#include <caml/fail.h>
#include <caml/alloc.h>

#define MAX_EXCEPTION_MESSAGE_LENGTH 256

double Base_caml_modf_positive_float_unboxed_exn(double a, double b) {
  // Raise in case of a negative modulus, as does Int.( % ).
  if (b < 0) {
    char exception_message[MAX_EXCEPTION_MESSAGE_LENGTH];
    sprintf(exception_message, "%.17g %% %.17g in float.ml: modulus should be positive",
            a, b);
    caml_invalid_argument(exception_message);
  }
  double m = fmod(a, b);
  // Produce a non-negative result in analogy with Int.( % ).
  return (m < 0) ? (m + b) : m;
}

CAMLprim value Base_caml_modf_positive_float_exn(value a, value b) {
  return caml_copy_double(
      Base_caml_modf_positive_float_unboxed_exn(Double_val(a), Double_val(b)));
}
