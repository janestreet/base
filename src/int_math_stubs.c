#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <caml/alloc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

static int64_t int_pow(int64_t base, int64_t exponent) {
  int64_t ret = 1;
  int64_t mul[4];
  mul[0] = 1;
  mul[1] = base;
  mul[3] = 1;

  while(exponent != 0) {
    mul[1] *= mul[3];
    mul[2] = mul[1] * mul[1];
    mul[3] = mul[2] * mul[1];
    ret *= mul[exponent & 3];
    exponent >>= 2;
  }

  return ret;
}

CAMLprim value Base_int_math_int_pow_stub(value base, value exponent) {
  return (Val_long(int_pow(Long_val(base), Long_val(exponent))));
}

CAMLprim value Base_int_math_int64_pow_stub(value base, value exponent) {
  CAMLparam2(base, exponent);
  CAMLreturn(caml_copy_int64(int_pow(Int64_val(base), Int64_val(exponent))));
}

/* This implementation is faster than [__builtin_popcount(v) - 1], even though
 * it seems more complicated.  The [&] clears the shifted sign bit after
 * [Long_val] or [Int_val]. */
CAMLprim value Base_int_math_int_popcount(value v) {
#ifdef ARCH_SIXTYFOUR
  return Val_int (__builtin_popcountll (Long_val (v) & ~((uint64_t)1 << 63)));
#else
  return Val_int (__builtin_popcount   (Int_val  (v) & ~((uint32_t)1 << 31)));
#endif
}

CAMLprim value Base_int_math_int_clz(value v) {
#ifdef ARCH_SIXTYFOUR
  return Val_int (__builtin_clzll (Long_val(v)));
#else
  return Val_int (__builtin_clz   (Int_val (v)));
#endif
}
