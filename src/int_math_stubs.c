#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#ifdef _MSC_VER

#include <intrin.h>

static int __inline __builtin_clz(uint32_t x) {
  int r = 0;
  _BitScanReverse(&r, x);
  return r;
}

static int __inline __builtin_clzll(uint64_t x) {
  int r = 0;
#ifdef _WIN64
  _BitScanReverse64(&r, x);
#else
  if (!_BitScanReverse(&r, (uint32_t)x) && _BitScanReverse(&r, (uint32_t)(x >> 32))) {
    r += 32;
  }
#endif
  return r;
}

static int __inline __builtin_ctz(uint32_t x) {
  int r = 0;
  _BitScanForward(&r, x);
  return r;
}

static int __inline __builtin_ctzll(uint64_t x) {
  int r = 0;
#ifdef _WIN64
  _BitScanForward64(&r, x);
#else
  if (_BitScanForward(&r, (uint32_t)(x >> 32))) {
    r += 32;
  } else {
    _BitScanForward(&r, (uint32_t)x);
  }
#endif
  return r;
}

#endif

static int64_t int_pow(int64_t base, int64_t exponent) {
  int64_t ret = 1;
  int64_t mul[4];
  mul[0] = 1;
  mul[1] = base;
  mul[3] = 1;

  while (exponent != 0) {
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

CAMLprim int64_t Base_int_math_int64_pow_stub_unboxed(int64_t base, int64_t exponent) {
  return int_pow(base, exponent);
}

CAMLprim value Base_int_math_int64_pow_stub(value base, value exponent) {
  CAMLparam2(base, exponent);
  CAMLreturn(caml_copy_int64(
      Base_int_math_int64_pow_stub_unboxed(Int64_val(base), Int64_val(exponent))));
}
