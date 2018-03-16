
//Provides: Base_int_math_int_popcount
function Base_int_math_int_popcount(v) {
  v = v - ((v >>> 1) & 0x55555555);
  v = (v & 0x33333333) + ((v >>> 2) & 0x33333333);
  return ((v + (v >>> 4) & 0xF0F0F0F) * 0x1010101) >>> 24;
}

//Provides: Base_heap_block_is_heap_block
function Base_heap_block_is_heap_block(x){
  return +(x instanceof Array);
}

//Provides: Base_clear_caml_backtrace_pos
function Base_clear_caml_backtrace_pos(x) {
  return 0
}

//Provides: Base_int_math_int_clz
function Base_int_math_int_clz(x){
  var n = 32;
  var y;
  y = x >>16; if (y != 0) { n = n -16; x = y; }
  y = x >> 8; if (y != 0) { n = n - 8; x = y; }
  y = x >> 4; if (y != 0) { n = n - 4; x = y; }
  y = x >> 2; if (y != 0) { n = n - 2; x = y; }
  y = x >> 1; if (y != 0) return n - 2;
  return n - x;
}

//Provides: Base_int_math_int_pow_stub
function Base_int_math_int_pow_stub(base, exponent){
  var one = 1;
  var mul = [one, base, one, one];
  var res = one;
  while (!exponent==0) {
    mul[1] = (mul[1] * mul[3]) | 0;
    mul[2] = (mul[1] * mul[1]) | 0;
    mul[3] = (mul[2] * mul[1]) | 0;
    res = (res * mul[exponent& 3]) | 0;
    exponent = exponent >> 2;
  }
  return res;
}

//Provides: Base_int_math_int64_pow_stub
//Requires: caml_int64_mul, caml_int64_is_zero, caml_int64_shift_right_unsigned
function Base_int_math_int64_pow_stub(base, exponent){
  var one = [255,1,0,0];
  var mul = [one, base, one, one];
  var res = one;
  while (!caml_int64_is_zero(exponent)) {
    mul[1] = caml_int64_mul(mul[1],mul[3]);
    mul[2] = caml_int64_mul(mul[1],mul[1]);
    mul[3] = caml_int64_mul(mul[2],mul[1]);
    res = caml_int64_mul(res, mul[exponent[1]& 3]);
    exponent = caml_int64_shift_right_unsigned(exponent, 2);
  }
  return res;
}

//Provides: Base_internalhash_fold_int64
//Requires: caml_hash_mix_int64
var Base_internalhash_fold_int64 = caml_hash_mix_int64
//Provides: Base_internalhash_fold_int
//Requires: caml_hash_mix_int
var Base_internalhash_fold_int = caml_hash_mix_int
//Provides: Base_internalhash_fold_float
//Requires: caml_hash_mix_float
var Base_internalhash_fold_float = caml_hash_mix_float
//Provides: Base_internalhash_fold_string
//Requires: caml_hash_mix_string
var Base_internalhash_fold_string = caml_hash_mix_string
//Provides: Base_internalhash_get_hash_value
//Requires: caml_hash_mix_final
function Base_internalhash_get_hash_value (seed) {
  var h = caml_hash_mix_final(seed);
  return h & 0x3FFFFFFF;
}

//Provides: Base_hash_string
//Requires: caml_hash
function Base_hash_string(s) {
  return caml_hash(1,1,0,s)
}
//Provides: Base_hash_double
//Requires: caml_hash
function Base_hash_double(d) {
  return caml_hash(1,1,0,d);
}

//Provides: Base_am_testing
//Weakdef
function Base_am_testing(x) {
  return 0
}
