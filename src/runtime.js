
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

//Provides: Base_string_dict_blocks_of_string
//Requires: caml_string_unsafe_get, caml_ml_string_length
function Base_string_dict_blocks_of_string(s) {
  var len = caml_ml_string_length(s);
  var len2 = ((len + 3) >> 2) << 2;
  var u8 = new joo_global_object.Uint8Array(len2);
  for(var i = 0; i < len; i++) u8[i] = caml_string_unsafe_get(s,i);
  var u32 = new joo_global_object.Uint32Array(u8.buffer,0, len2 >> 2);
  return u32;
}

//Provides: Base_string_dict_get_block
function Base_string_dict_get_block(blocks,offset) {
  return blocks[offset] >>> 0;
}

//Provides: Base_string_dict_num_blocks
function Base_string_dict_num_blocks(blocks) {
  return (blocks.length | 0);
}

//Provides: Base_string_dict_make_blocks
function Base_string_dict_make_blocks(blocks){
  var u32 = new joo_global_object.Uint32Array(blocks.length - 1);
  for(var i = 0; i < u32.length; i++){
    u32[i] = blocks[i+1];
  }
  return u32;
}

//Provides: Base_string_dict_find
//Requires: Base_string_dict_blocks_of_string
function Base_string_dict_find(t, key){
  key = Base_string_dict_blocks_of_string(key);
  var num_blocks = key.length;
  var input_blocks_idx = 0;
  for (; num_blocks; num_blocks--) {
    var input_block = key[input_blocks_idx++];
    var keys = t[2]; // Keys(t)
    var a = 0;
    var b = t[1] // num_children(t);
    /* It is likely that the first block is enough to distinguish all
       cases. So the remaining nodes will often form a chain. We
       optimize for this case. */
    if (b == 1) {
      if (input_block == keys[0])
        t = t[3][0+1]; // Child(t, 0)
      else
        return 0;
    } else {
      for (;;) {
        var c;
        var block;
        if (a >= b) return 0;
        c = (a + b) >> 1;
        block = keys[c];
        if (input_block < block)
          b = c;
        else if (input_block > block)
          a = c + 1;
        else {
          t = t[3][c+1]; //Child(t, c);
          break;
        }
      }
    }
  }
  return t[4] // Value(t);
}

//Provides: Base_am_testing
//Weakdef
function Base_am_testing(x) {
  return 0
}
