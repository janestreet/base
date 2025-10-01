//Provides: Base_clear_caml_backtrace_pos const
function Base_clear_caml_backtrace_pos(x) {
  return 0;
}

//Provides: Base_caml_exn_is_most_recent_exn const
function Base_caml_exn_is_most_recent_exn(x) {
  return 1;
}

//Provides: Base_int_math_int_pow_stub const
function Base_int_math_int_pow_stub(base, exponent) {
  var one = 1;
  var mul = [one, base, one, one];
  var res = one;
  while (!exponent == 0) {
    mul[1] = (mul[1] * mul[3]) | 0;
    mul[2] = (mul[1] * mul[1]) | 0;
    mul[3] = (mul[2] * mul[1]) | 0;
    res = (res * mul[exponent & 3]) | 0;
    exponent = exponent >> 2;
  }
  return res;
}

//Provides: Base_int_math_int64_pow_stub const
//Requires: caml_int64_mul, caml_int64_is_zero, caml_int64_shift_right_unsigned
//Requires: caml_int64_create_lo_hi, caml_int64_lo32
function Base_int_math_int64_pow_stub(base, exponent) {
  var one = caml_int64_create_lo_hi(1, 0);
  var mul = [one, base, one, one];
  var res = one;
  while (!caml_int64_is_zero(exponent)) {
    mul[1] = caml_int64_mul(mul[1], mul[3]);
    mul[2] = caml_int64_mul(mul[1], mul[1]);
    mul[3] = caml_int64_mul(mul[2], mul[1]);
    res = caml_int64_mul(res, mul[caml_int64_lo32(exponent) & 3]);
    exponent = caml_int64_shift_right_unsigned(exponent, 2);
  }
  return res;
}

//Provides: Base_hash_string mutable
//Requires: caml_hash_exn
function Base_hash_string(s) {
  return caml_hash_exn(1, 1, 0, s)
}
//Provides: Base_hash_double const
//Requires: caml_hash_exn
function Base_hash_double(d) {
  return caml_hash_exn(1, 1, 0, d);
}

//Provides: Base_am_testing const
//Weakdef
function Base_am_testing(x) {
  return 0;
}

//Provides: Base_unsafe_create_local_bytes
//Requires: caml_create_bytes
function Base_unsafe_create_local_bytes(v_len) {
  // This does a redundant bounds check and (since this is
  // javascript) doesn't allocate locally, but that's fine.
  return caml_create_bytes(v_len);
}

//Provides: caml_make_local_vect
//Requires: caml_make_vect
function caml_make_local_vect(v_len, v_elt) {
  // In javascript there's no local allocation.
  return caml_make_vect(v_len, v_elt);
}

//Provides: caml_dummy_obj_is_stack
function caml_dummy_obj_is_stack(x) {
  return 0;
}

//Provides: caml_dummy_succ_scannable_prefix_len
function caml_dummy_succ_scannable_prefix_len(x) {
  return 0;
}

//Provides: Base_caml_modf_positive_float_unboxed_exn
//Requires: caml_invalid_argument
function Base_caml_modf_positive_float_unboxed_exn(a, b) {
  if (b < 0) {
    caml_invalid_argument(`${a} % ${b} in float.ml: modulus should be positive`)
  }
  let m = a % b;
  return m < 0 ? m + b : m;
}

//Provides: Base_caml_modf_positive_float_exn
//Requires: Base_caml_modf_positive_float_unboxed_exn
function Base_caml_modf_positive_float_exn(a, b) {
  return Base_caml_modf_positive_float_unboxed_exn(a, b);
}

//Provides: base_array_unsafe_float_blit
//Requires: caml_array_blit
var base_array_unsafe_float_blit = caml_array_blit

//Provides: Base_string_concat_array
//Requires: caml_ml_string_length, caml_create_bytes, caml_blit_bytes
//Requires: caml_string_of_bytes, caml_string_of_jsstring
function Base_string_concat_array(v_string_array, v_sep) {
  // Arrays have a header element at the beginning, so the indices in this function
  // are off by one. Here, checking for length === 1 means the OCaml array is empty.
  if (v_string_array.length === 1) {
    return caml_string_of_jsstring("");
  }
  const sep_len = caml_ml_string_length(v_sep);
  let string_len = sep_len * (v_string_array.length - 2);
  for (let i = 1; i < v_string_array.length; i++) {
    string_len += caml_ml_string_length(v_string_array[i]);
  }
  const result = caml_create_bytes(string_len);
  let pos = 0;
  for (let i = 1; i < v_string_array.length; i++) {
    if (i !== 1) {
      caml_blit_bytes(v_sep, 0, result, pos, sep_len);
      pos += sep_len;
    }
    const string = v_string_array[i];
    const len = caml_ml_string_length(string);
    caml_blit_bytes(string, 0, result, pos, len);
    pos += len;
  }
  return caml_string_of_bytes(result);
}

// Provides: Base_obj_new_mixed_block
// Requires: caml_obj_block
function Base_obj_new_mixed_block(tag, wosize, scannable) {
  return caml_obj_block(tag, wosize);
}
