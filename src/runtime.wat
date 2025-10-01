(module
   (import "env" "caml_copy_int64"
      (func $caml_copy_int64 (param i64) (result (ref eq))))
   (import "env" "caml_copy_int32"
      (func $caml_copy_int32 (param i32) (result (ref eq))))
   (import "env" "caml_floatarray_blit"
      (func $caml_floatarray_blit
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (param (ref eq)) (result (ref eq))))
   (import "env" "Int64_val" (func $Int64_val (param (ref eq)) (result i64)))
   (import "env" "Int32_val" (func $Int32_val (param (ref eq)) (result i32)))
   (import "env" "caml_hash"
      (func $caml_hash
         (param (ref eq)) (param (ref eq)) (param (ref eq)) (param (ref eq))
         (result (ref eq))))
   (import "env" "caml_create_bytes"
      (func $caml_create_bytes (param (ref eq)) (result (ref eq))))
   (import "env" "caml_make_vect"
      (func $caml_make_vect
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "Double_val"
      (func $Double_val (param (ref eq)) (result f64)))
   (import "env" "caml_hash_mix_string"
      (func $caml_hash_mix_string (param i32 (ref $string)) (result i32)))
   (import "env" "caml_hash_mix_final"
      (func $caml_hash_mix_final (param i32) (result i32)))
   (import "env" "caml_hash_mix_double"
      (func $caml_hash_mix_double (param i32 f64) (result i32)))
   (import "Math" "fmod"
      (func $fmod (param f64) (param f64) (result f64)))
   (import "env" "caml_invalid_argument"
      (func $caml_invalid_argument (param (ref eq))))
   (import "env" "caml_string_cat"
      (func $caml_string_cat
         (param (ref eq)) (param (ref eq)) (result (ref eq))))
   (import "env" "caml_obj_block"
      (func $caml_obj_block
         (param (ref eq)) (param (ref eq)) (result (ref eq))))

   (type $string (array (mut i8)))
   (type $float (struct (field f64)))
   (type $block (array (mut (ref eq))))

   (func (export "caml_dummy_obj_is_stack")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "caml_dummy_succ_scannable_prefix_len")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "Base_int_math_int_popcount")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.popcnt (i31.get_u (ref.cast (ref i31) (local.get 0))))))

   (func (export "Base_int_math_int_clz")
      (param (ref eq)) (result (ref eq))
      (ref.i31
         (i32.clz
            (i32.or
               (i32.shl
                  (i31.get_s (ref.cast (ref i31) (local.get 0)))
                  (i32.const 1))
               (i32.const 1)))))

   (export "Base_int_math_nativeint_clz" (func $Base_int_math_int32_clz))
   (func $Base_int_math_int32_clz (export "Base_int_math_int32_clz")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.clz (call $Int32_val (local.get 0)))))

   (func (export "Base_int_math_int64_clz")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.wrap_i64 (i64.clz (call $Int64_val (local.get 0))))))

   (func (export "Base_int_math_int_ctz")
      (param (ref eq)) (result (ref eq))
      (ref.i31
         (i32.ctz (i31.get_s (ref.cast (ref i31) (local.get 0))))))

   (export "Base_int_math_nativeint_ctz" (func $Base_int_math_int32_ctz))
   (func $Base_int_math_int32_ctz (export "Base_int_math_int32_ctz")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.ctz (call $Int32_val (local.get 0)))))

   (func (export "Base_int_math_int64_ctz")
      (param (ref eq)) (result (ref eq))
      (ref.i31
         (i32.wrap_i64
            (i64.ctz (call $Int64_val (local.get 0))))))

   (func (export "Base_int_math_int_pow_stub")
      (param $vbase (ref eq)) (param $vexp (ref eq)) (result (ref eq))
      (local $base i32) (local $exp i32) (local $res i32)
      (local.set $base (i31.get_s (ref.cast (ref i31) (local.get $vbase))))
      (local.set $exp (i31.get_s (ref.cast (ref i31) (local.get $vexp))))
      (local.set $res (i32.const 1))
      (loop $loop
         (if (i32.ne (local.get $exp) (i32.const 0))
            (then
               (if (i32.and (local.get $exp) (i32.const 1))
                  (then
                     (local.set $res
                        (i32.mul (local.get $res) (local.get $base)))))
               (local.set $exp (i32.shr_u (local.get $exp) (i32.const 1)))
               (local.set $base (i32.mul (local.get $base) (local.get $base)))
               (br $loop))))
      (ref.i31 (local.get $res)))

   (func (export "Base_int_math_int64_pow_stub")
      (param $vbase (ref eq)) (param $vexp (ref eq)) (result (ref eq))
      (local $base i64) (local $exp i64) (local $res i64)
      (local.set $base (call $Int64_val (local.get $vbase)))
      (local.set $exp (call $Int64_val (local.get $vexp)))
      (local.set $res (i64.const 1))
      (loop $loop
         (if (i64.ne (local.get $exp) (i64.const 0))
            (then
               (if (i32.wrap_i64 (i64.and (local.get $exp) (i64.const 1)))
                  (then
                     (local.set $res
                        (i64.mul (local.get $res) (local.get $base)))))
               (local.set $exp (i64.shr_u (local.get $exp) (i64.const 1)))
               (local.set $base (i64.mul (local.get $base) (local.get $base)))
               (br $loop))))
      (return_call $caml_copy_int64 (local.get $res)))

   (func (export "Base_clear_caml_backtrace_pos")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 0)))

   (func (export "Base_caml_exn_is_most_recent_exn")
      (param (ref eq)) (result (ref eq))
      (ref.i31 (i32.const 1)))

   (func (export "Base_hash_string") (param $s (ref eq)) (result (ref eq))
      (local $h i32)
      (local.set $h
         (call $caml_hash_mix_string (i32.const 0)
                                     (ref.cast (ref $string) (local.get $s))))
      (ref.i31
         (i32.and (call $caml_hash_mix_final (local.get $h))
                  (i32.const 0x3FFFFFFF))))

   (func (export "Base_hash_double") (param $d (ref eq)) (result (ref eq))
      (local $h i32)
      (local.set $h
         (call $caml_hash_mix_double (i32.const 0)
                                     (call $Double_val (local.get $d))))
      (ref.i31
         (i32.and (call $caml_hash_mix_final (local.get $h))
                  (i32.const 0x3FFFFFFF))))

   (global $Base_am_testing_flag (export "Base_am_testing_flag") (mut i32)
      (i32.const 0))

   (func (export "Base_am_testing") (param (ref eq)) (result (ref eq))
      (ref.i31 (global.get $Base_am_testing_flag)))

   (export "Base_unsafe_create_local_bytes" (func $caml_create_bytes))

   (export "caml_make_local_vect" (func $caml_make_vect))

   (data $modulus_positive "float.ml: modulus should be positive")

   (func $modf_pos_u (param $a f64) (param $b f64) (result f64)
      (local $m f64)
      (if (f64.lt (local.get $b) (f64.const 0))
         (then
            (call $caml_invalid_argument
               (array.new_data $string $modulus_positive
                  (i32.const 0) (i32.const 36)))))
      (local.set $m (call $fmod
         (local.get $a)
         (local.get $b)))
      (if (result f64) (f64.lt (local.get $m) (f64.const 0))
         (then (f64.add (local.get $m) (local.get $b)))
         (else (local.get $m))))

   (func $modf_pos (param $a (ref eq)) (param $b (ref eq)) (result (ref eq))
      (struct.new $float (call $modf_pos_u
         (call $Double_val (local.get $a))
         (call $Double_val (local.get $b)))))

   ;; The unboxed version of this function never gets called, but it needs to
   ;; exist or else the wasm module won't validate. In JavaScript you can just not define
   ;; these and there's no issue since the unboxed impl won't get called. There aren't a
   ;; ton of instances of this that I can find, but in float.ml right under these you can
   ;; see: `external ( ** ) : t -> t -> t = "caml_power_float" "pow" [@@unboxed] [@@noalloc]`
   ;; and both caml_power_float and pow are handled in js_of_ocaml generate. I think the
   ;; unique thing here is that this behavior doesn't align with existing primitives, so
   ;; it'd be a larger set of changes to js_of_ocaml source to support directly there, and
   ;; since these primitives are Base specific, it feels more appropriate to write here
   ;; and include the unboxed version correctly working.
   (export "Base_caml_modf_positive_float_unboxed_exn" (func $modf_pos_u))
   (export "Base_caml_modf_positive_float_exn" (func $modf_pos))

   (func (export "caml_float_min")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (if (result (ref eq))
          (f64.lt
             (call $Double_val (local.get $x))
             (call $Double_val (local.get $y)))
         (then (local.get $x))
         (else (local.get $y))))

   (func (export "caml_float_max")
      (param $x (ref eq)) (param $y (ref eq)) (result (ref eq))
      (if (result (ref eq))
          (f64.gt
             (call $Double_val (local.get $x))
             (call $Double_val (local.get $y)))
         (then (local.get $x))
         (else (local.get $y))))

   (export "base_array_unsafe_float_blit" (func $caml_floatarray_blit))

   (func (export "Base_string_concat_array")
      (param $str_array (ref eq)) (param $sep_ref (ref eq)) (result (ref eq))
      (local $i i32)
      (local $len i32)
      (local $b (ref $block))
      (local $v (ref $string))
      (local $sep (ref $string))
      (local $str (ref $string))
      (local $total_len i32)
      (local $sep_len i32)
      (local $offset i32)
      (local $local_len i32)
      (local.set $sep (ref.cast (ref $string) (local.get $sep_ref)))
      (local.set $sep_len (array.len (local.get $sep)))
      (local.set $b (ref.cast (ref $block) (local.get $str_array)))
      (local.set $len (array.len (local.get $b)))
      (local.set $i (i32.const 1))
      (local.set $total_len (i32.const 0))
      (loop $compute_length
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $v (ref.cast
                  (ref $string)
                  (array.get $block (local.get $b) (local.get $i))))
               (local.set $total_len
                  (i32.add
                     (local.get $total_len)
                     (array.len (local.get $v))))
               (if (i32.lt_s (local.get $i) (i32.sub (local.get $len) (i32.const 1)))
                  (then
                  (local.set $total_len
                     (i32.add
                        (local.get $total_len)
                        (local.get $sep_len)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $compute_length))))

      (local.set $offset (i32.const 0))
      (local.set $local_len (i32.const 0))
      (local.set $i (i32.const 1))
      (local.set $str (array.new $string (i32.const 0) (local.get $total_len)))
      (loop $loop
         (if (i32.lt_s (local.get $i) (local.get $len))
            (then
               (local.set $v
                  (ref.cast
                     (ref $string)
                     (array.get $block (local.get $b) (local.get $i))))
               (local.set $local_len (array.len (local.get $v)))
               (array.copy $string $string
                  (local.get $str)
                  (local.get $offset)
                  (local.get $v)
                  (i32.const 0)
                  (local.get $local_len))
               (local.set $offset (i32.add (local.get $offset) (local.get $local_len)))
               (if (i32.lt_s (local.get $i) (i32.sub (local.get $len) (i32.const 1)))
                  (then
                     (array.copy $string $string
                        (local.get $str)
                        (local.get $offset)
                        (local.get $sep)
                        (i32.const 0)
                        (local.get $sep_len))
                     (local.set $offset (i32.add (local.get $offset) (local.get $sep_len)))))
               (local.set $i (i32.add (local.get $i) (i32.const 1)))
               (br $loop))))
      (local.get $str))

   (func (export "Base_obj_new_mixed_block")
      (param $tag (ref eq)) (param $wosize (ref eq)) (param $scannable (ref eq))
      (result (ref eq))
      (call $caml_obj_block (local.get $tag) (local.get $wosize)))
)
