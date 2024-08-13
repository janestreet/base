(module
   (import "env" "Int32_val" (func $Int32_val (param (ref eq)) (result i32)))
   (import "env" "Int64_val" (func $Int64_val (param (ref eq)) (result i64)))
   (import "env" "Double_val"
      (func $Double_val (param (ref eq)) (result f64)))
   (import "env" "caml_copy_int32"
      (func $caml_copy_int32 (param $i i32) (result (ref eq))))
   (import "env" "caml_hash_mix_int"
      (func $caml_hash_mix_int (param i32) (param i32) (result i32)))
   (import "env" "caml_hash_mix_int64"
      (func $caml_hash_mix_int64 (param i32) (param i64) (result i32)))
   (import "env" "caml_hash_mix_double"
      (func $caml_hash_mix_double (param i32) (param f64) (result i32)))
   (import "env" "caml_hash_mix_string"
      (func $caml_hash_mix_string
         (param i32) (param (ref $string)) (result i32)))
   (import "env" "caml_hash_mix_final"
      (func $caml_hash_mix_final (param i32) (result i32)))

   (type $string (array (mut i8)))
   (type $compare
      (func (param (ref eq)) (param (ref eq)) (param i32) (result i32)))
   (type $hash
      (func (param (ref eq)) (result i32)))
   (type $fixed_length (struct (field $bsize_32 i32) (field $bsize_64 i32)))
   (type $serialize
      (func (param (ref eq)) (param (ref eq)) (result i32) (result i32)))
   (type $deserialize (func (param (ref eq)) (result (ref eq)) (result i32)))
   (type $dup (func (param (ref eq)) (result (ref eq))))
   (type $custom_operations
      (struct
         (field $id (ref $string))
         (field $compare (ref null $compare))
         (field $compare_ext (ref null $compare))
         (field $hash (ref null $hash))
         (field $fixed_length (ref null $fixed_length))
         (field $serialize (ref null $serialize))
         (field $deserialize (ref null $deserialize))
         (field $dup (ref null $dup))))
   (type $custom (sub (struct (field (ref $custom_operations)))))
   (type $int32
      (sub final $custom (struct (field (ref $custom_operations)) (field i32))))

   (func (export "Base_internalhash_fold_int64")
      (param $st (ref eq)) (param $i (ref eq)) (result (ref eq))
      (call $caml_copy_int32
         (call $caml_hash_mix_int64
            (call $Int32_val (local.get $st))
            (call $Int64_val (local.get $i)))))

   (func (export "Base_internalhash_fold_int")
      (param $st (ref eq)) (param $i (ref eq)) (result (ref eq))
      (call $caml_copy_int32
         (call $caml_hash_mix_int
            (call $Int32_val (local.get $st))
            (i31.get_s (ref.cast (ref i31) (local.get $i))))))

   (func (export "Base_internalhash_fold_float")
      (param $st (ref eq)) (param $f (ref eq)) (result (ref eq))
      (call $caml_copy_int32
         (call $caml_hash_mix_double
            (call $Int32_val (local.get $st))
            (call $Double_val (local.get $f)))))

   (func (export "Base_internalhash_fold_string")
      (param $st (ref eq)) (param $s (ref eq)) (result (ref eq))
      (call $caml_copy_int32
         (call $caml_hash_mix_string
            (call $Int32_val (local.get $st))
            (ref.cast (ref $string) (local.get $s)))))

   (func (export "Base_internalhash_get_hash_value")
      (param $st (ref eq)) (result (ref eq))
      (ref.i31
         (i32.and
            (call $caml_hash_mix_final (call $Int32_val (local.get $st)))
            (i32.const 0x3FFFFFFF))))
)
