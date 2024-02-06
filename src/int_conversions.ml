open! Import
module Int = Int0
module Sys = Sys0

let convert_failure x a b to_string =
  Printf.failwithf
    "conversion from %s to %s failed: %s is out of range"
    a
    b
    (to_string x)
    ()
  [@@cold] [@@inline never] [@@local never] [@@specialise never]
;;

let num_bits_int = Sys.int_size_in_bits
let num_bits_int32 = 32
let num_bits_int64 = 64
let num_bits_nativeint = Word_size.num_bits Word_size.word_size
let () = assert (num_bits_int = 63 || num_bits_int = 31 || num_bits_int = 32)
let min_int32 = Stdlib.Int32.min_int
let max_int32 = Stdlib.Int32.max_int
let min_int64 = Stdlib.Int64.min_int
let max_int64 = Stdlib.Int64.max_int
let min_nativeint = Stdlib.Nativeint.min_int
let max_nativeint = Stdlib.Nativeint.max_int
let int_to_string = Stdlib.string_of_int
let int32_to_string = Stdlib.Int32.to_string
let int64_to_string = Stdlib.Int64.to_string
let nativeint_to_string = Stdlib.Nativeint.to_string

(* int <-> int32 *)

let int_to_int32_failure x = convert_failure x "int" "int32" int_to_string
let int32_to_int_failure x = convert_failure x "int32" "int" int32_to_string
let int32_to_int_trunc = Stdlib.Int32.to_int
let int_to_int32_trunc = Stdlib.Int32.of_int

let int_is_representable_as_int32 =
  if num_bits_int <= num_bits_int32
  then fun _ -> true
  else (
    let min = int32_to_int_trunc min_int32 in
    let max = int32_to_int_trunc max_int32 in
    fun x -> compare_int min x <= 0 && compare_int x max <= 0)
;;

let int32_is_representable_as_int =
  if num_bits_int32 <= num_bits_int
  then fun _ -> true
  else (
    let min = int_to_int32_trunc Int.min_value in
    let max = int_to_int32_trunc Int.max_value in
    fun x -> compare_int32 min x <= 0 && compare_int32 x max <= 0)
;;

let int_to_int32 x =
  if int_is_representable_as_int32 x then Some (int_to_int32_trunc x) else None
;;

let int32_to_int x =
  if int32_is_representable_as_int x then Some (int32_to_int_trunc x) else None
;;

let int_to_int32_exn x =
  if int_is_representable_as_int32 x then int_to_int32_trunc x else int_to_int32_failure x
;;

let int32_to_int_exn x =
  if int32_is_representable_as_int x then int32_to_int_trunc x else int32_to_int_failure x
;;

(* int <-> int64 *)

let[@cold] int64_to_int_failure x =
  convert_failure
    (Stdlib.Int64.add x 0L (* force int64 boxing to be here under flambda2 *))
    "int64"
    "int"
    int64_to_string
;;

let () = assert (num_bits_int < num_bits_int64)
let int_to_int64 = Stdlib.Int64.of_int
let int64_to_int_trunc = Stdlib.Int64.to_int

let int64_is_representable_as_int =
  let min = int_to_int64 Int.min_value in
  let max = int_to_int64 Int.max_value in
  fun x -> compare_int64 min x <= 0 && compare_int64 x max <= 0
;;

let int64_to_int x =
  if int64_is_representable_as_int x then Some (int64_to_int_trunc x) else None
;;

let int64_to_int_exn x =
  if int64_is_representable_as_int x then int64_to_int_trunc x else int64_to_int_failure x
;;

(* int <-> nativeint *)

let nativeint_to_int_failure x = convert_failure x "nativeint" "int" nativeint_to_string
let () = assert (num_bits_int <= num_bits_nativeint)
let int_to_nativeint = Stdlib.Nativeint.of_int
let nativeint_to_int_trunc = Stdlib.Nativeint.to_int

let nativeint_is_representable_as_int =
  if num_bits_nativeint <= num_bits_int
  then fun _ -> true
  else (
    let min = int_to_nativeint Int.min_value in
    let max = int_to_nativeint Int.max_value in
    fun x -> compare_nativeint min x <= 0 && compare_nativeint x max <= 0)
;;

let nativeint_to_int x =
  if nativeint_is_representable_as_int x then Some (nativeint_to_int_trunc x) else None
;;

let nativeint_to_int_exn x =
  if nativeint_is_representable_as_int x
  then nativeint_to_int_trunc x
  else nativeint_to_int_failure x
;;

(* int32 <-> int64 *)

let int64_to_int32_failure x = convert_failure x "int64" "int32" int64_to_string
let () = assert (num_bits_int32 < num_bits_int64)
let int32_to_int64 = Stdlib.Int64.of_int32
let int64_to_int32_trunc = Stdlib.Int64.to_int32

let int64_is_representable_as_int32 =
  let min = int32_to_int64 min_int32 in
  let max = int32_to_int64 max_int32 in
  fun x -> compare_int64 min x <= 0 && compare_int64 x max <= 0
;;

let int64_to_int32 x =
  if int64_is_representable_as_int32 x then Some (int64_to_int32_trunc x) else None
;;

let int64_to_int32_exn x =
  if int64_is_representable_as_int32 x
  then int64_to_int32_trunc x
  else int64_to_int32_failure x
;;

(* int32 <-> nativeint *)

let nativeint_to_int32_failure x =
  convert_failure x "nativeint" "int32" nativeint_to_string
;;

let () = assert (num_bits_int32 <= num_bits_nativeint)
let int32_to_nativeint = Stdlib.Nativeint.of_int32
let nativeint_to_int32_trunc = Stdlib.Nativeint.to_int32

let nativeint_is_representable_as_int32 =
  if num_bits_nativeint <= num_bits_int32
  then fun _ -> true
  else (
    let min = int32_to_nativeint min_int32 in
    let max = int32_to_nativeint max_int32 in
    fun x -> compare_nativeint min x <= 0 && compare_nativeint x max <= 0)
;;

let nativeint_to_int32 x =
  if nativeint_is_representable_as_int32 x
  then Some (nativeint_to_int32_trunc x)
  else None
;;

let nativeint_to_int32_exn x =
  if nativeint_is_representable_as_int32 x
  then nativeint_to_int32_trunc x
  else nativeint_to_int32_failure x
;;

(* int64 <-> nativeint *)

let int64_to_nativeint_failure x = convert_failure x "int64" "nativeint" int64_to_string
let () = assert (num_bits_int64 >= num_bits_nativeint)
let int64_to_nativeint_trunc = Stdlib.Int64.to_nativeint
let nativeint_to_int64 = Stdlib.Int64.of_nativeint

let int64_is_representable_as_nativeint =
  if num_bits_int64 <= num_bits_nativeint
  then fun _ -> true
  else (
    let min = nativeint_to_int64 min_nativeint in
    let max = nativeint_to_int64 max_nativeint in
    fun x -> compare_int64 min x <= 0 && compare_int64 x max <= 0)
;;

let int64_to_nativeint x =
  if int64_is_representable_as_nativeint x
  then Some (int64_to_nativeint_trunc x)
  else None
;;

let int64_to_nativeint_exn x =
  if int64_is_representable_as_nativeint x
  then int64_to_nativeint_trunc x
  else int64_to_nativeint_failure x
;;

(* int64 <-> int63 *)

let int64_to_int63_failure x = convert_failure x "int64" "int63" int64_to_string

let int64_is_representable_as_int63 =
  let min = Stdlib.Int64.shift_right min_int64 1 in
  let max = Stdlib.Int64.shift_right max_int64 1 in
  fun x -> compare_int64 min x <= 0 && compare_int64 x max <= 0
;;

let int64_fit_on_int63_exn x =
  if int64_is_representable_as_int63 x then () else int64_to_int63_failure x
;;
