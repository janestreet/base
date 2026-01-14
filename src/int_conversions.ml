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
[@@cold]
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

include Integer_to_string

(* int <-> int32 *)

let int_to_int32_failure x = convert_failure x "int" "int32" int_to_string

let int32_to_int_failure x =
  convert_failure (globalize_int32 x) "int32" "int" int32_to_string
;;

external int32_to_int_trunc : local_ int32 -> int @@ portable = "%int32_to_int"

external int_to_int32_trunc
  :  local_ int
  -> (int32[@local_opt])
  @@ portable
  = "%int32_of_int"

let int_is_representable_as_int32 @ portable =
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
    fun x -> compare_int32__local min x <= 0 && compare_int32__local x max <= 0)
;;

let int_to_int32 x =
  if int_is_representable_as_int32 x then Some (int_to_int32_trunc x) else None
;;

let int32_to_int x =
  if int32_is_representable_as_int x then Some (int32_to_int_trunc x) else None
;;

let int_to_int32_exn x = exclave_
  if int_is_representable_as_int32 x then int_to_int32_trunc x else int_to_int32_failure x
;;

let int32_to_int_exn x =
  if int32_is_representable_as_int x then int32_to_int_trunc x else int32_to_int_failure x
;;

(* int <-> int64 *)

let[@cold] [@zero_alloc] int64_to_int_failure x =
  convert_failure x "int64" "int" int64_to_string
;;

let () = assert (num_bits_int < num_bits_int64)

external int_to_int64 : local_ int -> (int64[@local_opt]) @@ portable = "%int64_of_int"
external int64_to_int_trunc : local_ int64 -> int @@ portable = "%int64_to_int"

let int64_is_representable_as_int =
  let min = int_to_int64 Int.min_value in
  let max = int_to_int64 Int.max_value in
  fun x -> compare_int64__local min x <= 0 && compare_int64__local x max <= 0
;;

let int64_to_int x =
  if int64_is_representable_as_int x then Some (int64_to_int_trunc x) else None
;;

let int64_to_int_exn x =
  if int64_is_representable_as_int x
  then int64_to_int_trunc x
  else (
    let x =
      Stdlib.Int64.add
        (globalize_int64 x)
        0L (* force int64 boxing to be here under flambda2 *)
    in
    int64_to_int_failure x)
;;

(* int <-> nativeint *)

let nativeint_to_int_failure x =
  convert_failure (globalize_nativeint x) "nativeint" "int" nativeint_to_string
;;

let () = assert (num_bits_int <= num_bits_nativeint)

external int_to_nativeint
  :  local_ int
  -> (nativeint[@local_opt])
  @@ portable
  = "%nativeint_of_int"

external nativeint_to_int_trunc
  :  local_ nativeint
  -> int
  @@ portable
  = "%nativeint_to_int"

let nativeint_is_representable_as_int =
  if num_bits_nativeint <= num_bits_int
  then fun _ -> true
  else (
    let min = int_to_nativeint Int.min_value in
    let max = int_to_nativeint Int.max_value in
    fun x -> compare_nativeint__local min x <= 0 && compare_nativeint__local x max <= 0)
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

let int64_to_int32_failure x =
  convert_failure (globalize_int64 x) "int64" "int32" int64_to_string
;;

let () = assert (num_bits_int32 < num_bits_int64)

external int32_to_int64
  :  local_ int32
  -> (int64[@local_opt])
  @@ portable
  = "%int64_of_int32"

external int64_to_int32_trunc
  :  local_ int64
  -> (int32[@local_opt])
  @@ portable
  = "%int64_to_int32"

include struct
  open struct
    let min = int32_to_int64 min_int32
    let max = int32_to_int64 max_int32
  end

  let int64_is_representable_as_int32 x =
    compare_int64__local min x <= 0 && compare_int64__local x max <= 0
  [@@zero_alloc]
  ;;
end

let int64_to_int32 x =
  if int64_is_representable_as_int32 x then Some (int64_to_int32_trunc x) else None
;;

let int64_to_int32_exn x = exclave_
  if int64_is_representable_as_int32 x
  then int64_to_int32_trunc x
  else int64_to_int32_failure x
;;

(* int32 <-> nativeint *)

let nativeint_to_int32_failure x =
  convert_failure (globalize_nativeint x) "nativeint" "int32" nativeint_to_string
;;

let () = assert (num_bits_int32 <= num_bits_nativeint)

external int32_to_nativeint
  :  local_ int32
  -> (nativeint[@local_opt])
  @@ portable
  = "%nativeint_of_int32"

external nativeint_to_int32_trunc
  :  local_ nativeint
  -> (int32[@local_opt])
  @@ portable
  = "%nativeint_to_int32"

let nativeint_is_representable_as_int32 =
  if num_bits_nativeint <= num_bits_int32
  then fun _ -> true
  else (
    let min = int32_to_nativeint min_int32 in
    let max = int32_to_nativeint max_int32 in
    fun x -> compare_nativeint__local min x <= 0 && compare_nativeint__local x max <= 0)
;;

let nativeint_to_int32 x =
  if nativeint_is_representable_as_int32 x
  then Some (nativeint_to_int32_trunc x)
  else None
;;

let nativeint_to_int32_exn x = exclave_
  if nativeint_is_representable_as_int32 x
  then nativeint_to_int32_trunc x
  else nativeint_to_int32_failure x
;;

(* int64 <-> nativeint *)

let int64_to_nativeint_failure x =
  convert_failure (globalize_int64 x) "int64" "nativeint" int64_to_string
;;

let () = assert (num_bits_int64 >= num_bits_nativeint)

external int64_to_nativeint_trunc
  :  local_ int64
  -> (nativeint[@local_opt])
  @@ portable
  = "%int64_to_nativeint"

external nativeint_to_int64
  :  local_ nativeint
  -> (int64[@local_opt])
  @@ portable
  = "%int64_of_nativeint"

let int64_is_representable_as_nativeint =
  if num_bits_int64 <= num_bits_nativeint
  then fun _ -> true
  else (
    let min = nativeint_to_int64 min_nativeint in
    let max = nativeint_to_int64 max_nativeint in
    fun x -> compare_int64__local min x <= 0 && compare_int64__local x max <= 0)
;;

let int64_to_nativeint x =
  if int64_is_representable_as_nativeint x
  then Some (int64_to_nativeint_trunc x)
  else None
;;

let int64_to_nativeint_exn x = exclave_
  if int64_is_representable_as_nativeint x
  then int64_to_nativeint_trunc x
  else int64_to_nativeint_failure x
;;

(* int64 <-> int63 *)

let int64_to_int63_failure x =
  convert_failure (globalize_int64 x) "int64" "int63" int64_to_string
;;

let int64_is_representable_as_int63 =
  let min = Stdlib.Int64.shift_right min_int64 1 in
  let max = Stdlib.Int64.shift_right max_int64 1 in
  fun x -> compare_int64__local min x <= 0 && compare_int64__local x max <= 0
;;

let int64_fit_on_int63_exn x =
  if int64_is_representable_as_int63 x then () else int64_to_int63_failure x
;;
