type seed = int
type hash_value = int

(* The main non-64-bit systems we are planning to support are JavaScript and WebAssembly.
   js_of_ocaml is treated as a 32-bit platform with 32-bit native integers. All of the
   external [caml_hash*] implementations are all written assuming these 32-bit native ints
   and so using Int32 on the [Non_immediate] path allows for this change to be a runtime
   noop for native and JavaScript. If we were to make this something like [Int63], which
   intuitively feels like a more consistent choice, we would have to change the
   implementations of all of these pretty foundational hash functions in both js and wasm. *)
include Sys.Immediate64.Make (Int) (Int32)

type state = t

let compare_state (x : state) (y : state) : hash_value =
  match repr with
  | Immediate -> Int.compare x y
  | Non_immediate -> Int32.compare x y
;;

let state_to_string (x : state) : string =
  match repr with
  | Immediate -> Int.to_string x
  | Non_immediate -> Int32.to_string x
;;

let create_seeded (x : int) : state =
  match repr with
  | Immediate -> x
  | Non_immediate -> Int32.of_int x
;;

(* onicole: On a 32-bit backend such as Wasm, these functions may allocate. *)
external fold_int64
  :  state
  -> (int64[@unboxed])
  -> state
  = "Base_internalhash_fold_int64" "Base_internalhash_fold_int64_unboxed"
[@@noalloc]

external fold_int : state -> int -> state = "Base_internalhash_fold_int" [@@noalloc]

external fold_float
  :  state
  -> (float[@unboxed])
  -> state
  = "Base_internalhash_fold_float" "Base_internalhash_fold_float_unboxed"
[@@noalloc]

external fold_string : state -> string -> state = "Base_internalhash_fold_string"
[@@noalloc]

external get_hash_value : state -> hash_value = "Base_internalhash_get_hash_value"
[@@noalloc]
