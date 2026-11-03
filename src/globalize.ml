(* The [globalize_{bool,char,unit}] functions are written as matches plus the identity
   function so that the type checker can give them the desired type, without having to do
   anything special. However, [globalize_int] cannot be written this way, so we resort to
   using an [external]. *)

open Basement.Or_null_shim.Export

let%template[@alloc stack] [@inline] maybe_globalize _ value = value
let%template[@alloc heap] [@inline] maybe_globalize globalize value = globalize value

let globalize_bool = function
  | (true | false) as b -> b
;;

let globalize_char = function
  | '\x00' .. '\xFF' as c -> c
;;

external globalize_float : float @ local -> float @@ portable = "%obj_dup"
external globalize_int : int @ local -> int @@ portable = "%identity"
external globalize_int32 : int32 @ local -> int32 @@ portable = "%obj_dup"
external globalize_int64 : int64 @ local -> int64 @@ portable = "%obj_dup"
external globalize_nativeint : nativeint @ local -> nativeint @@ portable = "%obj_dup"
external globalize_bytes : bytes @ local -> bytes @ unique @@ portable = "%obj_dup"
external globalize_string : string @ local -> string @ unique @@ portable = "%obj_dup"

let globalize_unit (() as u) = u

[%%template
[@@@kind k = (base_or_null_with_imm, value_or_null mod external64)]

external globalize_array'
  : ('a : k mod separable).
  'a array @ local -> 'a array
  @@ portable
  = "%obj_dup"

let globalize_array _ a = globalize_array' a [@@kind k]]

external globalize_floatarray
  :  floatarray @ local
  -> floatarray @ unique
  @@ portable
  = "%obj_dup"

let[@tail_mod_cons] rec globalize_list f = function
  | [] -> []
  | x :: xs -> f x :: (globalize_list [@tailcall]) f xs
;;

let globalize_option f = function
  | None -> None
  | Some x -> Some (f x)
;;

let globalize_or_null f = function
  | Null -> Null
  | This x -> This (f x)
;;

let globalize_result globalize_a globalize_b t =
  match t with
  | Ok a -> Ok (globalize_a a)
  | Error b -> Error (globalize_b b)
;;

let globalize_ref' r = ref !r
let globalize_ref _ r = globalize_ref' r

external globalize_lazy_t_mono : 'a lazy_t @ local -> 'a lazy_t @@ portable = "%identity"

let globalize_lazy_t _ t = globalize_lazy_t_mono t
