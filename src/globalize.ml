(* The [globalize_{bool,char,unit}] functions are written as matches plus the identity
   function so that the type checker can give them the desired type, without having to do
   anything special.  However, [globalize_int] cannot be written this way, so we resort to
   using an [external]. *)

let globalize_bool = function
  | (true | false) as b -> b
;;

let globalize_char = function
  | '\x00' .. '\xFF' as c -> c
;;

external globalize_float : local_ float -> float = "%obj_dup"
external globalize_int : local_ int -> int = "%identity"
external globalize_int32 : local_ int32 -> int32 = "%obj_dup"
external globalize_int64 : local_ int64 -> int64 = "%obj_dup"
external globalize_nativeint : local_ nativeint -> nativeint = "%obj_dup"
external globalize_bytes : local_ bytes -> bytes = "%obj_dup"
external globalize_string : local_ string -> string = "%obj_dup"

let globalize_unit (() as u) = u

external globalize_array' : local_ 'a array -> 'a array = "%obj_dup"

let globalize_array _ a = globalize_array' a

let[@tail_mod_cons] rec globalize_list f = function
  | [] -> []
  | x :: xs -> f x :: (globalize_list [@tailcall]) f xs
;;

let globalize_option f = function
  | None -> None
  | Some x -> Some (f x)
;;

let globalize_result globalize_a globalize_b t =
  match t with
  | Ok a -> Ok (globalize_a a)
  | Error b -> Error (globalize_b b)
;;

let globalize_ref' r = ref !r
let globalize_ref _ r = globalize_ref' r

external globalize_lazy_t_mono : local_ 'a lazy_t -> 'a lazy_t = "%identity"

let globalize_lazy_t _ t = globalize_lazy_t_mono t
