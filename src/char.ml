open! Import
module Array = Array0
module Sexp = Sexp0
module String = String0
include Char0

module T = struct
  type t = char
  [@@deriving compare ~localize, hash, globalize, sexp ~stackify, sexp_grammar]

  let to_string t = String.make 1 t

  let of_string s =
    match String.length s with
    | 1 -> s.[0]
    | _ -> Printf.failwithf "Char.of_string: %S" s ()
  ;;
end

include T

include%template Identifiable.Make [@modality portable] (struct
    include T

    let module_name = "Base.Char"
  end)

let pp fmt c = Stdlib.Format.fprintf fmt "%C" c

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Char_replace_polymorphic_compare

let invariant (_ : t) = ()

let all =
  Array.init 256 ~f:unsafe_of_int
  |> Array.to_list
  |> Portability_hacks.Cross.Portable.(cross (list infer))
;;

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false
;;

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_print = function
  | ' ' .. '~' -> true
  | _ -> false
;;

let is_whitespace = function
  | '\t' | '\n' | '\011' (* vertical tab *) | '\012' (* form feed *) | '\r' | ' ' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false
;;

(* Writing these out, instead of calling [is_alpha] and [is_digit], reduces runtime by
   approx. 30% *)
let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false
;;

let get_digit_unsafe t = to_int t - to_int '0'

let get_digit_exn t =
  if is_digit t
  then get_digit_unsafe t
  else Printf.failwithf "Char.get_digit_exn %C: not a digit" t ()
;;

let get_digit t = if is_digit t then Some (get_digit_unsafe t) else None

let is_hex_digit = function
  | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
  | _ -> false
;;

let is_hex_digit_lower = function
  | '0' .. '9' | 'a' .. 'f' -> true
  | _ -> false
;;

let is_hex_digit_upper = function
  | '0' .. '9' | 'A' .. 'F' -> true
  | _ -> false
;;

let get_hex_digit_exn = function
  | '0' .. '9' as t -> to_int t - to_int '0'
  | 'a' .. 'f' as t -> to_int t - to_int 'a' + 10
  | 'A' .. 'F' as t -> to_int t - to_int 'A' + 10
  | t ->
    Error.raise_s
      (Sexp.message
         "Char.get_hex_digit_exn: not a hexadecimal digit"
         [ "char", sexp_of_t t ])
;;

let get_hex_digit t = if is_hex_digit t then Some (get_hex_digit_exn t) else None

module O = struct
  let ( >= ) = ( >= )
  let ( <= ) = ( <= )
  let ( = ) = ( = )
  let ( > ) = ( > )
  let ( < ) = ( < )
  let ( <> ) = ( <> )
end

module Caseless = struct
  module T = struct
    type t = char [@@deriving sexp ~stackify, sexp_grammar]

    let compare c1 c2 = compare (lowercase c1) (lowercase c2)
    let compare__local c1 c2 = compare c1 c2
    let hash_fold_t state t = hash_fold_char state (lowercase t)
    let hash t = Hash.run hash_fold_t t
  end

  include T

  include%template Comparable.Make [@modality portable] (T)

  let equal__local t1 t2 = equal_int (compare__local t1 t2) 0
end

(* Include type-specific [Replace_polymorphic_compare] at the end, after including functor
   application that could shadow its definitions. This is here so that efficient versions
   of the comparison functions are exported by this module. *)
include Char_replace_polymorphic_compare
