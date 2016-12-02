open! Import

module String = String0

include Char0

module T = struct
  type t = char [@@deriving_inline hash, sexp]
  let t_of_sexp : Sexplib.Sexp.t -> t =
    let _tp_loc = "src/char.ml.T.t"  in fun t  -> char_of_sexp t
  let sexp_of_t : t -> Sexplib.Sexp.t = fun v  -> sexp_of_char v
  let (hash_fold_t :
         Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> hash_fold_char hsv arg
  let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_t (Ppx_hash_lib.Std.Hash.create ()) arg)

  [@@@end]

  let compare = compare

  let to_string t = String.make 1 t

  let of_string s =
    match String.length s with
    | 1 -> String.get s 0
    | _ -> failwithf "Char.of_string: %S" s ()
end

include T

include Identifiable.Make (struct
    include T
    let module_name = "Base.Char"
  end)

let all =
  Array.init 256 ~f:unsafe_of_int
  |> Array.to_list

let is_lowercase = function
  | 'a' .. 'z' -> true
  | _ -> false

let is_uppercase = function
  | 'A' .. 'Z' -> true
  | _ -> false

let is_print = function
  | ' ' .. '~' -> true
  | _ -> false

let is_whitespace = function
  | '\t'
  | '\n'
  | '\011' (* vertical tab *)
  | '\012' (* form feed *)
  | '\r'
  | ' '
    -> true
  | _
    -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let is_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | _ -> false

(* Writing these out, instead of calling [is_alpha] and [is_digit], reduces
   runtime by approx. 30% *)
let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> true
  | _ -> false

let get_digit_unsafe t = to_int t - to_int '0'

let get_digit_exn t =
  if is_digit t
  then get_digit_unsafe t
  else failwithf "Char.get_digit_exn %C: not a digit" t ()
;;

let get_digit t = if is_digit t then Some (get_digit_unsafe t) else None
