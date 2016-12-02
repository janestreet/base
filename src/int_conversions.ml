open! Import

type 'a int_spec = {
  name : string;
  num_bits : int;
  max : 'a;
  min : 'a;
  to_string : 'a -> string;
  compare : 'a -> 'a -> int;
}

let convert a b a_to_b b_to_a =
  if a.num_bits <= b.num_bits then
    ((fun i -> Some (a_to_b i)), a_to_b)
  else
    let { min = b_min; max = b_max; name = b_name; num_bits=_; to_string=_; compare=_ } = b in
    let min = b_to_a b_min in
    let max = b_to_a b_max in
    let compare = a.compare in
    let is_in_range i = compare min i <= 0 && compare i max <= 0 in
    let convert i = if is_in_range i then Some (a_to_b i) else None in
    let { name = a_name; to_string = a_to_string; num_bits=_; max=_; min=_; compare=_ } = a in
    let convert_exn i =
      if is_in_range i then
        a_to_b i
      else
        Printf.failwithf
          "conversion from %s to %s failed: %s is out of range"
          a_name b_name (a_to_string i) ()
    in
    (convert, convert_exn)
;;

let compare_int (x : int) y = compare x y

let int_num_bits = Caml.Sys.int_size

let () = assert(   int_num_bits = 63
                   || int_num_bits = 31
                   || int_num_bits = 32 )

let int = {
  name = "int";
  num_bits = int_num_bits;
  max = max_int;
  min = min_int;
  to_string = string_of_int;
  compare = compare_int;
}

let int32 = {
  name = "int32";
  num_bits = 32;
  max = Caml.Int32.max_int;
  min = Caml.Int32.min_int;
  to_string = Caml.Int32.to_string;
  compare = Caml.Int32.compare;
}

let int63 = {
  name = "int63";
  num_bits = 63;
  max = Caml.Int64.shift_right Caml.Int64.max_int 1;
  min = Caml.Int64.shift_right Caml.Int64.min_int 1;
  to_string = Caml.Int64.to_string;
  compare = Caml.Int64.compare;
}

let int64 = {
  name = "int64";
  num_bits = 64;
  max = Caml.Int64.max_int;
  min = Caml.Int64.min_int;
  to_string = Caml.Int64.to_string;
  compare = Caml.Int64.compare;
}

let nativeint = {
  name = "nativeint";
  num_bits = Word_size.num_bits Word_size.word_size;
  max = Caml.Nativeint.max_int;
  min = Caml.Nativeint.min_int;
  to_string = Caml.Nativeint.to_string;
  compare = Caml.Nativeint.compare;
}

let (int_to_int32, int_to_int32_exn) =
  convert int int32 Caml.Int32.of_int Caml.Int32.to_int
let (int32_to_int, int32_to_int_exn) =
  convert int32 int Caml.Int32.to_int Caml.Int32.of_int

let int_to_int64 = Caml.Int64.of_int
let (int64_to_int, int64_to_int_exn) =
  convert int64 int Caml.Int64.to_int Caml.Int64.of_int

let int_to_nativeint = Caml.Nativeint.of_int
let (nativeint_to_int, nativeint_to_int_exn) =
  convert nativeint int Caml.Nativeint.to_int Caml.Nativeint.of_int

let int32_to_int64 = Caml.Int64.of_int32
let (int64_to_int32, int64_to_int32_exn) =
  convert int64 int32 Caml.Int64.to_int32 Caml.Int64.of_int32

let int32_to_nativeint = Caml.Nativeint.of_int32
let (nativeint_to_int32, nativeint_to_int32_exn) =
  convert nativeint int32 Caml.Nativeint.to_int32 Caml.Nativeint.of_int32
;;

let (int64_to_nativeint, int64_to_nativeint_exn) =
  convert int64 nativeint Caml.Int64.to_nativeint Caml.Int64.of_nativeint
;;
let nativeint_to_int64 = Caml.Int64.of_nativeint

let int64_fit_on_int63_exn =
  let (_, int64_to_int63_exn) = convert int64 int63 Fn.id Fn.id in
  fun x -> ignore (int64_to_int63_exn x : int64)
;;

let num_bits_int       = int.num_bits
let num_bits_int32     = int32.num_bits
let num_bits_int64     = int64.num_bits
let num_bits_nativeint = nativeint.num_bits

let insert_delimiter_every input ~delimiter ~chars_per_delimiter =
  let input_length = String.length input in
  if input_length <= chars_per_delimiter then
    input
  else begin
    let has_sign = match input.[0] with '+' | '-' -> true | _ -> false in
    let num_digits = if has_sign then input_length - 1 else input_length in
    let num_delimiters = (num_digits - 1) / chars_per_delimiter in
    let output_length = input_length + num_delimiters in
    let output = String.create output_length in
    let input_pos = ref (input_length - 1) in
    let output_pos = ref (output_length - 1) in
    let num_chars_until_delimiter = ref chars_per_delimiter in
    let first_digit_pos = if has_sign then 1 else 0 in
    while !input_pos >= first_digit_pos do
      if !num_chars_until_delimiter = 0 then begin
        output.[!output_pos] <- delimiter;
        decr output_pos;
        num_chars_until_delimiter := chars_per_delimiter;
      end;
      output.[!output_pos] <- input.[!input_pos];
      decr input_pos;
      decr output_pos;
      decr num_chars_until_delimiter;
    done;
    if has_sign then output.[0] <- input.[0];
    output;
  end
;;

let insert_delimiter input ~delimiter =
  insert_delimiter_every input ~delimiter ~chars_per_delimiter:3

let insert_underscores input =
  insert_delimiter input ~delimiter:'_'

let sexp_of_int_style = Sexp.of_int_style

module Make (I : sig
    type t
    val to_string : t -> string
  end) = struct

  open I

  let chars_per_delimiter = 3

  let to_string_hum ?(delimiter='_') t =
    insert_delimiter_every (to_string t) ~delimiter ~chars_per_delimiter

  let sexp_of_t t =
    let s = to_string t in
    Sexp.Atom
      (match !sexp_of_int_style with
       | `Underscores -> insert_delimiter_every s ~chars_per_delimiter ~delimiter:'_'
       | `No_underscores -> s)
  ;;
end

module Make_hex (I : sig
    type t [@@deriving_inline compare, hash]
    include
    sig
      [@@@ocaml.warning "-32"]
      val hash_fold_t :
        Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state
      val hash : t -> Ppx_hash_lib.Std.Hash.hash_value
      val compare : t -> t -> int
    end
    [@@@end]
    val to_string : t -> string
    val of_string : string -> t
    val zero : t
    val (<) : t -> t -> bool
    val neg : t -> t
    val module_name : string
  end) =
struct

  module T_hex = struct

    type t = I.t [@@deriving_inline compare, hash]
    let (hash_fold_t :
           Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
      fun hsv  -> fun arg  -> I.hash_fold_t hsv arg
    let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      fun arg  ->
        Ppx_hash_lib.Std.Hash.get_hash_value
          (hash_fold_t (Ppx_hash_lib.Std.Hash.create ()) arg)

    let compare : t -> t -> int =
      fun a__001_  -> fun b__002_  -> I.compare a__001_ b__002_
    [@@@end]

    let chars_per_delimiter = 4

    let to_string' ?delimiter t =
      let make_suffix =
        match delimiter with
        | None -> I.to_string
        | Some delimiter ->
          (fun t ->
             insert_delimiter_every (I.to_string t) ~delimiter ~chars_per_delimiter)
      in
      if I.(<) t I.zero
      then "-0x" ^ make_suffix (I.neg t)
      else "0x" ^ make_suffix t

    let to_string t = to_string' t ?delimiter:None

    let to_string_hum ?(delimiter='_') t = to_string' t ~delimiter

    let invalid str =
      failwith (Printf.sprintf "%s.of_string: invalid input %S" I.module_name str)

    let of_string_with_delimiter str =
      I.of_string (String.filter str ~f:(fun c -> Char.( <> ) c '_'))

    let of_string str =
      let module L = Hex_lexer in
      match Option.try_with (fun () -> L.parse_hex (Caml.Lexing.from_string str)) with
      | None -> invalid str
      | Some (Neg body) -> I.neg (of_string_with_delimiter body)
      | Some (Pos body) -> of_string_with_delimiter body
  end

  module Hex = struct
    include T_hex
    include Sexpable.Of_stringable(T_hex)
  end

end
