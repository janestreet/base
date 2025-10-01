open! Import
module Sexp = Sexp0
module String = String0

(* string conversions *)

let insert_delimiter_every input ~delimiter ~chars_per_delimiter =
  let input_length = String.length input in
  if input_length <= chars_per_delimiter
  then input
  else (
    let has_sign =
      match input.[0] with
      | '+' | '-' -> true
      | _ -> false
    in
    let num_digits = if has_sign then input_length - 1 else input_length in
    let num_delimiters = (num_digits - 1) / chars_per_delimiter in
    let output_length = input_length + num_delimiters in
    let output = Bytes.create output_length in
    let input_pos = ref (input_length - 1) in
    let output_pos = ref (output_length - 1) in
    let num_chars_until_delimiter = ref chars_per_delimiter in
    let first_digit_pos = if has_sign then 1 else 0 in
    while !input_pos >= first_digit_pos do
      if !num_chars_until_delimiter = 0
      then (
        Bytes.set output !output_pos delimiter;
        decr output_pos;
        num_chars_until_delimiter := chars_per_delimiter);
      Bytes.set output !output_pos input.[!input_pos];
      decr input_pos;
      decr output_pos;
      decr num_chars_until_delimiter
    done;
    if has_sign then Bytes.set output 0 input.[0];
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:output)
;;

let insert_delimiter input ~delimiter =
  insert_delimiter_every input ~delimiter ~chars_per_delimiter:3
;;

let insert_underscores input = insert_delimiter input ~delimiter:'_'
let sexp_of_int_style = Sexp.of_int_style

module Make (I : sig
  @@ portable
    type t

    val to_string : local_ t -> string
  end) =
struct
  open I

  let chars_per_delimiter = 3

  let to_string_hum ?(delimiter = '_') t =
    insert_delimiter_every (to_string t) ~delimiter ~chars_per_delimiter
  ;;

  let sexp_of_t t =
    let s = to_string t in
    Sexp.Atom
      (match Dynamic.get sexp_of_int_style with
       | `Underscores -> insert_delimiter_every s ~chars_per_delimiter ~delimiter:'_'
       | `No_underscores -> s)
  ;;
end

module Make_hex (I : sig
  @@ portable
    type t : value mod contended portable [@@deriving compare ~localize, hash]

    val to_string : local_ t -> string
    val of_string : string -> t
    val zero : t
    val ( < ) : local_ t -> local_ t -> bool
    val neg : local_ t -> t
    val module_name : string
  end) =
struct
  module T_hex = struct
    type t = I.t [@@deriving compare ~localize, hash]

    let chars_per_delimiter = 4

    let to_string' ?delimiter t =
      let make_suffix =
        match delimiter with
        | None -> I.to_string
        | Some delimiter ->
          fun t -> insert_delimiter_every (I.to_string t) ~delimiter ~chars_per_delimiter
      in
      if I.( < ) t I.zero then "-0x" ^ make_suffix (I.neg t) else "0x" ^ make_suffix t
    ;;

    let to_string t = to_string' t ?delimiter:None
    let to_string_hum ?(delimiter = '_') t = to_string' t ~delimiter

    let invalid str =
      Printf.failwithf "%s.of_string: invalid input %S" I.module_name str ()
    ;;

    let of_string_with_delimiter str =
      I.of_string (String.filter str ~f:(fun c -> Char.( <> ) c '_'))
    ;;

    let of_string str =
      let module L = Hex_lexer in
      let lex = Stdlib.Lexing.from_string str in
      let result = Option.try_with (fun () -> L.parse_hex lex) in
      if lex.lex_curr_pos = lex.lex_buffer_len
      then (
        match result with
        | None -> invalid str
        | Some (Neg body) -> I.neg (of_string_with_delimiter body)
        | Some (Pos body) -> of_string_with_delimiter body)
      else invalid str
    ;;
  end

  module Hex = struct
    include T_hex

    include%template Sexpable.Of_stringable [@modality portable] (T_hex)

    module Hum = struct
      let to_string = to_string_hum
    end
  end
end

module Make_binary (I : sig
  @@ portable
    type t : value mod contended portable
    [@@deriving compare ~localize, equal ~localize, hash]

    val clz : local_ t -> t
    val ( lsr ) : local_ t -> int -> t
    val ( land ) : local_ t -> local_ t -> t
    val to_int_trunc : local_ t -> int
    val num_bits : t
    val zero : t
    val one : t
    val ( - ) : local_ t -> local_ t -> t
  end) =
struct
  module Binary = struct
    type t = I.t [@@deriving compare ~localize, hash]

    let bits t =
      let open I in
      (if equal__local t zero then zero else num_bits - clz t) |> to_int_trunc
    ;;

    let to_string_suffix (t : t) =
      let bits = bits t in
      if bits = 0
      then "0"
      else
        String.init bits ~f:(fun char_index ->
          let bit_index = bits - char_index - 1 in
          let bit = I.((t lsr bit_index) land one) in
          Char.unsafe_of_int (Char.to_int '0' + I.to_int_trunc bit))
        [@nontail]
    ;;

    let to_string (t : t) = "0b" ^ to_string_suffix t

    let to_string_hum ?(delimiter = '_') t =
      "0b" ^ insert_delimiter_every (to_string_suffix t) ~delimiter ~chars_per_delimiter:4
    ;;

    let sexp_of_t (t : t) : Sexp.t = Atom (to_string_hum t)

    module Hum = struct
      let to_string = to_string_hum
    end
  end
end
