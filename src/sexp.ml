open! Import

include%template Comparable.Make [@modality portable] (Sexp0)

include Sexp0
(* we include [sexp] last to ensure we get a faster [equal] than the one produced by
   [Comparable.Make] *)

let cross_portable = Basement.Portability_hacks.Cross.Portable.magic
let cross_contended = Basement.Portability_hacks.Cross.Contended.magic

module type Pretty_printing_to_string = Pretty_printing with type output := string

include (
struct
  include Sexplib0.Sexp
  include Sexplib0.Sexp.Private
end :
  Pretty_printing_to_string)

module Utf8_as_string = struct
  (* Sexp printing that does not escape UTF-8 characters. Adapted from [Sexplib0]. The
     structure of what gets escaped has changed, but how we print things out once we
     decide whether to escape them has not. *)

  open Stdlib.Format
  open Int_replace_polymorphic_compare

  module Comment_char = struct
    type t =
      | Bar
      | Hash
  end

  module Kind = struct
    type t =
      | Ascii
      | Backslash of char
      | Comment of Comment_char.t
      | Control
      | Delimiter
      | Space
      | Utf8

    let of_char = function
      | '\n' -> Backslash 'n'
      | '\t' -> Backslash 't'
      | '\r' -> Backslash 'r'
      | '\b' -> Backslash 'b'
      | '\x00' .. '\x1f' -> Control
      | ' ' -> Space
      | '"' -> Backslash '"'
      | '\\' -> Backslash '\\'
      | '#' -> Comment Hash
      | '|' -> Comment Bar
      | '(' | ')' | ';' -> Delimiter
      | '\x21' .. '\x7e' -> Ascii
      | '\x7f' .. '\xff' -> Control
    ;;

    let of_uchar uchar =
      if Uchar.Utf8.byte_length uchar > 1 then Utf8 else of_char (Uchar.to_char_exn uchar)
    ;;

    let byte_length t uchar =
      match t with
      | Ascii | Comment _ | Delimiter | Space -> 1
      | Backslash _ -> 2
      | Control -> 4
      | Utf8 -> Uchar.Utf8.byte_length uchar
    ;;
  end

  let must_escape str =
    String.is_empty str
    || (not (String.Utf8.is_valid str))
    || String.Utf8.of_string_unchecked str
       |> String.Utf8.to_sequence
       |> Sequence.fold_until
            ~init:Kind.Utf8
            ~f:(fun prev uchar ->
              let this = Kind.of_uchar uchar in
              match this, prev with
              | Comment Bar, Comment Hash | Comment Hash, Comment Bar -> Stop true
              | (Backslash _ | Control | Delimiter | Space), _ -> Stop true
              | (Comment _ | Ascii | Utf8), _ -> Continue this)
            ~finish:(fun _ -> false)
  ;;

  let blit_escaped_uchar bytes ~pos ~kind uchar =
    match (kind : Kind.t) with
    | Ascii | Comment _ | Delimiter | Space ->
      Bytes.unsafe_set bytes pos (Uchar.to_char_exn uchar)
    | Backslash char ->
      Bytes.unsafe_set bytes pos '\\';
      Bytes.unsafe_set bytes (pos + 1) char
    | Control ->
      let code = Char.to_int (Uchar.to_char_exn uchar) in
      let zero = 0x30 (* Char.code '0' *) in
      Bytes.unsafe_set bytes pos '\\';
      Bytes.unsafe_set bytes (pos + 1) (Char.unsafe_of_int (zero + (code / 100)));
      Bytes.unsafe_set bytes (pos + 2) (Char.unsafe_of_int (zero + (code / 10 mod 10)));
      Bytes.unsafe_set bytes (pos + 3) (Char.unsafe_of_int (zero + (code mod 10)))
    | Utf8 -> ignore (Bytes.Utf8.set bytes pos uchar : int)
  ;;

  module type Escape = sig @@ portable
    val escaped_length : string -> int
    val blit : bytes -> int -> string -> unit
  end

  module Escape_utf8 : sig
    include Escape
  end = struct
    let escaped_length string =
      let string = String.Utf8.of_string_unchecked string in
      String.Utf8.sum (module Int) string ~f:(fun uchar ->
        Kind.byte_length (Kind.of_uchar uchar) uchar)
    ;;

    let blit bytes pos string =
      let string = String.Utf8.of_string_unchecked string in
      let pos = ref pos in
      String.Utf8.iter string ~f:(fun uchar ->
        let kind = Kind.of_uchar uchar in
        blit_escaped_uchar bytes ~pos:!pos ~kind uchar;
        pos := !pos + Kind.byte_length kind uchar)
    ;;
  end

  module Escape_non_utf8 : sig
    include Escape
  end = struct
    let escaped_length string =
      String.sum (module Int) string ~f:(fun char ->
        Kind.byte_length (Kind.of_char char) (Uchar.of_char char))
    ;;

    let blit bytes pos string =
      let pos = ref pos in
      String.iter string ~f:(fun char ->
        let kind = Kind.of_char char in
        let uchar = Uchar.of_char char in
        blit_escaped_uchar bytes ~pos:!pos ~kind uchar;
        pos := !pos + Kind.byte_length kind uchar)
    ;;
  end

  let escape_without_quotes (module Escape : Escape) string =
    let len = Escape.escaped_length string in
    if len = String.length string
    then string
    else (
      let bytes = Bytes.create len in
      Escape.blit bytes 0 string;
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes)
  ;;

  let escape_with_quotes (module Escape : Escape) string =
    let len = 2 + Escape.escaped_length string in
    let bytes = Bytes.create len in
    Bytes.unsafe_set bytes 0 '"';
    Escape.blit bytes 1 string;
    Bytes.unsafe_set bytes (len - 1) '"';
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let escaped string ~quoted =
    let escape_m : (module Escape) =
      match String.Utf8.is_valid string with
      | true -> (module Escape_utf8)
      | false -> (module Escape_non_utf8)
    in
    match quoted with
    | true -> escape_with_quotes escape_m string
    | false -> escape_without_quotes escape_m string
  ;;

  let index_of_newline str start = String.index_from str start '\n'

  let get_substring str index end_pos_opt =
    let end_pos =
      match end_pos_opt with
      | None -> String.length str
      | Some end_pos -> end_pos
    in
    String.sub str ~pos:index ~len:(end_pos - index)
  ;;

  let is_one_line str =
    match index_of_newline str 0 with
    | None -> true
    | Some index -> index + 1 = String.length str
  ;;

  module Pretty_printing_helpers : Pretty_printing_helpers = struct
    let must_escape = must_escape

    let pp_hum_maybe_esc_str ppf str =
      if not (must_escape str)
      then pp_print_string ppf str
      else if is_one_line str
      then pp_print_string ppf (escaped ~quoted:true str)
      else (
        let rec loop index =
          let next_newline = index_of_newline str index in
          let next_line = get_substring str index next_newline in
          pp_print_string ppf (escaped ~quoted:false next_line);
          match next_newline with
          | None -> ()
          | Some newline_index ->
            pp_print_string ppf "\\";
            pp_force_newline ppf ();
            pp_print_string ppf "\\n";
            loop (newline_index + 1)
        in
        pp_open_box ppf 0;
        (* the leading space is to line up the lines *)
        pp_print_string ppf " \"";
        loop 0;
        pp_print_string ppf "\"";
        pp_close_box ppf ())
    ;;

    let esc_str str = escaped ~quoted:true str
    let mach_maybe_esc_str str = if must_escape str then esc_str str else str

    (* Output of S-expressions to formatters *)

    let rec pp_hum_indent indent ppf = function
      | Atom str -> pp_hum_maybe_esc_str ppf str
      | List (h :: t) ->
        pp_open_box ppf indent;
        pp_print_string ppf "(";
        pp_hum_indent indent ppf h;
        pp_hum_rest indent ppf t
      | List [] -> pp_print_string ppf "()"

    and pp_hum_rest indent ppf = function
      | h :: t ->
        pp_print_space ppf ();
        pp_hum_indent indent ppf h;
        pp_hum_rest indent ppf t
      | [] ->
        pp_print_string ppf ")";
        pp_close_box ppf ()
    ;;

    let rec pp_mach_internal may_need_space ppf = function
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = phys_equal str' str in
        if may_need_space && new_may_need_space then pp_print_string ppf " ";
        pp_print_string ppf str';
        new_may_need_space
      | List (h :: t) ->
        pp_print_string ppf "(";
        let may_need_space = pp_mach_internal false ppf h in
        pp_mach_rest may_need_space ppf t;
        false
      | List [] ->
        pp_print_string ppf "()";
        false

    and pp_mach_rest may_need_space ppf = function
      | h :: t ->
        let may_need_space = pp_mach_internal may_need_space ppf h in
        pp_mach_rest may_need_space ppf t
      | [] -> pp_print_string ppf ")"
    ;;

    let pp_hum ppf sexp = pp_hum_indent (Dynamic.get default_indent) ppf sexp
    let pp_mach ppf sexp = ignore (pp_mach_internal false ppf sexp : bool)
    let pp = pp_mach
  end

  include Pretty_printing_helpers

  (* Buffer conversions *)

  include Make_pretty_printing (Pretty_printing_helpers)
end

module Utf8 = struct
  include Utf8_as_string

  let to_string_hum ?indent ?max_width t =
    to_string_hum ?indent ?max_width t |> String.Utf8.of_string_unchecked
  ;;

  let to_string_mach t = to_string_mach t |> String.Utf8.of_string_unchecked
  let to_string t = to_string t |> String.Utf8.of_string_unchecked
end

module Private = struct
  module Utf8 = Utf8
end
