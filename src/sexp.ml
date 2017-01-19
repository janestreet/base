(* We do not [open! Import] because [Sexp] is used in [Sexp_conv], which is used in
   [Import]. *)

open! Import0
open Hash.Builtin
open Ppx_compare_lib.Builtin
open Caml.Format

module Char   = Char0
module List   = List0
module String = String0

(** Type of S-expressions *)
type t = Atom of string | List of t list
[@@deriving_inline compare, hash]
let rec (hash_fold_t :
           Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  (fun hsv  ->
     fun arg  ->
       match arg with
       | Atom _a0 ->
         hash_fold_string (Ppx_hash_lib.Std.Hash.fold_int hsv 0) _a0
       | List _a0 ->
         hash_fold_list (fun hsv  -> fun arg  -> hash_fold_t hsv arg)
           (Ppx_hash_lib.Std.Hash.fold_int hsv 1) _a0 : Ppx_hash_lib.Std.Hash.state
       ->
         t ->
       Ppx_hash_lib.Std.Hash.state)

let (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  fun arg  ->
    Ppx_hash_lib.Std.Hash.get_hash_value
      (hash_fold_t (Ppx_hash_lib.Std.Hash.create ()) arg)

let rec compare : t -> t -> int =
  fun a__001_  ->
  fun b__002_  ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else
      (match (a__001_, b__002_) with
       | (Atom _a__003_,Atom _b__004_) -> compare_string _a__003_ _b__004_
       | (Atom _,_) -> (-1)
       | (_,Atom _) -> 1
       | (List _a__005_,List _b__006_) ->
         compare_list compare _a__005_ _b__006_)

[@@@end]

let sexp_of_t t = t
let t_of_sexp t = t

let equal a b = compare a b = 0

exception Of_sexp_error of exn * t

module Printing = struct
  (* Default indentation level for human-readable conversions *)

  let default_indent = ref 1

  (* Escaping of strings used as atoms in S-expressions *)

  let must_escape str =
    let len = String.length str in
    len = 0 ||
    let rec loop ix =
      match str.[ix] with
      | '"' | '(' | ')' | ';' | '\\' -> true
      | '|' -> ix > 0 && let next = ix - 1 in Char.equal str.[next] '#' || loop next
      | '#' -> ix > 0 && let next = ix - 1 in Char.equal str.[next] '|' || loop next
      | '\000' .. '\032' | '\127' .. '\255' -> true
      | _ -> ix > 0 && loop (ix - 1)
    in
    loop (len - 1)

  let escaped s =
    let open String in
    let n = ref 0 in
    for i = 0 to length s - 1 do
      n := !n +
           (match unsafe_get s i with
            | '\"' | '\\' | '\n' | '\t' | '\r' | '\b' -> 2
            | ' ' .. '~' -> 1
            | _ -> 4)
    done;
    if !n = length s then copy s else begin
      let s' = create !n in
      n := 0;
      for i = 0 to length s - 1 do
        begin match unsafe_get s i with
        | ('\"' | '\\') as c ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n c
        | '\n' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'n'
        | '\t' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 't'
        | '\r' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'r'
        | '\b' ->
          unsafe_set s' !n '\\'; incr n; unsafe_set s' !n 'b'
        | (' ' .. '~') as c -> unsafe_set s' !n c
        | c ->
          let a = Char.to_int c in
          unsafe_set s' !n '\\';
          incr n;
          unsafe_set s' !n (Char.of_int_exn (48 + a / 100));
          incr n;
          unsafe_set s' !n (Char.of_int_exn (48 + (a / 10) mod 10));
          incr n;
          unsafe_set s' !n (Char.of_int_exn (48 + a mod 10));
        end;
        incr n
      done;
      s'
    end

  let esc_str str =
    let estr = escaped str in
    let elen = String.length estr in
    let res = String.create (elen + 2) in
    String.blit ~src:estr ~src_pos:0 ~dst:res ~dst_pos:1 ~len:elen;
    res.[0] <- '"';
    res.[elen + 1] <- '"';
    res

  let index_of_newline str start =
    try Some (String.index_from_exn str start '\n')
    with Not_found -> None

  let get_substring str index end_pos_opt =
    let end_pos =
      match end_pos_opt with
      | None -> String.length str
      | Some end_pos -> end_pos
    in
    String.sub str ~pos:index ~len:(end_pos - index)

  let is_one_line str =
    match index_of_newline str 0 with
    | None -> true
    | Some index -> index + 1 = String.length str

  let pp_hum_maybe_esc_str ppf str =
    if not (must_escape str) then
      pp_print_string ppf str
    else if is_one_line str then
      pp_print_string ppf (esc_str str)
    else begin
      let rec loop index =
        let next_newline = index_of_newline str index in
        let next_line = get_substring str index next_newline in
        pp_print_string ppf (escaped next_line);
        match next_newline with
        | None ->  ()
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
      pp_close_box ppf ();
    end

  let mach_maybe_esc_str str =
    if must_escape str then esc_str str else str

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
    | List [] -> pp_print_string ppf "()"; false

  and pp_mach_rest may_need_space ppf = function
    | h :: t ->
      let may_need_space = pp_mach_internal may_need_space ppf h in
      pp_mach_rest may_need_space ppf t
    | [] -> pp_print_string ppf ")"

  let pp_hum ppf sexp = pp_hum_indent !default_indent ppf sexp

  let pp_mach ppf sexp = ignore (pp_mach_internal false ppf sexp)
  let pp = pp_mach

  (* Sexp size *)

  let rec size_loop (v, c as acc) = function
    | Atom str -> v + 1, c + String.length str
    | List lst -> List.fold lst ~init:acc ~f:size_loop

  let size sexp = size_loop (0, 0) sexp

  (* Buffer conversions *)

  let to_buffer_hum ~buf ?(indent = !default_indent) sexp =
    let ppf = Caml.Format.formatter_of_buffer buf in
    Caml.Format.fprintf ppf "%a@?" (pp_hum_indent indent) sexp

  let to_buffer_mach ~buf sexp =
    let rec loop may_need_space = function
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = phys_equal str' str in
        if may_need_space && new_may_need_space then Caml.Buffer.add_char buf ' ';
        Caml.Buffer.add_string buf str';
        new_may_need_space
      | List (h :: t) ->
        Caml.Buffer.add_char buf '(';
        let may_need_space = loop false h in
        loop_rest may_need_space t;
        false
      | List [] -> Caml.Buffer.add_string buf "()"; false
    and loop_rest may_need_space = function
      | h :: t ->
        let may_need_space = loop may_need_space h in
        loop_rest may_need_space t
      | [] -> Caml.Buffer.add_char buf ')' in
    ignore (loop false sexp)

  let to_buffer = to_buffer_mach

  let to_buffer_gen ~buf ~add_char ~add_string sexp =
    let rec loop may_need_space = function
      | Atom str ->
        let str' = mach_maybe_esc_str str in
        let new_may_need_space = phys_equal str' str in
        if may_need_space && new_may_need_space then add_char buf ' ';
        add_string buf str';
        new_may_need_space
      | List (h :: t) ->
        add_char buf '(';
        let may_need_space = loop false h in
        loop_rest may_need_space t;
        false
      | List [] -> add_string buf "()"; false
    and loop_rest may_need_space = function
      | h :: t ->
        let may_need_space = loop may_need_space h in
        loop_rest may_need_space t
      | [] -> add_char buf ')' in
    ignore (loop false sexp)

  (* The maximum size of a thing on the minor heap is 256 words.
     Previously, this size of the returned buffer here was 4096 bytes, which
     caused the Buffer to be allocated on the *major* heap every time.

     According to a simple benchmark by Ron, we can improve performance for
     small s-expressions by a factor of ~4 if we only allocate 1024 bytes
     (128 words + some small overhead) worth of buffer initially.  And one
     can argue that if it's free to allocate strings smaller than 256 words,
     large s-expressions requiring larger expensive buffers won't notice
     the extra two doublings from 1024 bytes to 2048 and 4096. And especially
     performance-sensitive applications to always pass in a larger buffer to
     use. *)
  let buffer () = Caml.Buffer.create 1024

  (* String conversions *)

  let to_string_hum ?indent = function
    | Atom str when (match index_of_newline str 0 with None -> true | Some _ -> false) ->
      mach_maybe_esc_str str
    | sexp ->
      let buf = buffer () in
      to_buffer_hum ?indent sexp ~buf;
      Caml.Buffer.contents buf

  let to_string_mach = function
    | Atom str -> mach_maybe_esc_str str
    | sexp ->
      let buf = buffer () in
      to_buffer_mach sexp ~buf;
      Caml.Buffer.contents buf

  let to_string = to_string_mach
end
include Printing

let of_float_style : [ `Underscores | `No_underscores ] ref = ref `No_underscores
let of_int_style   : [ `Underscores | `No_underscores ] ref = ref `No_underscores

module Private = Printing

let message name fields =
  let rec conv_fields = function
    | [] -> []
    | (fname, fsexp) :: rest ->
      match fname with
      | "" -> fsexp :: conv_fields rest
      | _  -> List [ Atom fname; fsexp ] :: conv_fields rest
  in
  List (Atom name :: conv_fields fields)
