open! Import
module Array = Array0
module Bytes = Bytes0
module Int = Int0
module Sexp = Sexp0
module Uchar = Uchar0
module String = String0
include String_intf.Definitions
include String

let invalid_argf = Printf.invalid_argf
let raise_s = Error.raise_s
let%template stage = (Staged.stage [@mode p]) [@@mode p = (nonportable, portable)]

module T = struct
  type t = string [@@deriving globalize, hash, sexp ~stackify, sexp_grammar]

  let hashable : t Hashable.t = { hash; compare; sexp_of_t }
  let compare = compare
end

include T

include%template Comparator.Make [@modality portable] (T)

type elt = char

let invariant (_ : t) = ()

[%%template
[@@@alloc.default a @ l = (heap @ global, stack @ local)]

(* This is copied/adapted from 'blit.ml'. [sub], [subo] could be implemented using
   [Blit.Make(Bytes)] plus unsafe casts to/from string but were inlined here to avoid
   using [Bytes.unsafe_of_string] as much as possible.
*)
let unsafe_sub src ~pos ~len =
  if len = 0
  then ""
  else (
    (let dst = (Bytes.create [@alloc a]) len in
     Bytes.unsafe_blit_string ~src ~src_pos:pos ~dst ~dst_pos:0 ~len;
     Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst)
    [@exclave_if_stack a])
;;

let sub src ~pos ~len =
  if pos = 0 && len = String.length src
  then src
  else (
    Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length:(length src);
    (unsafe_sub [@alloc a]) src ~pos ~len [@exclave_if_stack a])
;;

let subo ?(pos = 0) ?len src =
  (sub [@alloc a])
    src
    ~pos
    ~len:
      (match len with
       | Some i -> i
       | None -> length src - pos) [@exclave_if_stack a]
;;]

let rec contains_unsafe t ~pos ~end_ char =
  pos < end_
  && (Char.equal (unsafe_get t pos) char || contains_unsafe t ~pos:(pos + 1) ~end_ char)
;;

let contains ?(pos = 0) ?len t char =
  let total_length = String.length t in
  let len = Option.value len ~default:(total_length - pos) in
  Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length;
  contains_unsafe t ~pos ~end_:(pos + len) char
;;

let is_empty t = length t = 0

let[@inline] index_from_internal string ~len ~(local_ not_found) ~(local_ found) char ~pos
  =
  let rec local_ loop ~pos =
    if pos >= len
    then not_found ()
    else if Char.equal (unsafe_get string pos) char
    then found pos
    else loop ~pos:(pos + 1)
  in
  loop ~pos [@nontail]
;;

let index t char =
  index_from_internal
    t
    char
    ~pos:0
    ~len:(length t)
    ~found:Option.some
    ~not_found:(local_ fun () -> None)
  [@nontail]
;;

let index_exn t char =
  index_from_internal
    t
    ~pos:0
    ~len:(length t)
    ~found:Fn.id
    ~not_found:(local_ fun () -> raise (Not_found_s (Atom "String.index_exn: not found")))
    char [@nontail]
;;

let index_from t pos char =
  index_from_internal
    t
    char
    ~pos
    ~len:(length t)
    ~found:Option.some
    ~not_found:(local_ fun () -> None)
  [@nontail]
;;

let index_from_exn =
  let not_found () = raise (Not_found_s (Atom "String.index_from_exn: not found")) in
  let index_from_exn t pos char =
    let len = length t in
    if pos < 0 || pos > len
    then invalid_arg "String.index_from_exn"
    else index_from_internal t ~pos ~len ~not_found ~found:Fn.id char
  in
  (* named to preserve symbol in compiled binary *)
  index_from_exn
;;

let[@inline] rindex_from_internal string char ~(local_ found) ~(local_ not_found) ~pos =
  let rec local_ loop ~pos =
    if pos < 0
    then not_found ()
    else if Char.equal (unsafe_get string pos) char
    then found pos
    else loop ~pos:(pos - 1)
  in
  loop ~pos [@nontail]
;;

let rindex t char =
  rindex_from_internal
    t
    char
    ~pos:(length t - 1)
    ~found:Option.some
    ~not_found:(local_ fun () -> None) [@nontail]
;;

let rindex_exn t char =
  rindex_from_internal
    t
    char
    ~pos:(length t - 1)
    ~found:Fn.id
    ~not_found:(local_ fun () ->
      raise (Not_found_s (Atom "String.rindex_exn: not found"))) [@nontail]
;;

let rindex_from t pos char =
  rindex_from_internal t char ~pos ~found:Option.some ~not_found:(local_ fun () -> None)
  [@nontail]
;;

let rindex_from_exn =
  let not_found () = raise (Not_found_s (Atom "String.rindex_from_exn: not found")) in
  let rindex_from_exn t pos char =
    if pos < -1 || pos >= length t
    then invalid_arg "String.rindex_from_exn"
    else rindex_from_internal t ~pos ~not_found ~found:Fn.id char
  in
  (* named to preserve symbol in compiled binary *)
  rindex_from_exn
;;

module Search_pattern0 = struct
  type t =
    { pattern : string
    ; case_sensitive : bool
    ; kmp_array : int iarray
    }

  let%template[@alloc a = (heap, stack)] sexp_of_t
    { pattern; case_sensitive; kmp_array = _ }
    : Sexp.t
    =
    List
      [ List [ Atom "pattern"; (sexp_of_string [@alloc a]) pattern ]
      ; List [ Atom "case_sensitive"; (sexp_of_bool [@alloc a]) case_sensitive ]
      ]
    [@exclave_if_stack a]
  ;;

  let%template[@mode l = (global, local)] pattern t = t.pattern
  let case_sensitive t = t.case_sensitive

  (* Find max number of matched characters at [next_text_char], given the current
     [matched_chars]. Try to extend the current match, if chars don't match, try to match
     fewer chars. If chars match then extend the match. *)
  let kmp_internal_loop ~matched_chars ~next_text_char ~pattern ~kmp_array ~char_equal =
    let matched_chars = ref matched_chars in
    while
      !matched_chars > 0
      && not (char_equal next_text_char (unsafe_get pattern !matched_chars))
    do
      matched_chars := Iarray0.unsafe_get kmp_array (!matched_chars - 1)
    done;
    if char_equal next_text_char (unsafe_get pattern !matched_chars)
    then matched_chars := !matched_chars + 1;
    !matched_chars
  ;;

  let get_char_equal ~case_sensitive =
    match case_sensitive with
    | true -> Char.equal
    | false -> Char.Caseless.equal
  ;;

  (* Classic KMP pre-processing of the pattern: build the int array, which, for each i,
     contains the length of the longest non-trivial prefix of s which is equal to a suffix
     ending at s.[i] *)
  let%template[@alloc a = (heap, stack)] create pattern ~case_sensitive =
    (let n = length pattern in
     let kmp_array = (Array.create [@alloc a]) ~len:n (-1) in
     if n > 0
     then (
       let char_equal = get_char_equal ~case_sensitive in
       Array.unsafe_set kmp_array 0 0;
       let matched_chars = ref 0 in
       for i = 1 to n - 1 do
         matched_chars
         := kmp_internal_loop
              ~matched_chars:!matched_chars
              ~next_text_char:(unsafe_get pattern i)
              ~pattern
              ~kmp_array:
                ((* The array won't be mutated for the duration of this function call, and
                    the function doesn't save the iarray reference. *)
                 Iarray0.unsafe_of_array__promise_no_mutation
                   kmp_array)
              ~char_equal;
         Array.unsafe_set kmp_array i !matched_chars
       done);
     { pattern
     ; case_sensitive
     ; kmp_array =
         (* The array won't be mutated from now on. *)
         Iarray0.unsafe_of_array__promise_no_mutation kmp_array
     })
    [@exclave_if_stack a]
  ;;

  (* Classic KMP: use the pre-processed pattern to optimize look-behinds on non-matches.
     We return int to avoid allocation in [index_exn]. -1 means no match. *)
  let index_internal ?(pos = 0) { pattern; case_sensitive; kmp_array } ~in_:text =
    if pos < 0 || pos > length text - length pattern
    then -1
    else (
      let char_equal = get_char_equal ~case_sensitive in
      let j = ref pos in
      let matched_chars = ref 0 in
      let k = length pattern in
      let n = length text in
      while !j < n && !matched_chars < k do
        let next_text_char = unsafe_get text !j in
        matched_chars
        := kmp_internal_loop
             ~matched_chars:!matched_chars
             ~next_text_char
             ~pattern
             ~kmp_array
             ~char_equal;
        j := !j + 1
      done;
      if !matched_chars = k then !j - k else -1)
  ;;

  let matches t str = index_internal t ~in_:str >= 0

  let index ?pos t ~in_ =
    let p = index_internal ?pos t ~in_ in
    if p < 0 then None else Some p
  ;;

  let index_exn ?pos t ~in_ =
    let p = index_internal ?pos t ~in_ in
    if p >= 0
    then p
    else
      raise_s
        (Sexp.message "Substring not found" [ "substring", sexp_of_string t.pattern ])
  ;;

  let index_all { pattern; case_sensitive; kmp_array } ~may_overlap ~in_:text =
    if length pattern = 0
    then List.init (1 + length text) ~f:Fn.id
    else (
      let char_equal = get_char_equal ~case_sensitive in
      let matched_chars = ref 0 in
      let k = length pattern in
      let n = length text in
      let found = ref [] in
      for j = 0 to n do
        if !matched_chars = k
        then (
          found := (j - k) :: !found;
          (* we just found a match in the previous iteration *)
          match may_overlap with
          | true -> matched_chars := Iarray0.unsafe_get kmp_array (k - 1)
          | false -> matched_chars := 0);
        if j < n
        then (
          let next_text_char = unsafe_get text j in
          matched_chars
          := kmp_internal_loop
               ~matched_chars:!matched_chars
               ~next_text_char
               ~pattern
               ~kmp_array
               ~char_equal)
      done;
      List.rev !found)
  ;;

  [%%template
  [@@@alloc.default a = (heap, stack)]

  let replace_first ?pos t ~in_:s ~with_ =
    match index ?pos t ~in_:s with
    | None -> s
    | Some i ->
      (let len_s = length s in
       let len_t = length t.pattern in
       let len_with = length with_ in
       let dst = (Bytes.create [@alloc a]) (len_s + len_with - len_t) in
       Bytes.blit_string ~src:s ~src_pos:0 ~dst ~dst_pos:0 ~len:i;
       Bytes.blit_string ~src:with_ ~src_pos:0 ~dst ~dst_pos:i ~len:len_with;
       Bytes.blit_string
         ~src:s
         ~src_pos:(i + len_t)
         ~dst
         ~dst_pos:(i + len_with)
         ~len:(len_s - i - len_t);
       Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst)
      [@exclave_if_stack a]
  ;;

  let replace_all t ~in_:s ~with_ =
    let matches = index_all t ~may_overlap:false ~in_:s in
    match matches with
    | [] -> s
    | _ :: _ ->
      (let len_s = length s in
       let len_t = length t.pattern in
       let len_with = length with_ in
       let num_matches = List.length matches in
       let dst = (Bytes.create [@alloc a]) (len_s + ((len_with - len_t) * num_matches)) in
       let next_dst_pos = ref 0 in
       let next_src_pos = ref 0 in
       List.iter matches ~f:(fun i ->
         let len = i - !next_src_pos in
         Bytes.blit_string ~src:s ~src_pos:!next_src_pos ~dst ~dst_pos:!next_dst_pos ~len;
         Bytes.blit_string
           ~src:with_
           ~src_pos:0
           ~dst
           ~dst_pos:(!next_dst_pos + len)
           ~len:len_with;
         next_dst_pos := !next_dst_pos + len + len_with;
         next_src_pos := !next_src_pos + len + len_t);
       Bytes.blit_string
         ~src:s
         ~src_pos:!next_src_pos
         ~dst
         ~dst_pos:!next_dst_pos
         ~len:(len_s - !next_src_pos);
       Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst)
      [@exclave_if_stack a]
  ;;]

  let split_on t s =
    let pattern_len = String.length t.pattern in
    let matches = index_all t ~may_overlap:false ~in_:s in
    List.map2_exn
      (-pattern_len :: matches)
      (matches @ [ String.length s ])
      ~f:(fun i j -> sub s ~pos:(i + pattern_len) ~len:(j - i - pattern_len))
  ;;

  module Private = struct
    type public = t

    type nonrec t = t =
      { pattern : string
      ; case_sensitive : bool
      ; kmp_array : int Iarray0.t
      }
    [@@deriving equal ~localize, sexp_of ~stackify]

    let representation = Fn.id
  end
end

module Search_pattern_helper = struct
  module Search_pattern = Search_pattern0
end

open Search_pattern_helper

let%template substr_index_gen ~case_sensitive ?pos t ~pattern =
  Search_pattern.index
    ?pos
    ((Search_pattern.create [@alloc stack]) ~case_sensitive pattern)
    ~in_:t [@nontail]
;;

let substr_index_exn_gen ~case_sensitive ?pos t ~pattern =
  Search_pattern.index_exn ?pos (Search_pattern.create ~case_sensitive pattern) ~in_:t
;;

let%template substr_index_all_gen ~case_sensitive t ~may_overlap ~pattern =
  Search_pattern.index_all
    ((Search_pattern.create [@alloc stack]) ~case_sensitive pattern)
    ~may_overlap
    ~in_:t [@nontail]
;;

[%%template
[@@@alloc.default a = (heap, stack)]

let substr_replace_first_gen ~case_sensitive ?pos t ~pattern ~with_ =
  (Search_pattern.replace_first [@alloc a])
    ?pos
    ((Search_pattern.create [@alloc stack]) ~case_sensitive pattern)
    ~in_:t
    ~with_ [@exclave_if_stack a] [@nontail]
;;

let substr_replace_all_gen ~case_sensitive t ~pattern ~with_ =
  (Search_pattern.replace_all [@alloc a])
    ((Search_pattern.create [@alloc stack]) ~case_sensitive pattern)
    ~in_:t
    ~with_ [@exclave_if_stack a] [@nontail]
;;]

let is_substring_gen ~case_sensitive t ~substring =
  Option.is_some (substr_index_gen t ~pattern:substring ~case_sensitive)
;;

let substr_index = substr_index_gen ~case_sensitive:true
let substr_index_exn = substr_index_exn_gen ~case_sensitive:true
let substr_index_all = substr_index_all_gen ~case_sensitive:true

[%%template
[@@@alloc.default a = (heap, stack)]

let substr_replace_first = (substr_replace_first_gen [@alloc a]) ~case_sensitive:true
let substr_replace_all = (substr_replace_all_gen [@alloc a]) ~case_sensitive:true]

let is_substring = is_substring_gen ~case_sensitive:true

let is_substring_at_gen =
  let rec loop ~str ~str_pos ~sub ~sub_pos ~sub_len ~char_equal =
    if sub_pos = sub_len
    then true
    else if char_equal (unsafe_get str str_pos) (unsafe_get sub sub_pos)
    then loop ~str ~str_pos:(str_pos + 1) ~sub ~sub_pos:(sub_pos + 1) ~sub_len ~char_equal
    else false
  in
  fun str ~pos:str_pos ~substring:sub ~char_equal ->
    let str_len = length str in
    let sub_len = length sub in
    if str_pos < 0 || str_pos > str_len
    then
      invalid_argf
        "String.is_substring_at: invalid index %d for string of length %d"
        str_pos
        str_len
        ();
    str_pos + sub_len <= str_len
    && loop ~str ~str_pos ~sub ~sub_pos:0 ~sub_len ~char_equal
;;

let is_suffix_gen string ~suffix ~char_equal =
  let string_len = length string in
  let suffix_len = length suffix in
  string_len >= suffix_len
  && is_substring_at_gen
       string
       ~pos:(string_len - suffix_len)
       ~substring:suffix
       ~char_equal
;;

let is_prefix_gen string ~prefix ~char_equal =
  let string_len = length string in
  let prefix_len = length prefix in
  string_len >= prefix_len
  && is_substring_at_gen string ~pos:0 ~substring:prefix ~char_equal
;;

module Caseless = struct
  module T = struct
    type t = string [@@deriving sexp ~stackify, sexp_grammar]

    module Fast_equality = struct
      (* equality can generate much faster code than comparison because we can check the
         lengths up front and use fewer conditionals in general *)
      let char_equal_caseless c1 c2 = Char.equal (Char.lowercase c1) (Char.lowercase c2)

      let rec equal_loop ~pos ~len ~string1 ~string2 =
        pos = len
        || (char_equal_caseless (unsafe_get string1 pos) (unsafe_get string2 pos)
            && equal_loop ~pos:(pos + 1) ~len ~string1 ~string2)
      ;;

      let equal__local string1 string2 =
        phys_equal string1 string2
        ||
        let len1 = String.length string1 in
        let len2 = String.length string2 in
        len1 = len2 && equal_loop ~pos:0 ~len:len1 ~string1 ~string2
      ;;

      let equal a b = equal__local a b
      let ( = ) a b = equal__local a b
      let ( <> ) a b = not (equal__local a b)
    end

    let char_compare_caseless c1 c2 = Char.compare (Char.lowercase c1) (Char.lowercase c2)

    let rec compare_loop ~pos ~string1 ~len1 ~string2 ~len2 =
      if pos = len1
      then if pos = len2 then 0 else -1
      else if pos = len2
      then 1
      else (
        let c = char_compare_caseless (unsafe_get string1 pos) (unsafe_get string2 pos) in
        match c with
        | 0 -> compare_loop ~pos:(pos + 1) ~string1 ~len1 ~string2 ~len2
        | _ -> c)
    ;;

    let compare__local string1 string2 =
      if phys_equal string1 string2
      then 0
      else
        compare_loop
          ~pos:0
          ~string1
          ~len1:(String.length string1)
          ~string2
          ~len2:(String.length string2)
    ;;

    let compare a b = compare__local a b

    let hash_fold_t state t =
      let len = length t in
      let state = ref (hash_fold_int state len) in
      for pos = 0 to len - 1 do
        state := hash_fold_char !state (Char.lowercase (unsafe_get t pos))
      done;
      !state
    ;;

    let hash t = Hash.run hash_fold_t t
    let is_suffix s ~suffix = is_suffix_gen s ~suffix ~char_equal:Char.Caseless.equal
    let is_prefix s ~prefix = is_prefix_gen s ~prefix ~char_equal:Char.Caseless.equal
    let substr_index = substr_index_gen ~case_sensitive:false
    let substr_index_exn = substr_index_exn_gen ~case_sensitive:false
    let substr_index_all = substr_index_all_gen ~case_sensitive:false

    [%%template
    [@@@alloc.default a = (heap, stack)]

    let substr_replace_first = (substr_replace_first_gen [@alloc a]) ~case_sensitive:false
    let substr_replace_all = (substr_replace_all_gen [@alloc a]) ~case_sensitive:false]

    let is_substring = is_substring_gen ~case_sensitive:false
    let is_substring_at = is_substring_at_gen ~char_equal:Char.Caseless.equal
  end

  include T

  include%template Comparable.Make [@modality portable] (T)

  (* we define equality after [Comparable.Make] so that we expose our faster version of
     equality instead of the version based on [compare] *)
  include Fast_equality
end

let of_string = Fn.id
let to_string = Fn.id

let%template[@alloc a @ l = (heap @ global, stack @ local)] to_list =
  let rec loop s acc i =
    if i < 0 then acc else loop s (s.[i] :: acc) (i - 1) [@exclave_if_stack a]
  in
  fun s -> loop s [] (length s - 1) [@exclave_if_stack a]
;;

let to_list_rev s =
  let len = length s in
  let rec loop acc i = if i = len then acc else loop (s.[i] :: acc) (i + 1) in
  loop [] 0
;;

let rev t =
  let len = length t in
  let res = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set res i (unsafe_get t (len - 1 - i))
  done;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res
;;

(** Efficient string splitting *)

let lsplit2_exn =
  let not_found () = raise (Not_found_s (Atom "String.lsplit2_exn: not found")) in
  let lsplit2_exn line ~on:delim =
    let len = length line in
    let pos = index_from_internal line ~pos:0 ~len ~not_found ~found:Fn.id delim in
    sub line ~pos:0 ~len:pos, sub line ~pos:(pos + 1) ~len:(len - pos - 1)
  in
  (* named to preserve symbol in compiled binary *)
  lsplit2_exn
;;

let rsplit2_exn =
  let not_found () = raise (Not_found_s (Atom "String.rsplit2_exn: not found")) in
  let rsplit2_exn line ~on:delim =
    let len = length line in
    let pos = rindex_from_internal line ~pos:(len - 1) ~not_found ~found:Fn.id delim in
    sub line ~pos:0 ~len:pos, sub line ~pos:(pos + 1) ~len:(len - pos - 1)
  in
  (* named to preserve symbol in compiled binary *)
  rsplit2_exn
;;

let lsplit2 line ~on =
  try Some (lsplit2_exn line ~on) with
  | Not_found_s _ | Stdlib.Not_found -> None
;;

let rsplit2 line ~on =
  try Some (rsplit2_exn line ~on) with
  | Not_found_s _ | Stdlib.Not_found -> None
;;

let rec char_list_mem l (c : char) =
  match l with
  | [] -> false
  | hd :: tl -> Char.equal hd c || char_list_mem tl c
;;

[%%template
[@@@alloc.default a = (heap, stack)]

let split_gen str ~on =
  let is_delim =
    match on with
    | `char c' -> fun c -> Char.equal c c'
    | `char_list l -> fun c -> char_list_mem l c
  in
  let len = length str in
  (let rec loop acc last_pos pos =
     (if pos = -1
      then (sub [@alloc a]) str ~pos:0 ~len:last_pos :: acc
      else if is_delim str.[pos]
      then (
        let pos1 = pos + 1 in
        let sub_str = (sub [@alloc a]) str ~pos:pos1 ~len:(last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1))
      else loop acc last_pos (pos - 1))
     [@exclave_if_stack a]
   in
   loop [] len (len - 1))
  [@exclave_if_stack a]
;;

let split str ~on = (split_gen [@alloc a]) str ~on:(`char on) [@exclave_if_stack a]

let split_on_chars str ~on:chars =
  (split_gen [@alloc a]) str ~on:(`char_list chars) [@exclave_if_stack a]
;;]

let is_suffix s ~suffix = is_suffix_gen s ~suffix ~char_equal:Char.equal
let is_prefix s ~prefix = is_prefix_gen s ~prefix ~char_equal:Char.equal

let is_substring_at s ~pos ~substring =
  is_substring_at_gen s ~pos ~substring ~char_equal:Char.equal
;;

(** precondition: when [0 <= n <= length t], [~pos] and [~len] are both in-bounds *)
let wrap_sub_n t n ~name ~pos ~len ~when_n_exceeds_length =
  if n > length t
  then when_n_exceeds_length
  else if n < 0
  then invalid_arg (name ^ " expecting nonnegative argument")
  else
    (* The way arguments to this function are constructed (see usages below), the check
       that [0 <= n <= length t] is sufficient to know that [pos] and [len] are valid.
       Thus [sub] should not raise. *)
    sub t ~pos ~len
;;

let drop_prefix t n =
  wrap_sub_n ~name:"drop_prefix" t n ~pos:n ~len:(length t - n) ~when_n_exceeds_length:""
;;

let drop_suffix t n =
  wrap_sub_n ~name:"drop_suffix" t n ~pos:0 ~len:(length t - n) ~when_n_exceeds_length:""
;;

let prefix t n = wrap_sub_n ~name:"prefix" t n ~pos:0 ~len:n ~when_n_exceeds_length:t

let suffix t n =
  wrap_sub_n ~name:"suffix" t n ~pos:(length t - n) ~len:n ~when_n_exceeds_length:t
;;

let lfindi ?(pos = 0) t ~f =
  let n = length t in
  let rec loop i = if i = n then None else if f i t.[i] then Some i else loop (i + 1) in
  loop pos [@nontail]
;;

let%template[@mode l = (global, local)] find t ~f =
  match[@exclave_if_local l ~reasons:[ Will_return_unboxed ]]
    lfindi t ~f:(fun _ c -> f c)
  with
  | None -> None
  | Some i -> Some t.[i]
;;

let%template find_map =
  let rec loop t n i ~f =
    if [@exclave_if_local lo ~reasons:[ May_return_local ]] i = n
    then None
    else (
      match f t.[i] with
      | None -> loop t n (i + 1) ~f
      | Some _ as res -> res)
  in
  fun t ~f ->
    let n = length t in
    loop t n 0 ~f [@exclave_if_local lo]
[@@mode li = (global, local), lo = (global, local)]
;;

let rfindi ?pos t ~f =
  let rec loop i = if i < 0 then None else if f i t.[i] then Some i else loop (i - 1) in
  let pos =
    match pos with
    | Some pos -> pos
    | None -> length t - 1
  in
  loop pos [@nontail]
;;

let last_non_drop ~drop t = rfindi t ~f:(fun _ c -> not (drop c)) [@nontail]

let rstrip ?(drop = Char.is_whitespace) t =
  match last_non_drop t ~drop with
  | None -> ""
  | Some i -> if i = length t - 1 then t else prefix t (i + 1)
;;

let first_non_drop ~drop t = lfindi t ~f:(fun _ c -> not (drop c)) [@nontail]

let lstrip ?(drop = Char.is_whitespace) t =
  match first_non_drop t ~drop with
  | None -> ""
  | Some 0 -> t
  | Some n -> drop_prefix t n
;;

(* [strip t] could be implemented as [lstrip (rstrip t)]. The implementation below saves
   (at least) a factor of two allocation, by only allocating the final result. This also
   saves some amount of time. *)
let strip ?(drop = Char.is_whitespace) t =
  let length = length t in
  if length = 0 || not (drop t.[0] || drop t.[length - 1])
  then t
  else (
    match first_non_drop t ~drop with
    | None -> ""
    | Some first ->
      (match last_non_drop t ~drop with
       | None -> assert false
       | Some last -> sub t ~pos:first ~len:(last - first + 1)))
;;

let%template mapi t ~f =
  (let l = length t in
   let t' = (Bytes.create [@alloc a]) l in
   for i = 0 to l - 1 do
     Bytes.unsafe_set t' i (f i t.[i])
   done;
   Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t')
  [@exclave_if_stack a]
[@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
;;

(* repeated code to avoid requiring an extra allocation for a closure on each call. *)
let%template map t ~f =
  (let l = length t in
   let t' = (Bytes.create [@alloc a]) l in
   for i = 0 to l - 1 do
     Bytes.unsafe_set t' i (f t.[i])
   done;
   Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t')
  [@exclave_if_stack a]
[@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
;;

let to_array s = Array.init (length s) ~f:(fun i -> s.[i]) [@nontail]

let%template[@mode l = (global, local)] exists =
  let rec loop s i ~len ~f = i < len && (f s.[i] || loop s (i + 1) ~len ~f) in
  fun s ~f -> loop s 0 ~len:(length s) ~f
;;

let%template[@mode l = (global, local)] for_all =
  let rec loop s i ~len ~f = i = len || (f s.[i] && loop s (i + 1) ~len ~f) in
  fun s ~f -> loop s 0 ~len:(length s) ~f
;;

let%template[@mode li = (global, local), lo = (global, local)] fold
  : t @ li -> init:_ @ lo -> f:(_ @ lo -> elt @ li -> _ @ lo) @ local -> _ @ lo
  =
  let rec loop t i ac ~f ~len =
    if [@exclave_if_local lo ~reasons:[ May_return_local ]] i = len
    then ac
    else loop t (i + 1) (f ac t.[i]) ~f ~len
  in
  fun t ~init ~f ->
    loop t 0 init ~f ~len:(length t) [@exclave_if_local lo ~reasons:[ May_return_local ]]
;;

let%template[@mode li = (global, local), lo = (global, local)] foldi =
  let rec loop t i ac ~f ~len : _ @ lo =
    if [@exclave_if_local lo ~reasons:[ May_return_local ]] i = len
    then ac
    else loop t (i + 1) (f i ac t.[i]) ~f ~len
  in
  fun t ~init ~f ->
    loop t 0 init ~f ~len:(length t) [@exclave_if_local lo ~reasons:[ May_return_local ]]
;;

let%template[@mode l = (global, local)] iteri t ~f =
  for i = 0 to length t - 1 do
    f i (unsafe_get t i)
  done
;;

let%template[@mode l = (global, local)] count t ~f =
  (Container.count [@mode l]) ~fold:(fold [@mode l global]) t ~f
;;

let%template[@mode li = (global, local), lo = (global, local)] sum l t ~f =
  (Container.sum [@mode li lo]) ~fold:(fold [@mode li lo]) l t ~f [@exclave_if_local lo]
;;

let%template[@mode l = (global, local)] min_elt t =
  (Container.min_elt [@mode l]) ~fold:(fold [@mode l l]) t [@exclave_if_local l]
;;

let%template[@mode l = (global, local)] max_elt t =
  (Container.max_elt [@mode l]) ~fold:(fold [@mode l l]) t [@exclave_if_local l]
;;

(* Reimplement [fold_until] without [With_return] so that it may return a [local] *)
let%template[@mode li = (global, local), lo = (global, local)] fold_until =
  let rec loop t i ac ~f ~len ~finish =
    if [@exclave_if_local lo ~reasons:[ May_return_local ]] i = len
    then finish ac
    else (
      match (f ac t.[i] : _ Container.Continue_or_stop.t) with
      | Continue ac -> loop t (i + 1) ac ~f ~len ~finish
      | Stop ac -> ac)
  in
  fun t ~init ~f ~finish ->
    let len = length t in
    loop t 0 init ~f ~len ~finish [@exclave_if_local lo]
;;

let%template[@mode li = (global, local), lo = (global, local)] fold_result t ~init ~f =
  (Container.fold_result [@mode li lo])
    ~fold_until:(fold_until [@mode li lo])
    ~init
    ~f
    t [@exclave_if_local lo]
;;

let%template[@mode li = (global, local), lo = (global, local)] foldi_until
  t
  ~init
  ~f
  ~finish
  =
  (Indexed_container.foldi_until [@mode li lo])
    ~fold_until:(fold_until [@mode li lo])
    ~init
    ~f
    t
    ~finish [@exclave_if_local lo]
;;

let%template[@mode li = (global, local), lo = (global, local)] iter_until t ~f ~finish =
  (Container.iter_until [@mode li lo])
    ~fold_until:(fold_until [@mode li lo])
    ~f
    t
    ~finish [@exclave_if_local lo]
;;

let%template[@mode li = (global, local), lo = (global, local)] iteri_until t ~f ~finish =
  (Indexed_container.iteri_until [@mode li lo])
    ~foldi_until:(foldi_until [@mode li lo])
    ~f
    t
    ~finish [@exclave_if_local lo]
;;

let%template[@mode li = (global, local), lo = (global, local)] find_mapi t ~f =
  (Indexed_container.find_mapi [@mode li lo])
    ~iteri_until:(iteri_until [@mode li lo])
    t
    ~f [@exclave_if_local lo]
;;

let%template[@mode l = (global, local)] findi t ~f =
  (Indexed_container.findi [@mode l])
    ~iteri_until:(iteri_until [@mode l l])
    t
    ~f [@exclave_if_local l]
;;

let%template[@mode l = (global, local)] counti t ~f =
  (Indexed_container.counti [@mode l]) ~foldi:(foldi [@mode l global]) t ~f
;;

let%template[@mode l = (global, local)] for_alli t ~f =
  (Indexed_container.for_alli [@mode l]) ~iteri_until:(iteri_until [@mode l global]) t ~f
;;

let%template[@mode l = (global, local)] existsi t ~f =
  (Indexed_container.existsi [@mode l]) ~iteri_until:(iteri_until [@mode l global]) t ~f
;;

let%template[@mode l = (global, local)] mem =
  let rec loop t c ~pos:i ~len =
    i < len && (Char.equal c (unsafe_get t i) || loop t c ~pos:(i + 1) ~len)
  in
  fun t c -> loop t c ~pos:0 ~len:(length t)
;;

let%template[@alloc a @ l = (heap_global, stack_local)] tr ~target ~replacement s =
  if Char.equal target replacement
  then s
  else if (mem [@mode l]) s target
  then
    (map [@mode l] [@alloc a]) s ~f:(fun c ->
      if Char.equal c target then replacement else c)
    [@exclave_if_stack a]
  else s
;;

let tr_multi ~target ~replacement =
  if is_empty target
  then stage Fn.id
  else if is_empty replacement
  then invalid_arg "tr_multi replacement is empty string"
  else (
    match Bytes_tr.tr_create_map ~target ~replacement with
    | None -> stage Fn.id
    | Some tr_map ->
      stage (fun s ->
        if exists s ~f:(fun c -> Char.( <> ) c (unsafe_get tr_map (Char.to_int c)))
        then map s ~f:(fun c -> unsafe_get tr_map (Char.to_int c))
        else s))
;;

external concat_array
  :  local_ string array
  -> sep:local_ string
  -> string
  @@ portable
  = "Base_string_concat_array"

let concat_array ?(local_ sep = "") ar = concat_array ar ~sep

let%template concat_map ?sep s ~f = concat_array ?sep (Array.map (to_array s) ~f)
[@@mode li = (global, local)] [@@alloc a = heap]
;;

let%template concat_mapi ?sep t ~f = concat_array ?sep (Array.mapi (to_array t) ~f)
[@@mode li = (global, local)] [@@alloc a = heap]
;;

let%template concat_mapi ?sep t ~f =
  let rec loop t i total parts ~f = exclave_
    if i < 0
    then total, parts
    else (
      let part = f i t.[i] in
      (loop [@tailcall]) t (i - 1) (total + length part) (part :: parts) ~f)
  in
  let len = length t in
  let sep =
    match sep with
    | None -> ""
    | Some sep -> sep
  in
  let sep_len = length sep in
  exclave_
  let total, parts = loop t (len - 1) 0 [] ~f in
  let dst = (Bytes.create [@alloc stack]) (total + (max (len - 1) 0 * sep_len)) in
  unsafe_blits ~dst ~dst_pos:0 ~sep ~sep_len parts;
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst
[@@mode li = (global, local)] [@@alloc stack]
;;

let%template concat_map ?sep s ~f = exclave_
  (concat_mapi [@mode li] [@alloc stack]) ?sep s ~f:(fun _ c -> exclave_ f c)
[@@mode li = (global, local)] [@@alloc stack]
;;

let concat_lines =
  let rec line_lengths ~lines ~newline_len ~sum =
    match lines with
    | [] -> sum
    | line :: lines ->
      let sum = sum + String.length line + newline_len in
      line_lengths ~lines ~newline_len ~sum
  in
  let rec write_lines ~buf ~lines ~crlf ~pos =
    match lines with
    | [] -> pos
    | line :: lines ->
      Bytes.unsafe_blit_string
        ~src:line
        ~src_pos:0
        ~dst:buf
        ~dst_pos:pos
        ~len:(String.length line);
      let pos = pos + String.length line in
      let pos =
        if crlf
        then (
          Bytes.unsafe_set buf pos '\r';
          pos + 1)
        else pos
      in
      Bytes.unsafe_set buf pos '\n';
      let pos = pos + 1 in
      write_lines ~buf ~lines ~crlf ~pos
  in
  fun ?(crlf = false) lines ->
    let newline_len = if crlf then 2 else 1 in
    let len = line_lengths ~newline_len ~lines ~sum:0 in
    let buf = Bytes.create len in
    let written = write_lines ~buf ~lines ~crlf ~pos:0 in
    assert (written = len);
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
;;

let chop_prefix s ~prefix =
  if is_prefix s ~prefix then Some (drop_prefix s (length prefix)) else None
;;

let chop_prefix_if_exists s ~prefix =
  if is_prefix s ~prefix then drop_prefix s (length prefix) else s
;;

let chop_prefix_exn s ~prefix =
  match chop_prefix s ~prefix with
  | Some str -> str
  | None -> invalid_argf "String.chop_prefix_exn %S %S" s prefix ()
;;

let chop_suffix s ~suffix =
  if is_suffix s ~suffix then Some (drop_suffix s (length suffix)) else None
;;

let chop_suffix_if_exists s ~suffix =
  if is_suffix s ~suffix then drop_suffix s (length suffix) else s
;;

let chop_suffix_exn s ~suffix =
  match chop_suffix s ~suffix with
  | Some str -> str
  | None -> invalid_argf "String.chop_suffix_exn %S %S" s suffix ()
;;

module For_common_prefix_and_suffix = struct
  (* When taking a string prefix or suffix, we extract from the shortest input available
     in case we can just return one of our inputs without allocating a new string. *)

  let shorter a b = if length a <= length b then a else b

  let shortest list =
    match list with
    | [] -> ""
    | first :: rest -> List.fold rest ~init:first ~f:shorter
  ;;

  (* Our generic accessors for common prefix/suffix abstract over [get_pos], which is
     either [pos_from_left] or [pos_from_right]. *)

  let pos_from_left (_ : t) (i : int) = i
  let pos_from_right t i = length t - i - 1

  let rec common_generic2_length_loop a b ~get_pos ~max_len ~len_so_far =
    if len_so_far >= max_len
    then max_len
    else if Char.equal
              (unsafe_get a (get_pos a len_so_far))
              (unsafe_get b (get_pos b len_so_far))
    then common_generic2_length_loop a b ~get_pos ~max_len ~len_so_far:(len_so_far + 1)
    else len_so_far
  ;;

  let common_generic2_length a b ~get_pos =
    let max_len = min (length a) (length b) in
    common_generic2_length_loop a b ~get_pos ~max_len ~len_so_far:0
  ;;

  let rec common_generic_length_loop first list ~get_pos ~max_len =
    match list with
    | [] -> max_len
    | second :: rest ->
      let max_len =
        (* We call [common_generic2_length_loop] rather than [common_generic2_length] so
           that [max_len] limits our traversal of [first] and [second]. *)
        common_generic2_length_loop first second ~get_pos ~max_len ~len_so_far:0
      in
      common_generic_length_loop second rest ~get_pos ~max_len
  ;;

  let common_generic_length list ~get_pos =
    match list with
    | [] -> 0
    | first :: rest ->
      (* Precomputing [max_len] based on [shortest list] saves us work in longer strings,
         at the cost of an extra pass over the spine of [list].

         For example, if you're looking for the longest prefix of the strings:

         {v
            let long_a = List.init 1000 ~f:(Fn.const 'a')
            [ long_a; long_a; 'aa' ]
         v}

         the approach below will just check the first two characters of all the strings.
      *)
      let max_len = length (shortest list) in
      common_generic_length_loop first rest ~get_pos ~max_len
  ;;

  (* Our generic accessors that produce a string abstract over [take], which is either
     [prefix] or [suffix]. *)

  let common_generic2 a b ~get_pos ~take =
    let len = common_generic2_length a b ~get_pos in
    (* Use the shorter of the two strings, so that if the shorter one is the shared
       prefix, [take] won't allocate another string. *)
    take (shorter a b) len
  ;;

  let common_generic list ~get_pos ~take =
    match list with
    | [] -> ""
    | first :: rest ->
      (* As with [common_generic_length], we base [max_len] on [shortest list]. We also
         use this result for [take], below, to potentially avoid allocating a string. *)
      let s = shortest list in
      let max_len = length s in
      if max_len = 0
      then ""
      else (
        let len =
          (* We call directly into [common_generic_length_loop] rather than
             [common_generic_length] to avoid recomputing [shortest list]. *)
          common_generic_length_loop first rest ~get_pos ~max_len
        in
        take s len)
  ;;
end

include struct
  open For_common_prefix_and_suffix

  let common_prefix list = common_generic list ~take:prefix ~get_pos:pos_from_left
  let common_suffix list = common_generic list ~take:suffix ~get_pos:pos_from_right
  let common_prefix2 a b = common_generic2 a b ~take:prefix ~get_pos:pos_from_left
  let common_suffix2 a b = common_generic2 a b ~take:suffix ~get_pos:pos_from_right
  let common_prefix_length list = common_generic_length list ~get_pos:pos_from_left
  let common_suffix_length list = common_generic_length list ~get_pos:pos_from_right
  let common_prefix2_length a b = common_generic2_length a b ~get_pos:pos_from_left
  let common_suffix2_length a b = common_generic2_length a b ~get_pos:pos_from_right
end

(* There used to be a custom implementation that was faster for very short strings
   (peaking at 40% faster for 4-6 char long strings). This new function is around 20%
   faster than the default hash function, but slower than the previous custom
   implementation. However, the new OCaml function is well behaved, and this
   implementation is less likely to diverge from the default OCaml implementation does,
   which is a desirable property. (The only way to avoid the divergence is to expose the
   macro redefined in hash_stubs.c in the hash.h header of the OCaml compiler.) *)
module Hash = struct
  external hash : string -> int @@ portable = "Base_hash_string" [@@noalloc]
end

(* [include Hash] to make the [external] version override the [hash] from
   [Hashable.Make_binable], so that we get a little bit of a speedup by exposing it as
   external in the mli. *)
let _ = hash

include Hash

(* for interactive top-levels -- modules deriving from String should have String's pretty
   printer. *)
let pp ppf string = Stdlib.Format.fprintf ppf "%S" string
let of_char c = make 1 c

let%template[@alloc a @ l = (heap_global, stack_local)] of_char_list l =
  (let t = (Bytes.create [@alloc a]) (List.length l) in
   (List.iteri [@mode l]) l ~f:(fun i c -> Bytes.set t i c);
   Bytes.unsafe_to_string ~no_mutation_while_string_reachable:t)
  [@exclave_if_stack a]
;;

let%template[@alloc a = (heap, stack)] of_list = (of_char_list [@alloc a])

let%template[@alloc a = (heap, stack)] of_array a =
  (init [@alloc a]) (Array.length a) ~f:(Array.get a) [@exclave_if_stack a]
;;

let to_sequence t =
  let len = length t in
  Sequence.unfold_step ~init:0 ~f:(fun pos ->
    if pos >= len then Done else Yield { value = unsafe_get t pos; state = pos + 1 })
;;

let of_sequence s = of_list (Sequence.to_list s)
let append = String0.( ^ )

let pad_right ?(char = ' ') s ~len =
  let src_len = length s in
  if src_len >= len
  then s
  else (
    let res = Bytes.create len in
    Bytes.blit_string ~src:s ~dst:res ~src_pos:0 ~dst_pos:0 ~len:src_len;
    Bytes.fill ~pos:src_len ~len:(len - src_len) res char;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res)
;;

let pad_left ?(char = ' ') s ~len =
  let src_len = length s in
  if src_len >= len
  then s
  else (
    let res = Bytes.create len in
    Bytes.blit_string ~src:s ~dst:res ~src_pos:0 ~dst_pos:(len - src_len) ~len:src_len;
    Bytes.fill ~pos:0 ~len:(len - src_len) res char;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:res)
;;

(* Called upon first difference generated by filtering. Allocates [buffer_len] bytes for
   new result, and copies [prefix_len] unchanged characters from [src]. Always returns a
   local buffer. *)
let local_copy_prefix (local_ src) ~prefix_len ~buffer_len = exclave_
  let dst = Bytes.create_local buffer_len in
  Bytes.unsafe_blit_string ~src ~dst ~src_pos:0 ~dst_pos:0 ~len:prefix_len;
  dst
;;

(* Copies a perhaps-local buffer into a string allocated by [a]. *)
let%template copy_to_string (local_ buf) ~pos @ l =
  (let str = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf in
   (unsafe_sub [@alloc a]) str ~pos:0 ~len:pos [@nontail])
  [@exclave_if_stack a]
[@@alloc a @ l = (heap_global, stack_local)]
;;

include%template struct
  open struct
    (* filter_map helpers *)

    (* Filters from string [src] into an allocated buffer [dst]; copies the allocated
       buffer to a heap-allocated result string.

       Pre-conditions: [src_len = length src] [src != dst] [0 <= src_pos < src_len]
       [0 <= dst_pos < length dst]
    *)
    let filter_mapi_into src (local_ dst) ~f ~src_pos ~dst_pos ~src_len @ lo =
      let local_ dst_pos = ref dst_pos in
      for src_pos = src_pos to src_len - 1 do
        match f src_pos (unsafe_get src src_pos) with
        | None -> ()
        | Some c ->
          Bytes.unsafe_set dst !dst_pos c;
          incr dst_pos
      done;
      let dst_pos = !dst_pos in
      (copy_to_string [@alloc a]) dst ~pos:dst_pos [@exclave_if_stack a]
    [@@alloc a @ lo = (heap_global, stack_local)]
    ;;

    (* Filters [t]. If the result turns out to be identical to the input, returns [t]
       directly without needing to allocate a buffer and traverse the string twice.

       Pre-condition: [len == length t] Pre-condition: [0 <= pos <= len] *)
    let rec filter_mapi_maybe_id t ~f ~pos ~len =
      if [@exclave_if_stack a] pos = len
      then (smart_globalize [@alloc a]) t
      else (
        let c1 = unsafe_get t pos in
        let next = Int.succ pos in
        match f pos c1 with
        | Some c2 when Char.equal c1 c2 ->
          (* if nothing has changed, continue *)
          (filter_mapi_maybe_id [@mode li] [@alloc a]) t ~f ~pos:next ~len
        | option ->
          (* If a character has been changed or dropped, begin an output buffer up to
             [pos], and write the new character into it. *)
          let copy = local_copy_prefix t ~prefix_len:pos ~buffer_len:len in
          let dst_pos =
            match option with
            | None -> pos
            | Some c ->
              Bytes.unsafe_set copy pos c;
              next
          in
          (filter_mapi_into [@alloc a])
            t
            copy
            ~f
            ~src_pos:next
            ~dst_pos
            ~src_len:len [@nontail])
    [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
    ;;
  end

  (* filter_map functions *)

  let filter_mapi t ~f =
    (filter_mapi_maybe_id [@mode li] [@alloc a])
      t
      ~f
      ~pos:0
      ~len:(length t) [@nontail] [@exclave_if_stack a]
  [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
  ;;

  let filter_map t ~f =
    (filter_mapi [@mode li] [@alloc a]) t ~f:(fun _ c -> f c [@exclave_if_stack a])
    [@nontail] [@exclave_if_stack a]
  [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
  ;;
end

include%template struct
  open struct
    (* partition helpers *)

    let partition_mapi_into src ~fsts ~snds ~f ~len ~src_pos ~fst_pos ~snd_pos =
      let local_ fst_pos = ref fst_pos in
      let local_ snd_pos = ref snd_pos in
      for src_pos = src_pos to len - 1 do
        match local_ (f src_pos (unsafe_get src src_pos) : (_, _) Either.t) with
        | First c ->
          Bytes.unsafe_set fsts !fst_pos c;
          incr fst_pos
        | Second c ->
          Bytes.unsafe_set snds !snd_pos c;
          incr snd_pos
      done;
      let fst_pos = !fst_pos
      and snd_pos = !snd_pos in
      ( (copy_to_string [@alloc a]) fsts ~pos:fst_pos
      , (copy_to_string [@alloc a]) snds ~pos:snd_pos )
      [@exclave_if_stack a]
    [@@alloc a @ lo = (heap_global, stack_local)]
    ;;

    let partition_mapi_difference src ~f ~len ~pos:src_pos ~fst_pos ~snd_pos either =
      (let fsts = local_copy_prefix src ~prefix_len:fst_pos ~buffer_len:len in
       let snds = local_copy_prefix src ~prefix_len:snd_pos ~buffer_len:len in
       let fst_pos, snd_pos =
         match (either : (_, _) Either.t) with
         | First c ->
           Bytes.unsafe_set fsts fst_pos c;
           local_ fst_pos + 1, snd_pos
         | Second c ->
           Bytes.unsafe_set snds snd_pos c;
           local_ fst_pos, snd_pos + 1
       in
       (partition_mapi_into [@alloc a])
         src
         ~fsts
         ~snds
         ~f
         ~len
         ~src_pos:(src_pos + 1)
         ~fst_pos
         ~snd_pos [@nontail])
      [@exclave_if_stack a]
    [@@alloc a @ lo = (heap_global, stack_local)]
    ;;

    let rec partition_mapi_first_maybe_id src ~f ~pos ~len =
      if [@exclave_if_stack a] pos = len
      then (smart_globalize [@alloc a]) src, ""
      else (
        let c1 = unsafe_get src pos in
        match local_ (f pos c1 : (_, _) Either.t) with
        | First c2 when Char.equal c1 c2 ->
          (partition_mapi_first_maybe_id [@mode li] [@alloc a]) src ~f ~len ~pos:(pos + 1)
        | either ->
          (partition_mapi_difference [@alloc a])
            src
            ~f
            ~len
            ~pos
            ~fst_pos:pos
            ~snd_pos:0
            either [@nontail])
    [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
    ;;

    let rec partition_mapi_second_maybe_id src ~f ~pos ~len =
      if [@exclave_if_stack a] pos = len
      then "", (smart_globalize [@alloc a]) src
      else (
        let c1 = unsafe_get src pos in
        match local_ (f pos c1 : (_, _) Either.t) with
        | Second c2 when Char.equal c1 c2 ->
          (partition_mapi_second_maybe_id [@mode li] [@alloc a]) src ~f ~len ~pos:(pos + 1)
        | either ->
          (partition_mapi_difference [@alloc a])
            src
            ~f
            ~len
            ~pos
            ~fst_pos:0
            ~snd_pos:pos
            either [@nontail])
    [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
    ;;
  end

  (* partition functions *)

  let partition_mapi src ~f =
    let len = length src in
    if [@exclave_if_stack a] len = 0
    then "", ""
    else (
      let c1 = unsafe_get src 0 in
      match local_ (f 0 c1 : (_, _) Either.t) with
      | First c2 when Char.equal c1 c2 ->
        (partition_mapi_first_maybe_id [@mode li] [@alloc a]) src ~f ~len ~pos:1
      | Second c2 when Char.equal c1 c2 ->
        (partition_mapi_second_maybe_id [@mode li] [@alloc a]) src ~f ~len ~pos:1
      | either ->
        (partition_mapi_difference [@alloc a])
          src
          ~f
          ~len
          ~pos:0
          ~fst_pos:0
          ~snd_pos:0
          either [@nontail])
  [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
  ;;

  let partitioni_tf t ~f =
    (partition_mapi [@mode l] [@alloc a]) t ~f:(fun i c -> exclave_
      if f i c then First c else Second c)
    [@nontail] [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let partition_tf t ~f =
    (partitioni_tf [@alloc a]) t ~f:(fun _ c -> f c) [@nontail] [@exclave_if_stack a]
  [@@alloc a @ l = (heap_global, stack_local)]
  ;;

  let partition_map t ~f =
    (partition_mapi [@mode li] [@alloc a]) t ~f:(fun _ c -> f c [@exclave_if_stack a])
    [@nontail] [@exclave_if_stack a]
  [@@mode li = (global, local)] [@@alloc a @ lo = (heap_global, stack_local)]
  ;;
end

let edit_distance s1 s2 =
  (* We maintain a table of edit distance between all indices of the shorter string, and
     the current and previous indices of the longer string. *)
  let s1, s2 = if String.length s1 <= String.length s2 then s1, s2 else s2, s1 in
  let table = Array.create_local ~len:(2 * (1 + String.length s1)) 0 in
  let at i j = (i * 2) + (j mod 2) in
  for i = 1 to String.length s1 do
    (* Insert [i] characters when [j=0]. *)
    table.(at i 0) <- i
  done;
  for j = 1 to String.length s2 do
    (* Insert [j] characters when [i=0]. *)
    table.(at 0 j) <- j;
    for i = 1 to String.length s1 do
      if Char.equal s1.[i - 1] s2.[j - 1]
      then
        (* Nothing to edit for the current character. *)
        table.(at i j) <- table.(at (i - 1) (j - 1))
      else (
        (* Edit the current character by substitution, addition, or deletion. *)
        let sub = table.(at (i - 1) (j - 1)) in
        let add = table.(at (i - 1) j) in
        let del = table.(at i (j - 1)) in
        table.(at i j) <- 1 + min sub (min add del))
    done
  done;
  (* Return the final result. *)
  table.(at (String.length s1) (String.length s2))
;;

module Escaping = struct
  (* If this is changed, make sure to update [escape], which attempts to ensure all the
     invariants checked here. *)
  let build_and_validate_escapeworthy_map escapeworthy_map escape_char func =
    let escapeworthy_map =
      if List.Assoc.mem escapeworthy_map ~equal:Char.equal escape_char
      then escapeworthy_map
      else (escape_char, escape_char) :: escapeworthy_map
    in
    let arr = Array.create ~len:256 (-1) in
    let vals = Array.create ~len:256 false in
    let rec loop = function
      | [] -> Ok (Iarray0.unsafe_of_array__promise_no_mutation arr)
      | (c_from, c_to) :: l ->
        let k, v =
          match func with
          | `Escape -> Char.to_int c_from, c_to
          | `Unescape -> Char.to_int c_to, c_from
        in
        if arr.(k) <> -1 || vals.(Char.to_int v)
        then
          Or_error.error_s
            (Sexp.message
               "escapeworthy_map not one-to-one"
               [ "c_from", sexp_of_char c_from
               ; "c_to", sexp_of_char c_to
               ; ( "escapeworthy_map"
                 , sexp_of_list (sexp_of_pair sexp_of_char sexp_of_char) escapeworthy_map
                 )
               ])
        else (
          arr.(k) <- Char.to_int v;
          vals.(Char.to_int v) <- true;
          loop l)
    in
    loop escapeworthy_map
  ;;

  let escape_gen ~escapeworthy_map ~escape_char =
    match build_and_validate_escapeworthy_map escapeworthy_map escape_char `Escape with
    | Error _ as x -> x
    | Ok escapeworthy ->
      Ok
        (fun src ->
          (* calculate a list of (index of char to escape * escaped char) first, the order
             is from tail to head *)
          let to_escape_len = ref 0 in
          let to_escape =
            foldi src ~init:[] ~f:(fun i acc c ->
              match escapeworthy.:(Char.to_int c) with
              | -1 -> acc
              | n ->
                (* (index of char to escape * escaped char) *)
                incr to_escape_len;
                (i, Char.unsafe_of_int n) :: acc)
          in
          match to_escape with
          | [] -> src
          | _ ->
            (* [to_escape] divide [src] to [List.length to_escape + 1] pieces separated by
               the chars to escape.

               Lets take
               {[
                 escape_gen_exn ~escapeworthy_map:[ 'a', 'A'; 'b', 'B'; 'c', 'C' ] ~escape_char:'_'
               ]}
               for example, and assume the string to escape is

               "000a111b222c333"

               then [to_escape] is [(11, 'C'); (7, 'B'); (3, 'A')].

               Then we create a [dst] of length [length src + 3] to store the result, copy
               piece "333" to [dst] directly, then copy '_' and 'C' to [dst]; then move on
               to next; after 3 iterations, copy piece "000" and we are done.

               Finally the result will be

               "000_A111_B222_C333" *)
            let src_len = length src in
            let dst_len = src_len + !to_escape_len in
            let dst = Bytes.create dst_len in
            let rec loop last_idx last_dst_pos = function
              | [] ->
                (* copy "000" at last *)
                Bytes.blit_string ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:last_idx
              | (idx, escaped_char) :: to_escape ->
                (*[idx] = the char to escape
                 * take first iteration for example
                 * calculate length of "333", minus 1 because we don't copy 'c' *)
                let len = last_idx - idx - 1 in
                (* set the dst_pos to copy to *)
                let dst_pos = last_dst_pos - len in
                (* copy "333", set [src_pos] to [idx + 1] to skip 'c' *)
                Bytes.blit_string ~src ~src_pos:(idx + 1) ~dst ~dst_pos ~len;
                (* backoff [dst_pos] by 2 to copy '_' and 'C' *)
                let dst_pos = dst_pos - 2 in
                Bytes.set dst dst_pos escape_char;
                Bytes.set dst (dst_pos + 1) escaped_char;
                loop idx dst_pos to_escape
            in
            (* set [last_dst_pos] and [last_idx] to length of [dst] and [src] first *)
            loop src_len dst_len to_escape;
            Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst)
  ;;

  let%template escape_gen_exn ~escapeworthy_map ~escape_char =
    escape_gen ~escapeworthy_map ~escape_char
    |> Modes.Portable.wrap_ok
    |> Or_error.ok_exn
    |> Modes.Portable.unwrap
    |> (stage [@mode portable])
  ;;

  let escape ~escapeworthy ~escape_char =
    (* For [escape_gen_exn], we don't know how to fix invalid escapeworthy_map so we have
       to raise exception; but in this case, we know how to fix duplicated elements in
       escapeworthy list, so we just fix it instead of raising exception to make this
       function easier to use. *)
    let escapeworthy_map =
      escapeworthy
      |> List.dedup_and_sort ~compare:Char.compare
      |> List.map ~f:(fun c -> c, c)
    in
    escape_gen_exn ~escapeworthy_map ~escape_char
  ;;

  (*=In an escaped string, any char is either `Escaping, `Escaped or `Literal. For
     example, the escape statuses of chars in string "a_a__" with escape_char = '_' are

     a : `Literal
     _ : `Escaping
     a : `Escaped
     _ : `Escaping
     _ : `Escaped

     [update_escape_status str ~escape_char i previous_status] gets escape status of
     str.[i] basing on escape status of str.[i - 1] *)
  let update_escape_status str ~escape_char i = function
    | `Escaping -> `Escaped
    | `Literal | `Escaped ->
      if Char.equal str.[i] escape_char then `Escaping else `Literal
  ;;

  let%template unescape_gen ~escapeworthy_map ~escape_char =
    match build_and_validate_escapeworthy_map escapeworthy_map escape_char `Unescape with
    | Error _ as x -> x
    | Ok escapeworthy ->
      Ok
        (fun src ->
          (* Continue the example in [escape_gen_exn], now we unescape

             "000_A111_B222_C333"

             back to

             "000a111b222c333"

             Then [to_unescape] is [14; 9; 4], which is indexes of '_'s.

             Then we create a string [dst] to store the result, copy "333" to it, then
             copy 'c', then move on to next iteration. After 3 iterations copy "000" and
             we are done. *)
          (* indexes of escape chars *)
          let to_unescape =
            let rec loop i status acc =
              if i >= length src
              then acc
              else (
                let status = update_escape_status src ~escape_char i status in
                loop
                  (i + 1)
                  status
                  (match status with
                   | `Escaping -> i :: acc
                   | `Escaped | `Literal -> acc))
            in
            loop 0 `Literal []
          in
          match to_unescape with
          | [] -> src
          | idx :: to_unescape' ->
            let dst = Bytes.create (length src - List.length to_unescape) in
            let rec loop last_idx last_dst_pos = function
              | [] ->
                (* copy "000" at last *)
                Bytes.blit_string ~src ~src_pos:0 ~dst ~dst_pos:0 ~len:last_idx
              | idx :: to_unescape ->
                (* [idx] = index of escaping char *)
                (* take 1st iteration as example, calculate the length of "333", minus 2
                   to skip '_C' *)
                let len = last_idx - idx - 2 in
                (* point [dst_pos] to the position to copy "333" to *)
                let dst_pos = last_dst_pos - len in
                (* copy "333" *)
                Bytes.blit_string ~src ~src_pos:(idx + 2) ~dst ~dst_pos ~len;
                (* backoff [dst_pos] by 1 to copy 'c' *)
                let dst_pos = dst_pos - 1 in
                Bytes.set
                  dst
                  dst_pos
                  (match escapeworthy.:(Char.to_int src.[idx + 1]) with
                   | -1 -> src.[idx + 1]
                   | n -> Char.unsafe_of_int n);
                (* update [last_dst_pos] and [last_idx] *)
                loop idx dst_pos to_unescape
            in
            if idx < length src - 1
            then
              (* set [last_dst_pos] and [last_idx] to length of [dst] and [src] *)
              loop (length src) (Bytes.length dst) to_unescape
            else
              (* for escaped string ending with an escaping char like "000_", just ignore
                 the last escaping char *)
              loop (length src - 1) (Bytes.length dst) to_unescape';
            Bytes.unsafe_to_string ~no_mutation_while_string_reachable:dst)
  ;;

  let%template unescape_gen_exn ~escapeworthy_map ~escape_char =
    unescape_gen ~escapeworthy_map ~escape_char
    |> Modes.Portable.wrap_ok
    |> Or_error.ok_exn
    |> Modes.Portable.unwrap
    |> (stage [@mode portable])
  ;;

  let unescape ~escape_char = unescape_gen_exn ~escapeworthy_map:[] ~escape_char

  let preceding_escape_chars str ~escape_char pos =
    let rec loop p cnt =
      if p < 0 || Char.( <> ) str.[p] escape_char then cnt else loop (p - 1) (cnt + 1)
    in
    loop (pos - 1) 0
  ;;

  (*=In an escaped string, any char is either `Escaping, `Escaped or `Literal. For
     example, the escape statuses of chars in string "a_a__" with escape_char = '_' are

     a : `Literal
     _ : `Escaping
     a : `Escaped
     _ : `Escaping
     _ : `Escaped

     [update_escape_status str ~escape_char i previous_status] gets escape status of
     str.[i] basing on escape status of str.[i - 1] *)
  let update_escape_status str ~escape_char i = function
    | `Escaping -> `Escaped
    | `Literal | `Escaped ->
      if Char.equal str.[i] escape_char then `Escaping else `Literal
  ;;

  let escape_status str ~escape_char pos =
    let odd = preceding_escape_chars str ~escape_char pos mod 2 = 1 in
    match odd, Char.equal str.[pos] escape_char with
    | true, (true | false) -> `Escaped
    | false, true -> `Escaping
    | false, false -> `Literal
  ;;

  let check_bound str pos function_name =
    if pos >= length str || pos < 0 then invalid_argf "%s: out of bounds" function_name ()
  ;;

  let is_char_escaping str ~escape_char pos =
    check_bound str pos "is_char_escaping";
    match escape_status str ~escape_char pos with
    | `Escaping -> true
    | `Escaped | `Literal -> false
  ;;

  let is_char_escaped str ~escape_char pos =
    check_bound str pos "is_char_escaped";
    match escape_status str ~escape_char pos with
    | `Escaped -> true
    | `Escaping | `Literal -> false
  ;;

  let is_char_literal str ~escape_char pos =
    check_bound str pos "is_char_literal";
    match escape_status str ~escape_char pos with
    | `Literal -> true
    | `Escaped | `Escaping -> false
  ;;

  let index_from str ~escape_char pos char =
    check_bound str pos "index_from";
    let rec loop i status =
      if i >= pos
         && (match status with
             | `Literal -> true
             | `Escaped | `Escaping -> false)
         && Char.equal str.[i] char
      then Some i
      else (
        let i = i + 1 in
        if i >= length str
        then None
        else loop i (update_escape_status str ~escape_char i status))
    in
    loop pos (escape_status str ~escape_char pos)
  ;;

  let index_from_exn str ~escape_char pos char =
    match index_from str ~escape_char pos char with
    | None ->
      raise_s
        (Sexp.message
           "index_from_exn: not found"
           [ "str", sexp_of_t str
           ; "escape_char", sexp_of_char escape_char
           ; "pos", sexp_of_int pos
           ; "char", sexp_of_char char
           ])
    | Some pos -> pos
  ;;

  let index str ~escape_char char = index_from str ~escape_char 0 char
  let index_exn str ~escape_char char = index_from_exn str ~escape_char 0 char

  let rindex_from str ~escape_char pos char =
    check_bound str pos "rindex_from";
    (* if the target char is the same as [escape_char], we have no way to determine which
       escape_char is literal, so just return None *)
    if Char.equal char escape_char
    then None
    else (
      let rec loop pos =
        if pos < 0
        then None
        else (
          let escape_chars = preceding_escape_chars str ~escape_char pos in
          if escape_chars mod 2 = 0 && Char.equal str.[pos] char
          then Some pos
          else loop (pos - escape_chars - 1))
      in
      loop pos)
  ;;

  let rindex_from_exn str ~escape_char pos char =
    match rindex_from str ~escape_char pos char with
    | None ->
      raise_s
        (Sexp.message
           "rindex_from_exn: not found"
           [ "str", sexp_of_t str
           ; "escape_char", sexp_of_char escape_char
           ; "pos", sexp_of_int pos
           ; "char", sexp_of_char char
           ])
    | Some pos -> pos
  ;;

  let rindex str ~escape_char char =
    if is_empty str then None else rindex_from str ~escape_char (length str - 1) char
  ;;

  let rindex_exn str ~escape_char char =
    rindex_from_exn str ~escape_char (length str - 1) char
  ;;

  (* [split_gen str ~escape_char ~on] works similarly to [String.split_gen], with an
     additional requirement: only split on literal chars, not escaping or escaped *)
  let split_gen str ~escape_char ~on =
    let is_delim =
      match on with
      | `char c' -> fun c -> Char.equal c c'
      | `char_list l -> fun c -> char_list_mem l c
    in
    let len = length str in
    let rec loop acc status last_pos pos =
      if pos = len
      then List.rev (sub str ~pos:last_pos ~len:(len - last_pos) :: acc)
      else (
        let status = update_escape_status str ~escape_char pos status in
        if (match status with
            | `Literal -> true
            | `Escaped | `Escaping -> false)
           && is_delim str.[pos]
        then (
          let sub_str = sub str ~pos:last_pos ~len:(pos - last_pos) in
          loop (sub_str :: acc) status (pos + 1) (pos + 1))
        else loop acc status last_pos (pos + 1))
    in
    loop [] `Literal 0 0
  ;;

  let split str ~on = split_gen str ~on:(`char on)
  let split_on_chars str ~on:chars = split_gen str ~on:(`char_list chars)

  let split_at str pos =
    sub str ~pos:0 ~len:pos, sub str ~pos:(pos + 1) ~len:(length str - pos - 1)
  ;;

  let lsplit2 str ~on ~escape_char =
    Option.map (index str ~escape_char on) ~f:(fun x -> split_at str x)
  ;;

  let rsplit2 str ~on ~escape_char =
    Option.map (rindex str ~escape_char on) ~f:(fun x -> split_at str x)
  ;;

  let lsplit2_exn str ~on ~escape_char = split_at str (index_exn str ~escape_char on)
  let rsplit2_exn str ~on ~escape_char = split_at str (rindex_exn str ~escape_char on)

  (* [last_non_drop_literal] and [first_non_drop_literal] are either both [None] or both
     [Some]. If [Some], then the former is >= the latter. *)
  let last_non_drop_literal ~drop ~escape_char t =
    rfindi t ~f:(fun i c ->
      (not (drop c))
      || is_char_escaping t ~escape_char i
      || is_char_escaped t ~escape_char i)
    [@nontail]
  ;;

  let first_non_drop_literal ~drop ~escape_char t =
    lfindi t ~f:(fun i c ->
      (not (drop c))
      || is_char_escaping t ~escape_char i
      || is_char_escaped t ~escape_char i)
    [@nontail]
  ;;

  let rstrip_literal ?(drop = Char.is_whitespace) t ~escape_char =
    match last_non_drop_literal t ~drop ~escape_char with
    | None -> ""
    | Some i -> if i = length t - 1 then t else prefix t (i + 1)
  ;;

  let lstrip_literal ?(drop = Char.is_whitespace) t ~escape_char =
    match first_non_drop_literal t ~drop ~escape_char with
    | None -> ""
    | Some 0 -> t
    | Some n -> drop_prefix t n
  ;;

  (* [strip t] could be implemented as [lstrip (rstrip t)]. The implementation below saves
     (at least) a factor of two allocation, by only allocating the final result. This also
     saves some amount of time. *)
  let strip_literal ?(drop = Char.is_whitespace) t ~escape_char =
    let length = length t in
    (* performance hack: avoid copying [t] in common cases *)
    if length = 0 || not (drop t.[0] || drop t.[length - 1])
    then t
    else (
      match first_non_drop_literal t ~drop ~escape_char with
      | None -> ""
      | Some first ->
        (match last_non_drop_literal t ~drop ~escape_char with
         | None -> assert false
         | Some last -> sub t ~pos:first ~len:(last - first + 1)))
  ;;
end

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! String_replace_polymorphic_compare

let between t ~low ~high = low <= t && t <= high
let clamp_unchecked t ~min ~max = if t < min then min else if t <= max then t else max

let clamp_exn t ~min ~max =
  assert (min <= max);
  clamp_unchecked t ~min ~max
;;

let clamp t ~min ~max =
  if min > max
  then
    Or_error.error_s
      (Sexp.message
         "clamp requires [min <= max]"
         [ "min", T.sexp_of_t min; "max", T.sexp_of_t max ])
  else Ok (clamp_unchecked t ~min ~max)
;;

(* Override [Search_pattern] with default case-sensitivity argument at the end of the
   file, so that call sites above are forced to supply case-sensitivity explicitly. *)
module Search_pattern = struct
  include Search_pattern0

  let%template[@alloc a = (heap, stack)] create ?(case_sensitive = true) pattern =
    (create [@alloc a]) pattern ~case_sensitive [@exclave_if_stack a]
  ;;
end

module Make_utf (Format : sig
  @@ portable
    val codec_name : string
    val module_name : string
    val is_valid : local_ t -> bool
    val byte_length : Uchar.t -> int
    val get_decode_result : local_ t -> byte_pos:int -> Uchar.utf_decode
    val set : local_ bytes -> int -> Uchar.t -> int
  end) =
struct
  type elt = Uchar.t
  type t = string

  let codec_name = Format.codec_name
  let is_valid = Format.is_valid

  let raise_get_message =
    Portable_lazy.from_fun (fun () ->
      Printf.sprintf
        "%s.get: invalid %s encoding at given position"
        Format.module_name
        Format.codec_name)
  ;;

  let[@cold] raise_get t pos =
    raise_s
      (Sexp.message
         (Portable_lazy.force raise_get_message)
         [ "", Atom t; "pos", sexp_of_int pos ])
  ;;

  let get_unchecked = Format.get_decode_result

  let get t ~byte_pos =
    (* Even if [t] is validated, we need to validate [pos], so we check the decoding *)
    let decode = get_unchecked t ~byte_pos in
    if Uchar.utf_decode_is_valid decode
    then Uchar.utf_decode_uchar decode
    else raise_get t byte_pos
  ;;

  let to_string = Fn.id
  let of_string_unchecked = Fn.id

  let raise_of_string_message =
    concat [ Format.module_name; ".of_string: invalid "; codec_name ]
  ;;

  let[@cold] raise_of_string string =
    raise_s (Sexp.message raise_of_string_message [ "", Atom string ])
  ;;

  let of_string string =
    match is_valid string with
    | true -> string
    | false -> raise_of_string string
  ;;

  include%template Sexpable.Of_stringable [@modality portable] (struct
      type nonrec t = t

      let of_string = of_string
      let to_string = to_string
    end)

  include%template Identifiable.Make [@mode local] [@modality portable] (struct
      type nonrec t = t

      let[@mode l = (local, global)] compare = (compare [@mode l])
      let hash = hash
      let hash_fold_t = hash_fold_t
      let of_string = of_string
      let to_string = to_string
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
      let module_name = Format.module_name
    end)

  let to_sequence t =
    let open Int_replace_polymorphic_compare in
    let len = length t in
    Sequence.unfold ~init:0 ~f:(fun byte_pos ->
      if byte_pos >= len
      then None
      else (
        let decode = Format.get_decode_result t ~byte_pos in
        Some (Uchar.utf_decode_uchar decode, byte_pos + Uchar.utf_decode_length decode)))
  ;;

  let fold t ~init:acc ~f =
    let len = length t in
    let rec local_ loop byte_pos acc =
      if Int_replace_polymorphic_compare.equal byte_pos len
      then acc
      else (
        let decode = Format.get_decode_result t ~byte_pos in
        loop
          (byte_pos + Uchar.utf_decode_length decode)
          (f acc (Uchar.utf_decode_uchar decode)))
    in
    loop 0 acc [@nontail]
  ;;

  let sanitize t =
    let len = fold t ~init:0 ~f:(fun pos uchar -> pos + Format.byte_length uchar) in
    let bytes = Bytes.create len in
    let pos = fold t ~init:0 ~f:(fun pos uchar -> pos + Format.set bytes pos uchar) in
    assert (Int_replace_polymorphic_compare.equal pos len);
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let of_list uchars =
    let len = List.fold uchars ~init:0 ~f:(fun n u -> n + Format.byte_length u) in
    let bytes = Bytes.create len in
    let pos =
      List.fold uchars ~init:0 ~f:(fun pos uchar -> pos + Format.set bytes pos uchar)
    in
    assert (Int_replace_polymorphic_compare.equal pos len);
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let of_array uchars =
    let local_ len = ref 0 in
    for i = 0 to Array.length uchars - 1 do
      len := !len + Format.byte_length uchars.(i)
    done;
    let bytes = Bytes.create !len in
    let local_ pos = ref 0 in
    for i = 0 to Array.length uchars - 1 do
      pos := !pos + Format.set bytes !pos uchars.(i)
    done;
    assert (Int_replace_polymorphic_compare.equal !pos !len);
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes
  ;;

  let concat list = concat ~sep:"" list

  let split t ~on =
    let len = length t in
    let[@tail_mod_cons] rec local_ loop ~start ~until =
      if Int_replace_polymorphic_compare.equal until len
      then [ sub t ~pos:start ~len:(until - start) ]
      else (
        let uchar = get t ~byte_pos:until in
        let next = until + Format.byte_length uchar in
        if Uchar.equal uchar on
        then sub t ~pos:start ~len:(until - start) :: loop ~start:next ~until:next
        else loop ~start ~until:next)
    in
    loop ~start:0 ~until:0 [@nontail]
  ;;

  module%template C = Indexed_container.Make0_with_creators [@modality portable] (struct
      module Elt = Uchar

      type nonrec t = t

      let fold_until t ~init ~f ~finish = Container.fold_until ~fold t ~init ~f ~finish
      let fold = `Custom fold
      let concat = concat
      let of_list = of_list
      let of_array = of_array
      let init = `Define_using_of_array
      let length = `Define_using_fold
      let foldi = `Define_using_fold
      let foldi_until = `Define_using_fold_until
      let iter = `Define_using_fold
      let iter_until = `Define_using_fold_until
      let iteri = `Define_using_fold
      let concat_mapi = `Define_using_concat
    end)

  let append = C.append
  let concat_map = C.concat_map
  let concat_mapi = C.concat_mapi
  let count = C.count
  let counti = C.counti
  let exists = C.exists
  let existsi = C.existsi
  let filter = C.filter
  let filter_map = C.filter_map
  let filter_mapi = C.filter_mapi
  let filteri = C.filteri
  let find = C.find
  let find_map = C.find_map
  let find_mapi = C.find_mapi
  let findi = C.findi
  let fold_result = C.fold_result
  let fold_until = C.fold_until
  let foldi = C.foldi
  let foldi_until = C.foldi_until
  let for_all = C.for_all
  let for_alli = C.for_alli
  let init = C.init
  let is_empty = C.is_empty
  let iter = C.iter
  let iteri = C.iteri
  let iter_until = C.iter_until
  let iteri_until = C.iteri_until
  let length = C.length
  let map = C.map
  let mapi = C.mapi
  let max_elt = C.max_elt
  let mem = C.mem
  let min_elt = C.min_elt
  let partition_map = C.partition_map
  let partition_mapi = C.partition_mapi
  let partition_tf = C.partition_tf
  let partitioni_tf = C.partitioni_tf
  let sum = C.sum
  let to_array = C.to_array
  let to_list = C.to_list
  let length_in_uchars = length
end

module Utf8 = Make_utf (struct
    let codec_name = "UTF-8"
    let module_name = "Base.String.Utf8"
    let is_valid = is_valid_utf_8
    let byte_length = Uchar.utf_8_byte_length
    let get_decode_result = get_utf_8_uchar
    let set = Bytes.set_uchar_utf_8
  end)

module Utf16le = Make_utf (struct
    let codec_name = "UTF-16LE"
    let module_name = "Base.String.Utf16le"
    let is_valid = is_valid_utf_16le
    let byte_length = Uchar.utf_16_byte_length
    let get_decode_result = get_utf_16le_uchar
    let set = Bytes.set_uchar_utf_16le
  end)

module Utf16be = Make_utf (struct
    let codec_name = "UTF-16BE"
    let module_name = "Base.String.Utf16be"
    let is_valid = is_valid_utf_16be
    let byte_length = Uchar.utf_16_byte_length
    let get_decode_result = get_utf_16be_uchar
    let set = Bytes.set_uchar_utf_16be
  end)

module Make_utf32 (Format : sig
  @@ portable
    val codec_name : string
    val module_name : string
    val get_decode_result : local_ t -> byte_pos:int -> Uchar.utf_decode
    val set : local_ bytes -> int -> Uchar.t -> int
  end) =
Make_utf (struct
    open Int_replace_polymorphic_compare

    let byte_length _ = 4
    let codec_name = Format.codec_name
    let module_name = Format.module_name
    let set = Format.set
    let get_decode_result = Format.get_decode_result

    let is_valid t =
      let len = String.length t in
      match len mod 4 with
      | 0 ->
        let rec local_ loop byte_pos =
          match byte_pos < len with
          | false -> true
          | true ->
            let result = Format.get_decode_result t ~byte_pos in
            Uchar.utf_decode_is_valid result && loop (byte_pos + 4)
        in
        loop 0 [@nontail]
      | _ -> false
    ;;
  end)

module Utf32le = Make_utf32 (struct
    let codec_name = "UTF-32LE"
    let module_name = "Base.String.Utf32le"
    let get_decode_result = get_utf_32le_uchar
    let set = Bytes.set_uchar_utf_32le
  end)

module Utf32be = Make_utf32 (struct
    let codec_name = "UTF-32BE"
    let module_name = "Base.String.Utf32be"
    let get_decode_result = get_utf_32be_uchar
    let set = Bytes.set_uchar_utf_32be
  end)

(* Include type-specific [Replace_polymorphic_compare] at the end, after including functor
   application that could shadow its definitions. This is here so that efficient versions
   of the comparison functions are exported by this module. *)
include String_replace_polymorphic_compare
