open! Import

type ('f, 's) t =
  | First of 'f
  | Second of 's
[@@deriving_inline compare, hash, sexp, sexp_grammar]

let compare :
  'f 's. ('f -> 'f -> int) -> ('s -> 's -> int) -> ('f, 's) t -> ('f, 's) t -> int
  =
  fun _cmp__f _cmp__s a__001_ b__002_ ->
  if Ppx_compare_lib.phys_equal a__001_ b__002_
  then 0
  else (
    match a__001_, b__002_ with
    | First _a__003_, First _b__004_ -> _cmp__f _a__003_ _b__004_
    | First _, _ -> -1
    | _, First _ -> 1
    | Second _a__005_, Second _b__006_ -> _cmp__s _a__005_ _b__006_)
;;

let hash_fold_t
  : type f s.
    (Ppx_hash_lib.Std.Hash.state -> f -> Ppx_hash_lib.Std.Hash.state)
    -> (Ppx_hash_lib.Std.Hash.state -> s -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> (f, s) t
    -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_f _hash_fold_s hsv arg ->
  match arg with
  | First _a0 ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
    let hsv = hsv in
    _hash_fold_f hsv _a0
  | Second _a0 ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
    let hsv = hsv in
    _hash_fold_s hsv _a0
;;

let t_of_sexp
  : type f s.
    (Sexplib0.Sexp.t -> f) -> (Sexplib0.Sexp.t -> s) -> Sexplib0.Sexp.t -> (f, s) t
  =
  let error_source__007_ = "either0.ml.t" in
  fun _of_f _of_s -> function
    | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom (("first" | "First") as _tag) :: sexp_args)
      as _sexp ->
      (match sexp_args with
       | [ arg0__008_ ] ->
         let res0__009_ = _of_f arg0__008_ in
         First res0__009_
       | _ -> Sexplib0.Sexp_conv_error.stag_incorrect_n_args error_source__007_ _tag _sexp)
    | Sexplib0.Sexp.List (Sexplib0.Sexp.Atom (("second" | "Second") as _tag) :: sexp_args)
      as _sexp ->
      (match sexp_args with
       | [ arg0__010_ ] ->
         let res0__011_ = _of_s arg0__010_ in
         Second res0__011_
       | _ -> Sexplib0.Sexp_conv_error.stag_incorrect_n_args error_source__007_ _tag _sexp)
    | Sexplib0.Sexp.Atom ("first" | "First") as sexp ->
      Sexplib0.Sexp_conv_error.stag_takes_args error_source__007_ sexp
    | Sexplib0.Sexp.Atom ("second" | "Second") as sexp ->
      Sexplib0.Sexp_conv_error.stag_takes_args error_source__007_ sexp
    | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp ->
      Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__007_ sexp
    | Sexplib0.Sexp.List [] as sexp ->
      Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__007_ sexp
    | sexp -> Sexplib0.Sexp_conv_error.unexpected_stag error_source__007_ sexp
;;

let sexp_of_t
  : type f s.
    (f -> Sexplib0.Sexp.t) -> (s -> Sexplib0.Sexp.t) -> (f, s) t -> Sexplib0.Sexp.t
  =
  fun _of_f _of_s -> function
    | First arg0__012_ ->
      let res0__013_ = _of_f arg0__012_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "First"; res0__013_ ]
    | Second arg0__014_ ->
      let res0__015_ = _of_s arg0__014_ in
      Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Second"; res0__015_ ]
;;

let (t_sexp_grammar :
       'f Sexplib0.Sexp_grammar.t
     -> 's Sexplib0.Sexp_grammar.t
     -> ('f, 's) t Sexplib0.Sexp_grammar.t)
  =
  fun _'f_sexp_grammar _'s_sexp_grammar ->
  { untyped =
      Variant
        { name_kind = Capitalized
        ; clauses =
            [ { name = "First"
              ; clause_kind =
                  List_clause { args = Cons (_'f_sexp_grammar.untyped, Empty) }
              }
            ; { name = "Second"
              ; clause_kind =
                  List_clause { args = Cons (_'s_sexp_grammar.untyped, Empty) }
              }
            ]
        }
  }
;;

[@@@end]
