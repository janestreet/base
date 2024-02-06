open! Import

type ('f, 's) t =
  | First of 'f
  | Second of 's
[@@deriving_inline compare ~localize, hash, sexp, sexp_grammar]

let compare__local :
      'f 's. ('f -> 'f -> int) -> ('s -> 's -> int) -> ('f, 's) t -> ('f, 's) t -> int
  =
  fun _cmp__f _cmp__s a__007_ b__008_ ->
  if Stdlib.( == ) a__007_ b__008_
  then 0
  else (
    match a__007_, b__008_ with
    | First _a__009_, First _b__010_ -> _cmp__f _a__009_ _b__010_
    | First _, _ -> -1
    | _, First _ -> 1
    | Second _a__011_, Second _b__012_ -> _cmp__s _a__011_ _b__012_)
;;

let compare :
      'f 's. ('f -> 'f -> int) -> ('s -> 's -> int) -> ('f, 's) t -> ('f, 's) t -> int
  =
  fun _cmp__f _cmp__s a__001_ b__002_ ->
  if Stdlib.( == ) a__001_ b__002_
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

let t_of_sexp :
      'f 's.
      (Sexplib0.Sexp.t -> 'f) -> (Sexplib0.Sexp.t -> 's) -> Sexplib0.Sexp.t -> ('f, 's) t
  =
  fun (type f__029_ s__030_)
    :  ((Sexplib0.Sexp.t -> f__029_) -> (Sexplib0.Sexp.t -> s__030_) -> Sexplib0.Sexp.t
    -> (f__029_, s__030_) t) ->
  let error_source__017_ = "either0.ml.t" in
  fun _of_f__013_ _of_s__014_ -> function
    | Sexplib0.Sexp.List
        (Sexplib0.Sexp.Atom (("first" | "First") as _tag__020_) :: sexp_args__021_) as
      _sexp__019_ ->
      (match sexp_args__021_ with
       | arg0__022_ :: [] ->
         let res0__023_ = _of_f__013_ arg0__022_ in
         First res0__023_
       | _ ->
         Sexplib0.Sexp_conv_error.stag_incorrect_n_args
           error_source__017_
           _tag__020_
           _sexp__019_)
    | Sexplib0.Sexp.List
        (Sexplib0.Sexp.Atom (("second" | "Second") as _tag__025_) :: sexp_args__026_) as
      _sexp__024_ ->
      (match sexp_args__026_ with
       | arg0__027_ :: [] ->
         let res0__028_ = _of_s__014_ arg0__027_ in
         Second res0__028_
       | _ ->
         Sexplib0.Sexp_conv_error.stag_incorrect_n_args
           error_source__017_
           _tag__025_
           _sexp__024_)
    | Sexplib0.Sexp.Atom ("first" | "First") as sexp__018_ ->
      Sexplib0.Sexp_conv_error.stag_takes_args error_source__017_ sexp__018_
    | Sexplib0.Sexp.Atom ("second" | "Second") as sexp__018_ ->
      Sexplib0.Sexp_conv_error.stag_takes_args error_source__017_ sexp__018_
    | Sexplib0.Sexp.List (Sexplib0.Sexp.List _ :: _) as sexp__016_ ->
      Sexplib0.Sexp_conv_error.nested_list_invalid_sum error_source__017_ sexp__016_
    | Sexplib0.Sexp.List [] as sexp__016_ ->
      Sexplib0.Sexp_conv_error.empty_list_invalid_sum error_source__017_ sexp__016_
    | sexp__016_ -> Sexplib0.Sexp_conv_error.unexpected_stag error_source__017_ sexp__016_
;;

let sexp_of_t :
      'f 's.
      ('f -> Sexplib0.Sexp.t) -> ('s -> Sexplib0.Sexp.t) -> ('f, 's) t -> Sexplib0.Sexp.t
  =
  fun (type f__037_ s__038_)
    :  ((f__037_ -> Sexplib0.Sexp.t) -> (s__038_ -> Sexplib0.Sexp.t)
    -> (f__037_, s__038_) t -> Sexplib0.Sexp.t) ->
  fun _of_f__031_ _of_s__032_ -> function
  | First arg0__033_ ->
    let res0__034_ = _of_f__031_ arg0__033_ in
    Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "First"; res0__034_ ]
  | Second arg0__035_ ->
    let res0__036_ = _of_s__032_ arg0__035_ in
    Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Second"; res0__036_ ]
;;

let t_sexp_grammar :
      'f 's.
      'f Sexplib0.Sexp_grammar.t
      -> 's Sexplib0.Sexp_grammar.t
      -> ('f, 's) t Sexplib0.Sexp_grammar.t
  =
  fun _'f_sexp_grammar _'s_sexp_grammar ->
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag
                { name = "First"
                ; clause_kind =
                    List_clause { args = Cons (_'f_sexp_grammar.untyped, Empty) }
                }
            ; No_tag
                { name = "Second"
                ; clause_kind =
                    List_clause { args = Cons (_'s_sexp_grammar.untyped, Empty) }
                }
            ]
        }
  }
;;

[@@@end]
