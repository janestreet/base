[%%template:
[@@@mode.default m = (global, local)]

external select
  : 'a.
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  = "caml_csel_value"
[@@kind k = (value_or_null, immediate, immediate64)]
[@@noalloc]
[@@no_effects]
[@@no_coeffects]
[@@builtin]

val select : 'a. bool -> 'a -> 'a -> 'a
[@@kind k = (float64, bits32, bits64, word)] [@@zero_alloc]]
