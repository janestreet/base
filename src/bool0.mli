[%%template:
[@@@mode.default m = (global, local)]
[@@@mode.default c = (uncontended, shared, contended)]

external select
  : 'a.
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  = "caml_csel_value"
[@@kind k = value_or_null_with_imm]
[@@noalloc]
[@@no_effects]
[@@no_coeffects]
[@@builtin]

val select : 'a. bool -> 'a -> 'a -> 'a
[@@kind
  k = (base_non_value, value & value, value & value & value, value & value & value & value)]
[@@zero_alloc]]
