[@@@warning "-incompatible-with-upstream"]

[%%template:
[@@@mode.default m = (global, local)]

external select
  : ('a : k).
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  @@ portable
  = "caml_csel_value"
[@@kind k = value_or_null_with_imm]
[@@noalloc]
[@@no_effects]
[@@no_coeffects]
[@@builtin]

val select : ('a : k). bool -> 'a @ m -> 'a @ m -> 'a @ m @@ portable
[@@kind
  k = (base_non_value, value & value, value & value & value, value & value & value & value)]
[@@zero_alloc]]
