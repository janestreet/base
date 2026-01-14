[@@@warning "-incompatible-with-upstream"]

[%%template:
[@@@mode.default m = (global, local)]
[@@@mode.default c = (uncontended, shared, contended)]

external select
  : ('a : k).
  bool -> ('a[@local_opt]) @ c -> ('a[@local_opt]) @ c -> ('a[@local_opt]) @ c
  @@ portable
  = "caml_csel_value"
[@@kind k = value_or_null_with_imm]
[@@noalloc]
[@@no_effects]
[@@no_coeffects]
[@@builtin]

val select : ('a : k). bool -> 'a @ c m -> 'a @ c m -> 'a @ c m @@ portable
[@@kind
  k = (base_non_value, value & value, value & value & value, value & value & value & value)]
[@@zero_alloc]]
