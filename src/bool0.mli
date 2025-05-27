[%%template:
[@@@mode.default m = (global, local)]

external select
  : ('a : k).
  bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
  @@ portable
  = "caml_csel_value"
[@@kind k = value_or_null] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

val select : ('a : k). bool -> 'a @ m -> 'a @ m -> 'a @ m @@ portable
[@@kind k = (float64, bits32, bits64, word)] [@@zero_alloc]]
