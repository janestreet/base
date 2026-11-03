[%%template:
[@@@mode.default m = (global, local)]
[@@@mode.default c = (uncontended, shared, contended)]
[@@@mode.default v = (read_write, read)]

external select
  : ('a : k).
  bool -> ('a[@local_opt]) @ c v -> ('a[@local_opt]) @ c v -> ('a[@local_opt]) @ c v
  @@ portable
  = "caml_csel_value"
[@@kind k = value_or_null] [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]

val select : ('a : k). bool -> 'a @ c m v -> 'a @ c m v -> 'a @ c m v @@ portable
[@@kind
  k
  = ( base_non_value
    , value_or_null & value_or_null
    , value_or_null & value_or_null & value_or_null
    , value_or_null & value_or_null & value_or_null & value_or_null
    , value_or_null & bits64 )]
[@@zero_alloc]]
