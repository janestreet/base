external select
  :  bool
  -> ('a[@local_opt])
  -> ('a[@local_opt])
  -> ('a[@local_opt])
  = "caml_csel_value"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]
