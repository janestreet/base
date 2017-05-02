(** This module is for use by ppx_sexp_conv, and is thus not in the interface of
    Base. *)
module Conv_error = Sexp_conv_error
module Conv       = Sexp_conv

(** @canonical Base.Sexp *)
module Sexp       = Sexp
