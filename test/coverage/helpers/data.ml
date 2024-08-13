open! Base
open Base_quickcheck
open Overrides
include Data_intf.Definitions

module Int : S with type t = int = struct
  type t = int [@@deriving compare, equal, quickcheck, sexp_of]

  let to_int = Fn.id
  let of_int = Fn.id
  let combine_non_commutative a b = (a * 10) + b
end

module List (T : With_equal) = struct
  type t = T.t list [@@deriving equal, sexp_of]
end

module Or_error (T : With_equal) = struct
  type t = (T.t, (Error.t[@equal.ignore])) Result.t [@@deriving equal, sexp_of]
end

module Option (T : With_equal) = struct
  type t = T.t option [@@deriving equal, sexp_of]
end

module Pair (T : With_quickcheck) = struct
  type t = T.t * T.t [@@deriving equal, quickcheck, sexp_of]

  let quickcheck_generator =
    let open Base_quickcheck.Generator.Let_syntax in
    match%bind Base_quickcheck.Generator.bool with
    | true -> [%generator: t]
    | false ->
      let%map x = [%generator: T.t] in
      x, x
  ;;
end
