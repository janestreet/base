open! Import

include (
struct
  type ('a : value_or_null) t = 'a ref
  [@@deriving compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar]
end :
sig
@@ portable
  type ('a : value_or_null) t = 'a ref
  [@@deriving compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar]
end)

(* In the definition of [t], we do not have [[@@deriving compare, sexp]] because in
   general, syntax extensions tend to use the implementation when available rather than
   using the alias. Here that would lead to use the record representation
   [ { mutable contents : 'a } ] which would result in different (and unwanted) behavior. *)
type ('a : value_or_null) t = 'a ref = { mutable contents : 'a }

external create
  : ('a : value_or_null).
  'a -> ('a t[@local_opt])
  @@ portable
  = "%makemutable"

external ( ! ) : ('a : value_or_null). ('a t[@local_opt]) -> 'a @@ portable = "%field0"

external ( := )
  : ('a : value_or_null).
  ('a t[@local_opt]) -> 'a -> unit
  @@ portable
  = "%setfield0"

let swap t1 t2 =
  let tmp = !t1 in
  t1 := !t2;
  t2 := tmp
;;

let replace t f = t := f !t

let set_temporarily t a ~f =
  let restore_to = !t in
  t := a;
  Exn.protect ~f ~finally:(fun () -> t := restore_to)
;;

module And_value = struct
  type t = T : 'a ref * 'a -> t [@@deriving sexp_of ~stackify]

  let set (T (r, a)) = r := a
  let sets ts = List.iter ts ~f:set
  let snapshot (T (r, _)) = T (r, !r)
  let snapshots ts = List.map ts ~f:snapshot
end

let sets_temporarily and_values ~f =
  let restore_to = And_value.snapshots and_values in
  And_value.sets and_values;
  Exn.protect ~f ~finally:(fun () -> And_value.sets restore_to)
;;
