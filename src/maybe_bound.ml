open! Import0
module List = List0

type interval_comparison =
  | Below_lower_bound
  | In_range
  | Above_upper_bound
[@@deriving sexp ~stackify, sexp_grammar, compare ~localize, hash]

[%%template
[@@@kind.default k = base_or_null]

type ('a : k) t =
  | Incl of 'a
  | Excl of 'a
  | Unbounded
[@@deriving sexp ~stackify, sexp_grammar, globalize]

let all (all_of_a : ('a List.Constructors.t[@kind k])) : ('a t[@kind k]) list =
  List.rev_append
    ((List.rev_map [@kind k value_or_null]) all_of_a ~f:(fun a : ('a t[@kind k]) ->
       Incl a))
    (List.rev_append
       ((List.rev_map [@kind k value_or_null]) all_of_a ~f:(fun a : ('a t[@kind k]) ->
          Excl a))
       [ (Unbounded : ('a t[@kind k])) ])
;;

let map (t : (_ t[@kind k])) ~f : (_ t[@kind k]) =
  match t with
  | Incl incl -> Incl (f incl)
  | Excl excl -> Excl (f excl)
  | Unbounded -> Unbounded
;;

let is_lower_bound (t : (_ t[@kind k])) ~of_:a ~compare =
  match t with
  | Incl incl -> compare incl a <= 0
  | Excl excl -> compare excl a < 0
  | Unbounded -> true
;;

let is_upper_bound (t : (_ t[@kind k])) ~of_:a ~compare =
  match t with
  | Incl incl -> compare a incl <= 0
  | Excl excl -> compare a excl < 0
  | Unbounded -> true
;;

let bounds_crossed ~(lower : ('a t[@kind k])) ~(upper : ('a t[@kind k])) ~compare =
  match lower with
  | Unbounded -> false
  | Incl lower | Excl lower ->
    (match upper with
     | Unbounded -> false
     | Incl upper | Excl upper -> compare lower upper > 0)
;;

let check_interval_exn ~lower ~upper ~compare =
  if (bounds_crossed [@kind k]) ~lower ~upper ~compare
  then failwith "Maybe_bound.compare_to_interval_exn: lower bound > upper bound"
;;

let compare_to_interval_exn ~lower ~upper a ~compare =
  (check_interval_exn [@kind k]) ~lower ~upper ~compare;
  if not ((is_lower_bound [@kind k]) lower ~of_:a ~compare)
  then Below_lower_bound
  else if not ((is_upper_bound [@kind k]) upper ~of_:a ~compare)
  then Above_upper_bound
  else In_range
;;

let interval_contains_exn ~lower ~upper a ~compare =
  match (compare_to_interval_exn [@kind k]) ~lower ~upper a ~compare with
  | In_range -> true
  | Below_lower_bound | Above_upper_bound -> false
;;]
