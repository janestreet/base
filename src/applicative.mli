open! Import

include module type of Applicative_intf (** @inline *)

module Make  (X : Basic ) : S  with type  'a      t :=  'a      X.t
module Make2 (X : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) X.t

module Make_using_map2  (X : Basic_using_map2 ) : S  with type  'a      t :=  'a      X.t
module Make2_using_map2 (X : Basic2_using_map2) : S2 with type ('a, 'e) t := ('a, 'e) X.t

module Make_args  (X : S ) : Args  with type  'a      arg :=  'a      X.t
module Make_args2 (X : S2) : Args2 with type ('a, 'e) arg := ('a, 'e) X.t

(** The following functors give a sense of what Applicatives one can define.

    Of these, [Of_monad] is likely the most useful.  The others are mostly didactic. *)

(** Every monad is Applicative via:

    {[
      let apply mf mx =
        mf >>= fun f ->
        mx >>| fun x ->
        f x
    ]} *)
module Of_monad (M : Monad.S)   : S with type 'a t := 'a M.t
module Compose  (F : S) (G : S) : S with type 'a t =  'a F.t G.t
module Pair     (F : S) (G : S) : S with type 'a t =  'a F.t * 'a G.t

(** Every monoid gives rise to a constant Applicative. *)
module Const (Monoid : sig
    type t
    val zero : t
    val plus : t -> t -> t
    (** Laws: [plus] is associative and [zero] is both a left and right unit for [plus] *)
  end)
  : S with type 'a t = Monoid.t
