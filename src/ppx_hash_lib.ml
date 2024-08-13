(** This module is for use by ppx_hash, and is thus not in the interface of Base. *)
module Std = struct
  module Hash = Hash (** @canonical Base.Hash *)
end

(** The functor is exposed to make it possible to use ppx_hash with alternative
      hash types without having to duplicate these definitions. *)
module F (Types : sig
    type hash_state
    type hash_value
  end) =
struct
  open Types

  type 'a hash_fold = hash_state -> 'a -> hash_state

  module Hashable = struct
    module type S_any = sig
      type t

      val hash_fold_t : t hash_fold
      val hash : t -> hash_value
    end

    module type S = sig
      type t

      include S_any with type t := t
    end

    module type S1 = sig
      type 'a t

      val hash_fold_t : 'a hash_fold -> 'a t hash_fold
    end

    module type S2 = sig
      type ('a, 'b) t

      val hash_fold_t : 'a hash_fold -> 'b hash_fold -> ('a, 'b) t hash_fold
    end

    module type S3 = sig
      type ('a, 'b, 'c) t

      val hash_fold_t
        :  'a hash_fold
        -> 'b hash_fold
        -> 'c hash_fold
        -> ('a, 'b, 'c) t hash_fold
    end
  end
end

include F (struct
    type nonrec hash_state = Std.Hash.state
    type nonrec hash_value = Std.Hash.hash_value
  end)
