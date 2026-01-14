(** Standard type for [blit] functions, and reusable code for validating [blit] arguments. *)

open! Import

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@kind_set.define values = (value, value mod external64)]

module Definitions = struct
  [@@@mode.default v = (read_write, read, immutable)]

  (** If [blit : (src, dst) blit], then [blit ~src ~src_pos ~len ~dst ~dst_pos] blits
      [len] values from [src] starting at position [src_pos] to [dst] at position
      [dst_pos]. Furthermore, [blit] raises if [src_pos], [len], and [dst_pos] don't
      specify valid slices of [src] and [dst]. *)
  type ('src, 'dst) blit =
    src:'src @ local v
    -> src_pos:int
    -> dst:'dst @ local
    -> dst_pos:int
    -> len:int
    -> unit

  (** [blito] is like [blit], except that the [src_pos], [src_len], and [dst_pos] are
      optional (hence the "o" in "blito"). Also, we use [src_len] rather than [len] as a
      reminder that if [src_len] isn't supplied, then the default is to take the slice
      running from [src_pos] to the end of [src]. *)
  type ('src, 'dst) blito =
    src:'src @ local v
    -> ?src_pos:int (** default is [0] *)
    -> ?src_len:int (** default is [length src - src_pos] *)
    -> dst:'dst @ local
    -> ?dst_pos:int (** default is [0] *)
    -> unit
    -> unit

  (** If [sub : (src, dst) sub], then [sub ~src ~pos ~len] returns a sequence of type
      [dst] containing [len] characters of [src] starting at [pos].

      [subo] is like [sub], except [pos] and [len] are optional. *)
  type ('src, 'dst) sub = 'src @ local v -> pos:int -> len:int -> 'dst

  type ('src, 'dst) subo =
    ?pos:int (** default is [0] *)
    -> ?len:int (** default is [length src - pos] *)
    -> 'src @ local v
    -> 'dst

  (** Like [blit], but not allowing [local_] values (on compilers supporting modes). *)
  type ('src, 'dst) blit_global =
    src:'src @ v -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit

  (** Like [blito], but not allowing [local_] values (on compilers supporting modes). *)
  type ('src, 'dst) blito_global =
    src:'src @ v
    -> ?src_pos:int
    -> ?src_len:int
    -> dst:'dst
    -> ?dst_pos:int
    -> unit
    -> unit

  (** Like [sub], but not allowing [local_] values (on compilers supporting modes). *)
  type ('src, 'dst) sub_global = 'src @ v -> pos:int -> len:int -> 'dst

  (** Like [subo], but not allowing [local_] values (on compilers supporting modes). *)
  type ('src, 'dst) subo_global = ?pos:int -> ?len:int -> 'src @ v -> 'dst

  (** Blit for distinct [src] and [dst] types that each have two parameters: ['elt] that
      must be the same in both types, and ['phantom] that can be different. *)
  module type S1_phantom2_distinct = sig
    type ('elt : k, 'p1, 'p2) src
    type ('elt : k, 'p1, 'p2) dst

    val blit : ((('a, _, _) src, ('a, _, _) dst) blit[@mode v])
    val blito : ((('a, _, _) src, ('a, _, _) dst) blito[@mode v])
    val unsafe_blit : ((('a, _, _) src, ('a, _, _) dst) blit[@mode v])
    val sub : ((('a, _, _) src, ('a, _, _) dst) sub[@mode v])
    val subo : ((('a, _, _) src, ('a, _, _) dst) subo[@mode v])
  end
  [@@kind k = values]

  module type S = sig
    type t

    include
      S1_phantom2_distinct
      [@kind k] [@mode v]
      with type (_, _, _) src := t
       and type (_, _, _) dst := t
  end
  [@@kind k = values]

  module type S1 = sig
    type ('a : k) t

    include
      S1_phantom2_distinct
      [@kind k] [@mode v]
      with type ('a, _, _) src := 'a t
       and type ('a, _, _) dst := 'a t
  end
  [@@kind k = values]

  module type S_distinct = sig
    type src
    type dst

    include
      S1_phantom2_distinct
      [@mode v]
      with type (_, _, _) src := src
       and type (_, _, _) dst := dst
  end

  module type S_distinct_global = sig
    type src
    type dst

    val blit : ((src, dst) blit_global[@mode v])
    val blito : ((src, dst) blito_global[@mode v])
    val unsafe_blit : ((src, dst) blit_global[@mode v])
    val sub : ((src, dst) sub_global[@mode v])
    val subo : ((src, dst) subo_global[@mode v])
  end

  module type S_phantom_distinct = sig
    type 'a src
    type 'a dst

    include
      S1_phantom2_distinct
      [@mode v]
      with type (_, 'a, _) src := 'a src
       and type (_, 'a, _) dst := 'a dst
  end

  module type S_to_string = sig
    type t

    val sub : ((t, string) sub[@mode v])
    val subo : ((t, string) subo[@mode v])
  end

  module type S_to_string_global = sig
    type t

    val sub : ((t, string) sub_global[@mode v])
    val subo : ((t, string) subo_global[@mode v])
  end

  (** Users of modules matching the blit signatures [S], [S1], [S_phantom_distinct], and
      [S1_phantom2_distinct] only need to understand the code above. The code below is
      only for those that need to implement modules that match those signatures. *)

  module type Sequence = sig
    type t

    val length : t @ local v -> int
  end

  module type Sequence1 = sig
    type ('a : k) t

    (** [Make1*] guarantees to only call [create_like ~len t] with [len > 0] if
        [length t > 0]. *)
    val create_like : len:int -> 'a t @ local v -> 'a t

    val length : _ t @ local v -> int
    val unsafe_blit : (('a t, 'a t) blit[@mode v])
  end
  [@@kind k = values]
end

module type Blit = sig @@ portable
  include module type of struct
    include Definitions
  end

  [@@@mode.default v = (read_write, read, immutable)]

  (** There are various [Make*] functors that turn an [unsafe_blit] function into a [blit]
      function. The functors differ in whether the sequence type is monomorphic or
      polymorphic, and whether the src and dst types are distinct or are the same.

      The blit functions make sure the slices are valid and then call [unsafe_blit]. They
      guarantee at a call [unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len] that:

      {[
        len > 0
        && src_pos >= 0
        && src_pos + len <= get_src_len src
        && dst_pos >= 0
        && dst_pos + len <= get_dst_len dst
      ]}

      The [Make*] functors also automatically create unit tests. *)

  (** [Make] is for blitting between two values of the same monomorphic type. *)
  module%template.portable Make (Sequence : sig
      include Sequence [@mode v]

      val create : len:int -> t
      val unsafe_blit : ((t, t) blit[@mode v])
    end) : S [@mode v] with type t := Sequence.t

  (** [Make_distinct] is for blitting between values of distinct monomorphic types. *)
  module%template.portable Make_distinct
      (Src : Sequence
    [@mode v])
      (Dst : sig
         include Sequence

         val create : len:int -> t
         val unsafe_blit : ((Src.t, t) blit[@mode v])
       end) : S_distinct [@mode v] with type src := Src.t with type dst := Dst.t

  module%template.portable Make_to_string
      (T : sig
         type t
       end)
      (To_bytes : S_distinct [@mode v] with type src := T.t with type dst := bytes) :
    S_to_string [@mode v] with type t := T.t

  (** [Make1] is for blitting between two values of the same polymorphic type. *)
  module%template.portable Make1 (Sequence : Sequence1 [@kind k] [@mode v]) :
    S1 [@kind k] [@mode v] with type ('a : k) t := 'a Sequence.t
  [@@kind k = values]

  module%template.portable Make1_phantom2_distinct
      (Src : sig
         type ('elt, 'p1, 'p2) t

         val length : (_, _, _) t @ local v -> int
       end)
      (Dst : sig
         type ('elt, 'p1, 'p2) t

         val length : (_, _, _) t @ local -> int
         val create_like : len:int -> ('elt, _, _) Src.t @ local v -> ('elt, _, _) t
         val unsafe_blit : ((('elt, _, _) Src.t, ('elt, _, _) t) blit[@mode v])
       end) :
    S1_phantom2_distinct
    [@mode v]
    with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Src.t
    with type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Dst.t
end]
