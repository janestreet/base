(** Standard type for [blit] functions, and reusable code for validating [blit] arguments. *)

open! Import

[@@@warning "-incompatible-with-upstream"]

module Definitions = struct
  (** If [blit : (src, dst) blit], then [blit ~src ~src_pos ~len ~dst ~dst_pos] blits
      [len] values from [src] starting at position [src_pos] to [dst] at position
      [dst_pos]. Furthermore, [blit] raises if [src_pos], [len], and [dst_pos] don't
      specify valid slices of [src] and [dst]. *)
  type ('src, 'dst) blit =
    src:local_ 'src -> src_pos:int -> dst:local_ 'dst -> dst_pos:int -> len:int -> unit

  (** [blito] is like [blit], except that the [src_pos], [src_len], and [dst_pos] are
      optional (hence the "o" in "blito"). Also, we use [src_len] rather than [len] as a
      reminder that if [src_len] isn't supplied, then the default is to take the slice
      running from [src_pos] to the end of [src]. *)
  type ('src, 'dst) blito =
    src:local_ 'src
    -> ?src_pos:int (** default is [0] *)
    -> ?src_len:int (** default is [length src - src_pos] *)
    -> dst:local_ 'dst
    -> ?dst_pos:int (** default is [0] *)
    -> unit
    -> unit

  (** If [sub : (src, dst) sub], then [sub ~src ~pos ~len] returns a sequence of type
      [dst] containing [len] characters of [src] starting at [pos].

      [subo] is like [sub], except [pos] and [len] are optional. *)
  type ('src, 'dst) sub = local_ 'src -> pos:int -> len:int -> 'dst

  type ('src, 'dst) subo =
    ?pos:int (** default is [0] *)
    -> ?len:int (** default is [length src - pos] *)
    -> local_ 'src
    -> 'dst

  (** Like [blit], but not allowing [local_] values (on compilers supporting modes). *)
  type ('src, 'dst) blit_global =
    src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit

  (** Like [blito], but not allowing [local_] values (on compilers supporting modes). *)
  type ('src, 'dst) blito_global =
    src:'src -> ?src_pos:int -> ?src_len:int -> dst:'dst -> ?dst_pos:int -> unit -> unit

  (** Like [sub], but not allowing [local_] values (on compilers supporting modes). *)
  type ('src, 'dst) sub_global = 'src -> pos:int -> len:int -> 'dst

  (** Like [subo], but not allowing [local_] values (on compilers supporting modes). *)
  type ('src, 'dst) subo_global = ?pos:int -> ?len:int -> 'src -> 'dst

  (** Blit for distinct [src] and [dst] types that each have two parameters: ['elt] that
      must be the same in both types, and ['phantom] that can be different. *)
  module type%template S1_phantom2_distinct = sig
    type ('elt : k, 'p1, 'p2) src
    type ('elt : k, 'p1, 'p2) dst

    val blit : (('a, _, _) src, ('a, _, _) dst) blit
    val blito : (('a, _, _) src, ('a, _, _) dst) blito
    val unsafe_blit : (('a, _, _) src, ('a, _, _) dst) blit
    val sub : (('a, _, _) src, ('a, _, _) dst) sub
    val subo : (('a, _, _) src, ('a, _, _) dst) subo
  end
  [@@kind k = (value, immediate, immediate64)]

  module type%template S = sig
    type t

    include
      S1_phantom2_distinct
      [@kind k]
      with type (_, _, _) src := t
       and type (_, _, _) dst := t
  end
  [@@kind k = (value, immediate, immediate64)]

  module type%template S1 = sig
    type ('a : k) t

    include
      S1_phantom2_distinct
      [@kind k]
      with type ('a, _, _) src := 'a t
       and type ('a, _, _) dst := 'a t
  end
  [@@kind k = (value, immediate, immediate64)]

  module type S_distinct = sig
    type src
    type dst

    include
      S1_phantom2_distinct with type (_, _, _) src := src and type (_, _, _) dst := dst
  end

  module type S_distinct_global = sig
    type src
    type dst

    val blit : (src, dst) blit_global
    val blito : (src, dst) blito_global
    val unsafe_blit : (src, dst) blit_global
    val sub : (src, dst) sub_global
    val subo : (src, dst) subo_global
  end

  module type S_phantom_distinct = sig
    type 'a src
    type 'a dst

    include
      S1_phantom2_distinct
      with type (_, 'a, _) src := 'a src
       and type (_, 'a, _) dst := 'a dst
  end

  module type S_to_string = sig
    type t

    val sub : (t, string) sub
    val subo : (t, string) subo
  end

  module type S_to_string_global = sig
    type t

    val sub : (t, string) sub_global
    val subo : (t, string) subo_global
  end

  (** Users of modules matching the blit signatures [S], [S1], [S_phantom_distinct], and
      [S1_phantom2_distinct] only need to understand the code above. The code below is
      only for those that need to implement modules that match those signatures. *)

  module type Sequence = sig
    type t

    val length : local_ t -> int
  end

  module type%template Sequence1 = sig
    type ('a : k) t

    (** [Make1*] guarantees to only call [create_like ~len t] with [len > 0] if
        [length t > 0]. *)
    val create_like : len:int -> local_ 'a t -> 'a t

    val length : local_ _ t -> int
    val unsafe_blit : ('a t, 'a t) blit
  end
  [@@kind k = (value, immediate, immediate64)]
end

module type Blit = sig @@ portable
  include module type of struct
    include Definitions
  end

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
      include Sequence

      val create : len:int -> t
      val unsafe_blit : (t, t) blit
    end) : S with type t := Sequence.t

  (** [Make_distinct] is for blitting between values of distinct monomorphic types. *)
  module%template.portable Make_distinct
      (Src : Sequence)
      (Dst : sig
         include Sequence

         val create : len:int -> t
         val unsafe_blit : (Src.t, t) blit
       end) : S_distinct with type src := Src.t with type dst := Dst.t

  module%template.portable Make_to_string
      (T : sig
         type t
       end)
      (To_bytes : S_distinct with type src := T.t with type dst := bytes) :
    S_to_string with type t := T.t

  (** [Make1] is for blitting between two values of the same polymorphic type. *)
  module%template.portable Make1 (Sequence : Sequence1 [@kind k]) :
    S1 [@kind k] with type ('a : k) t := 'a Sequence.t
  [@@kind k = (value, immediate, immediate64)]

  module%template.portable Make1_phantom2_distinct
      (Src : sig
         type ('elt, 'p1, 'p2) t

         val length : local_ (_, _, _) t -> int
       end)
      (Dst : sig
         type ('elt, 'p1, 'p2) t

         val length : local_ (_, _, _) t -> int
         val create_like : len:int -> local_ ('elt, _, _) Src.t -> ('elt, _, _) t
         val unsafe_blit : (('elt, _, _) Src.t, ('elt, _, _) t) blit
       end) :
    S1_phantom2_distinct
    with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Src.t
    with type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Dst.t
end
