(** This module defines signatures that are to be included in other signatures to ensure a
    consistent interface to [equal] functions. There is a signature ([S], [S1], [S2],
    [S3]) for each arity of type. Usage looks like:

    {[
      type t
      include Equal.S with type t := t
    ]}

    or

    {[
      type 'a t
      include Equal.S1 with type 'a t := 'a t
    ]} *)

open! Import

[%%template
[@@@mode.default m = (global, local)]

type 'a t = 'a @ m -> 'a @ m -> bool
type 'a equal = ('a t[@mode m])

module type S = sig
  type t

  val%template equal : (t equal[@mode m]) [@@mode m = (global, m)]
end

module type S1 = sig
  type 'a t

  val%template equal : ('a equal[@mode m]) -> ('a t equal[@mode m])
  [@@mode m = (global, m)]
end

module type S2 = sig
  type ('a, 'b) t

  val%template equal
    :  ('a equal[@mode m])
    -> ('b equal[@mode m])
    -> (('a, 'b) t equal[@mode m])
  [@@mode m = (global, m)]
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val%template equal
    :  ('a equal[@mode m])
    -> ('b equal[@mode m])
    -> ('c equal[@mode m])
    -> (('a, 'b, 'c) t equal[@mode m])
  [@@mode m = (global, m)]
end]
