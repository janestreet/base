open! Import
module Sexp = Sexp0

type 'a inv = 'a -> unit

module Definitions = struct
  type 'a t = 'a inv

  module type S = sig
    type t

    val invariant : t inv
  end

  [%%template
  [@@@kind.default ka = (value, float64, bits32, bits64, word, immediate, immediate64)]

  module type S1 = sig
    type 'a t

    val invariant : 'a inv -> 'a t inv [@@kind ka]
  end

  [@@@kind.default kb = (value, float64, bits32, bits64, word, immediate, immediate64)]

  module type S2 = sig
    type ('a, 'b) t

    val invariant : 'a inv -> 'b inv -> ('a, 'b) t inv [@@kind ka kb]
  end

  [@@@kind.default kc = (value, float64, bits32, bits64, word, immediate, immediate64)]

  module type S3 = sig
    type ('a, 'b, 'c) t

    val invariant : 'a inv -> 'b inv -> 'c inv -> ('a, 'b, 'c) t inv [@@kind ka kb kc]
  end]
end

module type Invariant = sig
  (** This module defines signatures that are to be included in other signatures to ensure
      a consistent interface to invariant-style functions. There is a signature ([S],
      [S1], [S2], [S3]) for each arity of type. Usage looks like:

      {[
        type t
        include Invariant.S with type t := t
      ]}

      or

      {[
        type 'a t
        include Invariant.S1 with type 'a t := 'a t
      ]} *)

  include module type of struct
    include Definitions
  end

  [%%template:
  [@@@kind.default k = (value, float64, bits32, bits64, word, immediate, immediate64)]

  (** [invariant t sexp_of_t f] runs [f ()], and if [f] raises, wraps the exception in an
      [Error.t] that states "invariant failed" and includes both the exception raised by
      [f], as well as [sexp_of_t t]. Idiomatic usage looks like:

      {[
        invariant t [%sexp_of: t] (fun () ->
          ... check t's invariants ... )
      ]}

      For polymorphic types:

      {[
        let invariant check_a t =
          Invariant.invariant t [%sexp_of: _ t] (fun () -> ... )
      ]}

      It's okay to use [ [%sexp_of: _ t] ] because the exceptions raised by [check_a] will
      show the parts that are opaque at top-level. *)
  val invariant
    :  ?here:Stdlib.Lexing.position
    -> 'a
    -> ('a -> Sexp.t)
    -> (unit -> unit)
    -> unit

  (** [check_field] is used when checking invariants using [Fields.iter]. It wraps an
      exception raised when checking a field with the field's name. Idiomatic usage looks
      like:

      {[
        type t =
          { foo : Foo.t
          ; bar : Bar.t
          }
        [@@deriving fields]

        let invariant t : unit =
          Invariant.invariant t [%sexp_of: t] (fun () ->
            let check f = Invariant.check_field t f in
            Fields.iter ~foo:(check Foo.invariant) ~bar:(check Bar.invariant))
        ;;
      ]} *)
  val check_field : 'a -> 'b t -> ('a, 'b) Field.t -> unit]
end
