(* This module is included in [Import]. It is aimed at modules that define the standard
   combinators for [sexp_of], [of_sexp], [compare] and [hash] and are included in
   [Import]. *)

[@@@warning "-incompatible-with-upstream"]

include (
  Shadow_stdlib :
    module type of struct
      include Shadow_stdlib
    end
    with type ('a : value_or_null) ref := 'a ref
    with type ('a, 'b, 'c) format := ('a, 'b, 'c) format
    with type ('a, 'b, 'c, 'd) format4 := ('a, 'b, 'c, 'd) format4
    with type ('a, 'b, 'c, 'd, 'e, 'f) format6 := ('a, 'b, 'c, 'd, 'e, 'f) format6
    (* These modules are redefined in Base *)
    with module Array := Shadow_stdlib.Array
    with module Atomic := Shadow_stdlib.Atomic
    with module Bool := Shadow_stdlib.Bool
    with module Buffer := Shadow_stdlib.Buffer
    with module Bytes := Shadow_stdlib.Bytes
    with module Char := Shadow_stdlib.Char
    with module Either := Shadow_stdlib.Either
    with module Float := Shadow_stdlib.Float
    with module Hashtbl := Shadow_stdlib.Hashtbl
    with module Int := Shadow_stdlib.Int
    with module Int32 := Shadow_stdlib.Int32
    with module Int64 := Shadow_stdlib.Int64
    with module Lazy := Shadow_stdlib.Lazy
    with module List := Shadow_stdlib.List
    with module Map := Shadow_stdlib.Map
    with module Modes := Shadow_stdlib.Modes
    with module Nativeint := Shadow_stdlib.Nativeint
    with module Obj := Shadow_stdlib.Obj
    with module Option := Shadow_stdlib.Option
    with module Printf := Shadow_stdlib.Printf
    with module Queue := Shadow_stdlib.Queue
    with module Random := Shadow_stdlib.Random
    with module Result := Shadow_stdlib.Result
    with module Set := Shadow_stdlib.Set
    with module Stack := Shadow_stdlib.Stack
    with module String := Shadow_stdlib.String
    with module Sys := Shadow_stdlib.Sys
    with module Uchar := Shadow_stdlib.Uchar
    with module Unit := Shadow_stdlib.Unit)
[@ocaml.warning "-3"]

[@@@warning "-incompatible-with-upstream"]

type ('a : value_or_null) ref = 'a Stdlib.ref = { mutable contents : 'a }
type ('a : any mod separable) iarray = 'a Basement.Stdlib_iarray_labels.t
type 'a or_null = 'a Basement.Or_null_shim.t [@@or_null_reexport]

module Stdlib = struct
  include Stdlib

  (* Reshuffle [Stdlib] so that we choose the modules using labels when available. *)
  include Stdlib.StdLabels
  include Stdlib.MoreLabels

  (* Shadow unsafe [Stdlib] functions with their safe versions from
     [Basement.Stdlib_shim]. *)

  module Atomic = struct
    include Stdlib.Atomic
    include Basement.Stdlib_shim.Atomic
    include Basement.Stdlib_shim.Atomic.Local
  end

  module Domain = struct
    include Stdlib.Domain
    include Basement.Stdlib_shim.Domain.Safe
  end

  module Format = struct
    include Stdlib.Format
    include Basement.Stdlib_shim.Format.Safe
  end

  module Obj = struct
    include Stdlib.Obj
    include Basement.Stdlib_shim.Obj

    module Extension_constructor = struct
      include Stdlib.Obj.Extension_constructor
      include Basement.Stdlib_shim.Obj.Extension_constructor
    end
  end

  module Printexc = struct
    include Stdlib.Printexc
    include Basement.Stdlib_shim.Printexc.Safe
  end
end

module Portability_hacks = Basement.Portability_hacks

external ( |> ) : 'a -> (('a -> 'b)[@local_opt]) -> 'b @@ portable = "%revapply"

(* These need to be declared as an external to get the lazy behavior *)
external ( && )
  :  (bool[@local_opt])
  -> (bool[@local_opt])
  -> bool
  @@ portable
  = "%sequand"

external ( || ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool @@ portable = "%sequor"
external not : (bool[@local_opt]) -> bool @@ portable = "%boolnot"

(* We use a binding to [%identity] here because [Obj.magic] is an optimization barrier. *)
external bool_to_int : bool -> int @@ portable = "%identity"

(* This needs to be declared as an external for the warnings to work properly *)
external ignore : _ -> unit @@ portable = "%ignore"
external ignore_contended : _ @ contended -> unit @@ portable = "%ignore"

let ( != ) = Stdlib.( != )
let ( * ) = Stdlib.( * )

external ( ** )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> float
  @@ portable
  = "caml_power_float" "pow"
[@@unboxed] [@@noalloc]

external ( *. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  @@ portable
  = "%mulfloat"

let ( + ) = Stdlib.( + )

external ( +. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  @@ portable
  = "%addfloat"

let ( - ) = Stdlib.( - )

external ( -. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  @@ portable
  = "%subfloat"

let ( / ) = Stdlib.( / )

external ( /. )
  :  (float[@local_opt])
  -> (float[@local_opt])
  -> (float[@local_opt])
  @@ portable
  = "%divfloat"

module Hash = Ppx_hash_lib.Std.Hash

module Poly = Poly0 (** @canonical Base.Poly *)

include Replace_polymorphic_compare
include Int_replace_polymorphic_compare

(* This needs to be defined as an external so that the compiler can specialize it as a
   direct set or caml_modify. *)
external ( := )
  : ('a : value_or_null).
  ('a ref[@local_opt]) -> 'a -> unit
  @@ portable
  = "%setfield0"

(* These need to be defined as an external otherwise the compiler won't unbox references. *)
external ( ! ) : ('a : value_or_null). ('a ref[@local_opt]) -> 'a @@ portable = "%field0"

external ref
  : ('a : value_or_null).
  'a -> ('a ref[@local_opt])
  @@ portable
  = "%makemutable"

let ( @ ) = Stdlib.( @ )
let ( ^ ) = Stdlib.( ^ )
let ( ~- ) = Stdlib.( ~- )

external ( .:() )
  :  ('a iarray[@local_opt])
  -> int
  -> ('a[@local_opt])
  @@ portable
  = "%array_safe_get"

external ( ~-. ) : (float[@local_opt]) -> (float[@local_opt]) @@ portable = "%negfloat"

let ( asr ) = Stdlib.( asr )

external ( land ) : local_ int -> local_ int -> int @@ portable = "%andint"

let lnot = Stdlib.lnot
let ( lor ) = Stdlib.( lor )
let ( lsl ) = Stdlib.( lsl )
let ( lsr ) = Stdlib.( lsr )
let ( lxor ) = Stdlib.( lxor )
let ( mod ) = Stdlib.( mod )
let abs = Stdlib.abs
let failwith = Stdlib.failwith
let invalid_arg = Stdlib.invalid_arg

external fst
  : ('a : value_or_null) ('b : value_or_null).
  ('a * 'b[@local_opt]) -> ('a[@local_opt])
  @@ portable
  = "%field0_immut"

external snd
  : ('a : value_or_null) ('b : value_or_null).
  ('a * 'b[@local_opt]) -> ('b[@local_opt])
  @@ portable
  = "%field1_immut"

(* [raise] needs to be defined as an external as the compiler automatically replaces
   '%raise' by '%reraise' when appropriate. *)
  external%template raise
    : ('a : value_or_null).
    exn -> 'a @ portable unique
    @@ portable
    = "%reraise"

[%%template
[@@@kind kr1 = (value & value)]
[@@@kind kr2 = (value & value & value)]
[@@@kind kr3 = (value & value & value & value)]

let raise : ('a : k). (exn -> 'a @ portable unique) @ portable =
  fun exn ->
  match (raise exn : Nothing0.t) with
  | _ -> .
[@@kind k = (base_non_value, value & (base, kr1, kr2, kr3), bits32 & bits32)]
;;]

external phys_equal : ('a[@local_opt]) -> ('a[@local_opt]) -> bool @@ portable = "%eq"
external decr : (int ref[@local_opt]) -> unit @@ portable = "%decr"
external incr : (int ref[@local_opt]) -> unit @@ portable = "%incr"

(* Used by sexp_conv, which float0 depends on through option. *)
let float_of_string = Stdlib.float_of_string

(* [am_testing] is used in a few places to behave differently when in testing mode, such
   as in [random.ml]. [am_testing] is implemented using [Base_am_testing], a weak C/js
   primitive that returns [false], but when linking an inline-test-runner executable, is
   overridden by another primitive that returns [true]. *)
external am_testing : unit -> bool @@ portable = "Base_am_testing"

let am_testing = am_testing ()
