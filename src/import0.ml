(* This module is included in [Import].  It is aimed at modules that define the standard
   combinators for [sexp_of], [of_sexp], [compare] and [hash] and are included in
   [Import]. *)

include
  (Shadow_stdlib
   : (module type of struct include Shadow_stdlib end
       with type 'a ref                           := 'a ref
       with type ('a, 'b, 'c) format              := ('a, 'b, 'c) format
       with type ('a, 'b, 'c, 'd) format4         := ('a, 'b, 'c, 'd) format4
       with type ('a, 'b, 'c, 'd, 'e, 'f) format6 := ('a, 'b, 'c, 'd, 'e, 'f) format6
       with module Pervasives := Pervasives
       (* These modules are redefined in Base *)
       with module Array     := Caml.Array
       with module Buffer    := Caml.Buffer
       with module Bytes     := Caml.Bytes
       with module Char      := Caml.Char
       with module Hashtbl   := Caml.Hashtbl
       with module Int32     := Caml.Int32
       with module Int64     := Caml.Int64
       with module Lazy      := Caml.Lazy
       with module List      := Caml.List
       with module Map       := Caml.Map
       with module Nativeint := Caml.Nativeint
       with module Printf    := Caml.Printf
       with module Queue     := Caml.Queue
       with module Random    := Caml.Random
       with module Set       := Caml.Set
       with module String    := Caml.String
       with module Sys       := Caml.Sys
       with module Uchar     := Caml.Uchar
     ))

type 'a ref = 'a Caml.ref = { mutable contents: 'a }

(* Reshuffle [Caml] so that we choose the modules using labels when available. *)
module Caml = struct


  (** @canonical Caml.Arg *)
  module Arg       = Caml.Arg

  (** @canonical Caml.StdLabels.Array *)
  module Array     = Caml.StdLabels.Array

  (** @canonical Caml.Buffer *)
  module Buffer    = Caml.Buffer

  (** @canonical Caml.Bytes *)
  module Bytes     = Caml.StdLabels.Bytes

  (** @canonical Caml.Char *)
  module Char      = Caml.Char

  (** @canonical Caml.Ephemeron *)
  module Ephemeron = Caml.Ephemeron

  (** @canonical Caml.Format *)
  module Format    = Caml.Format

  (** @canonical Caml.Gc *)
  module Gc        = Caml.Gc

  (** @canonical Caml.MoreLabels.Hashtbl *)
  module Hashtbl   = Caml.MoreLabels.Hashtbl

  (** @canonical Caml.Int32 *)
  module Int32     = Caml.Int32

  (** @canonical Caml.Int64 *)
  module Int64     = Caml.Int64

  (** @canonical Caml.Lazy *)
  module Lazy      = Caml.Lazy

  (** @canonical Caml.Lexing *)
  module Lexing    = Caml.Lexing

  (** @canonical Caml.StdLabels.List *)
  module List      = Caml.StdLabels.List

  (** @canonical Caml.MoreLabels.Map *)
  module Map       = Caml.MoreLabels.Map

  (** @canonical Caml.Nativeint *)
  module Nativeint = Caml.Nativeint

  (** @canonical Caml.Obj *)
  module Obj       = Caml.Obj

  (** @canonical Caml.Parsing *)
  module Parsing   = Caml.Parsing

  (** @canonical Caml.Printexc *)
  module Printexc  = Caml.Printexc

  (** @canonical Caml.Printf *)
  module Printf    = Caml.Printf

  (** @canonical Caml.Queue *)
  module Queue     = Caml.Queue

  (** @canonical Caml.Random *)
  module Random    = Caml.Random

  (** @canonical Caml.Scanf *)
  module Scanf     = Caml.Scanf

  (** @canonical Caml.MoreLabels.Set *)
  module Set       = Caml.MoreLabels.Set

  (** @canonical Caml.Stack *)
  module Stack     = Caml.Stack

  (** @canonical Caml.Stream *)
  module Stream    = Caml.Stream

  (** @canonical Caml.StdLabels.String *)
  module String    = Caml.StdLabels.String

  (** @canonical Caml.Sys *)
  module Sys       = Caml.Sys
  module Uchar     = Caml.Uchar

  include Caml.Pervasives

  exception Not_found = Caml.Not_found
end

external ( |> ) : 'a -> ( 'a -> 'b) -> 'b = "%revapply"

(* These need to be declared as an external to get the lazy behavior *)
external ( && ) : bool -> bool -> bool = "%sequand"
external ( || ) : bool -> bool -> bool = "%sequor"
external not : bool -> bool = "%boolnot"

(* This need to be declared as an external for the warnings to work properly *)
external ignore : _ -> unit = "%ignore"

let ( !=  ) = Caml.( !=  )
let ( *   ) = Caml.( *   )
let ( **  ) = Caml.( **  )
let ( *.  ) = Caml.( *.  )
let ( +   ) = Caml.( +   )
let ( +.  ) = Caml.( +.  )
let ( -   ) = Caml.( -   )
let ( -.  ) = Caml.( -.  )
let ( /   ) = Caml.( /   )
let ( /.  ) = Caml.( /.  )

(** @canonical Base.Polymorphic_compare *)
module Polymorphic_compare = struct
  (** Primitives for polymorphic compare. *)

  (*_ Polymorphic compiler primitives can't be aliases as this doesn't play well with
    inlining. (If aliased without a type annotation, the compiler would implement them
    using the generic code doing a C call, and it's this code that would be inlined.) As a
    result we have to copy the [external ...] declaration here. *)
  external ( < )     : 'a -> 'a -> bool = "%lessthan"
  external ( <= )    : 'a -> 'a -> bool = "%lessequal"
  external ( <> )    : 'a -> 'a -> bool = "%notequal"
  external ( = )     : 'a -> 'a -> bool = "%equal"
  external ( > )     : 'a -> 'a -> bool = "%greaterthan"
  external ( >= )    : 'a -> 'a -> bool = "%greaterequal"
  external ascending : 'a -> 'a -> int  = "%compare"
  external compare   : 'a -> 'a -> int  = "%compare"
  external equal     : 'a -> 'a -> bool = "%equal"
  let descending x y = compare y x
  let max = Pervasives.max
  let min = Pervasives.min
end

(** @canonical Base.Poly *)
module Poly = Polymorphic_compare

module Int_replace_polymorphic_compare = struct
  let ( <  ) (x : int) y = Poly.( <  ) x y
  let ( <= ) (x : int) y = Poly.( <= ) x y
  let ( <> ) (x : int) y = Poly.( <> ) x y
  let ( =  ) (x : int) y = Poly.( =  ) x y
  let ( >  ) (x : int) y = Poly.( >  ) x y
  let ( >= ) (x : int) y = Poly.( >= ) x y

  let ascending  (x : int) y = Poly.ascending  x y
  let descending (x : int) y = Poly.descending x y
  let compare    (x : int) y = Poly.compare    x y
  let equal      (x : int) y = Poly.equal      x y
  let max        (x : int) y = if x >= y then x else y
  let min        (x : int) y = if x <= y then x else y
end

include Int_replace_polymorphic_compare

module Int64_replace_polymorphic_compare = struct
  let ( <  ) (x : Caml.Int64.t) y = Poly.( <  ) x y
  let ( <= ) (x : Caml.Int64.t) y = Poly.( <= ) x y
  let ( <> ) (x : Caml.Int64.t) y = Poly.( <> ) x y
  let ( =  ) (x : Caml.Int64.t) y = Poly.( =  ) x y
  let ( >  ) (x : Caml.Int64.t) y = Poly.( >  ) x y
  let ( >= ) (x : Caml.Int64.t) y = Poly.( >= ) x y

  let ascending  (x : Caml.Int64.t) y = Poly.ascending  x y
  let descending (x : Caml.Int64.t) y = Poly.descending x y
  let compare    (x : Caml.Int64.t) y = Poly.compare    x y
  let equal      (x : Caml.Int64.t) y = Poly.equal      x y
  let max        (x : Caml.Int64.t) y = if x >= y then x else y
  let min        (x : Caml.Int64.t) y = if x <= y then x else y
end

module Nativeint_replace_polymorphic_compare = struct
  let ( <  ) (x : Caml.Nativeint.t) y = Poly.( <  ) x y
  let ( <= ) (x : Caml.Nativeint.t) y = Poly.( <= ) x y
  let ( <> ) (x : Caml.Nativeint.t) y = Poly.( <> ) x y
  let ( =  ) (x : Caml.Nativeint.t) y = Poly.( =  ) x y
  let ( >  ) (x : Caml.Nativeint.t) y = Poly.( >  ) x y
  let ( >= ) (x : Caml.Nativeint.t) y = Poly.( >= ) x y

  let ascending  (x : Caml.Nativeint.t) y = Poly.ascending  x y
  let descending (x : Caml.Nativeint.t) y = Poly.descending x y
  let compare    (x : Caml.Nativeint.t) y = Poly.compare    x y
  let equal      (x : Caml.Nativeint.t) y = Poly.equal      x y
  let max        (x : Caml.Nativeint.t) y = if x >= y then x else y
  let min        (x : Caml.Nativeint.t) y = if x <= y then x else y
end

module Bool_replace_polymorphic_compare = struct
  let ( <  ) (x : bool) y = Poly.( <  ) x y
  let ( <= ) (x : bool) y = Poly.( <= ) x y
  let ( <> ) (x : bool) y = Poly.( <> ) x y
  let ( =  ) (x : bool) y = Poly.( =  ) x y
  let ( >  ) (x : bool) y = Poly.( >  ) x y
  let ( >= ) (x : bool) y = Poly.( >= ) x y

  let ascending  (x : bool) y = Poly.ascending  x y
  let descending (x : bool) y = Poly.descending x y
  let compare    (x : bool) y = Poly.compare    x y
  let equal      (x : bool) y = Poly.equal      x y
  let max        (x : bool) y = if x >= y then x else y
  let min        (x : bool) y = if x <= y then x else y
end

module Char_replace_polymorphic_compare = struct
  let ( <  ) (x : char) y = Poly.( <  ) x y
  let ( <= ) (x : char) y = Poly.( <= ) x y
  let ( <> ) (x : char) y = Poly.( <> ) x y
  let ( =  ) (x : char) y = Poly.( =  ) x y
  let ( >  ) (x : char) y = Poly.( >  ) x y
  let ( >= ) (x : char) y = Poly.( >= ) x y

  let ascending  (x : char) y = Poly.ascending  x y
  let descending (x : char) y = Poly.descending x y
  let compare    (x : char) y = Poly.compare    x y
  let equal      (x : char) y = Poly.equal      x y
  let max        (x : char) y = if x >= y then x else y
  let min        (x : char) y = if x <= y then x else y
end

module Uchar_replace_polymorphic_compare = struct
  let i x = Caml.Uchar.to_int x

  let ( <  ) (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.( <  ) (i x) (i y)
  let ( <= ) (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.( <= ) (i x) (i y)
  let ( <> ) (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.( <> ) (i x) (i y)
  let ( =  ) (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.( =  ) (i x) (i y)
  let ( >  ) (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.( >  ) (i x) (i y)
  let ( >= ) (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.( >= ) (i x) (i y)

  let ascending  (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.ascending  (i x) (i y)
  let descending (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.descending (i x) (i y)
  let compare    (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.compare    (i x) (i y)
  let equal      (x : Caml.Uchar.t) y = Int_replace_polymorphic_compare.equal      (i x) (i y)
  let max        (x : Caml.Uchar.t) y = if x >= y then x else y
  let min        (x : Caml.Uchar.t) y = if x <= y then x else y
end

module Float_replace_polymorphic_compare = struct
  let ( <  ) (x : float) y = Poly.( <  ) x y
  let ( <= ) (x : float) y = Poly.( <= ) x y
  let ( <> ) (x : float) y = Poly.( <> ) x y
  let ( =  ) (x : float) y = Poly.( =  ) x y
  let ( >  ) (x : float) y = Poly.( >  ) x y
  let ( >= ) (x : float) y = Poly.( >= ) x y

  let ascending  (x : float) y = Poly.ascending  x y
  let descending (x : float) y = Poly.descending x y
  let compare    (x : float) y = Poly.compare    x y
  let equal      (x : float) y = Poly.equal      x y
  let max        (x : float) y = if x >= y then x else y
  let min        (x : float) y = if x <= y then x else y
end

module String_replace_polymorphic_compare = struct
  let ( <  ) (x : string) y = Poly.( <  ) x y
  let ( <= ) (x : string) y = Poly.( <= ) x y
  let ( <> ) (x : string) y = Poly.( <> ) x y
  let ( =  ) (x : string) y = Poly.( =  ) x y
  let ( >  ) (x : string) y = Poly.( >  ) x y
  let ( >= ) (x : string) y = Poly.( >= ) x y

  let ascending  (x : string) y = Poly.ascending  x y
  let descending (x : string) y = Poly.descending x y
  let compare    (x : string) y = Poly.compare    x y
  let equal      (x : string) y = Poly.equal      x y
  let max        (x : string) y = if x >= y then x else y
  let min        (x : string) y = if x <= y then x else y
end

module Bytes_replace_polymorphic_compare = struct
  let ( <  ) (x : bytes) y = Poly.( <  ) x y
  let ( <= ) (x : bytes) y = Poly.( <= ) x y
  let ( <> ) (x : bytes) y = Poly.( <> ) x y
  let ( =  ) (x : bytes) y = Poly.( =  ) x y
  let ( >  ) (x : bytes) y = Poly.( >  ) x y
  let ( >= ) (x : bytes) y = Poly.( >= ) x y

  let ascending  (x : bytes) y = Poly.ascending  x y
  let descending (x : bytes) y = Poly.descending x y
  let compare    (x : bytes) y = Poly.compare    x y
  let equal      (x : bytes) y = Poly.equal      x y
  let max        (x : bytes) y = if x >= y then x else y
  let min        (x : bytes) y = if x <= y then x else y
end

(* This needs to be defined as an external so that the compiler can specialize it as a
   direct set or caml_modify *)
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

(* These need to be defined as an external otherwise the compiler won't unbox
   references *)
external ( ! ) : 'a ref -> 'a = "%field0"
external ref : 'a -> 'a ref = "%makemutable"

let ( @   ) = Caml.( @   )
let ( ^   ) = Caml.( ^   )
let ( ~-  ) = Caml.( ~-  )
let ( ~-. ) = Caml.( ~-. )

let ( asr  ) = Caml.( asr  )
let ( land ) = Caml.( land )
let   lnot   = Caml.lnot
let ( lor  ) = Caml.( lor  )
let ( lsl  ) = Caml.( lsl  )
let ( lsr  ) = Caml.( lsr  )
let ( lxor ) = Caml.( lxor )
let ( mod  ) = Caml.( mod  )

let abs             = Caml.abs
let failwith        = Caml.failwith
let fst             = Caml.fst
let invalid_arg     = Caml.invalid_arg
let raise           = Caml.raise
let snd             = Caml.snd

let phys_equal      = Caml.( == )

let decr            = Caml.decr
let incr            = Caml.incr

(* used by sexp_conv, which float0 depends on through option *)
let float_of_string = Caml.float_of_string

(* [am_testing] is used in a few places to behave differently when in testing mode, such
   as in [random.ml].  [am_testing] is implemented using [Base_am_testing], a weak C/js
   primitive that returns [false], but when linking an inline-test-runner executable, is
   overridden by another primitive that returns [true]. *)
external am_testing : unit -> bool = "Base_am_testing"
let am_testing = am_testing ()
