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
       with module Char      := Caml.Char
       with module Hashtbl   := Caml.Hashtbl
       with module Int32     := Caml.Int32
       with module Int64     := Caml.Int64
       with module Lazy      := Caml.Lazy
       with module List      := Caml.List
       with module Map       := Caml.Map
       with module Nativeint := Caml.Nativeint
       with module Printf    := Caml.Printf
       with module Random    := Caml.Random
       with module Set       := Caml.Set
       with module String    := Caml.String
     ))

type 'a ref = 'a Caml.ref = { mutable contents: 'a }

(* Reshuffle [Caml] so that we choose the modules using labels when available. *)
module Caml = struct
  module Arg       = Caml.Arg
  module Array     = Caml.StdLabels.Array
  module Buffer    = Caml.Buffer
  module Char      = Caml.Char
  module Ephemeron = Caml.Ephemeron
  module Format    = Caml.Format
  module Gc        = Caml.Gc
  module Hashtbl   = Caml.MoreLabels.Hashtbl
  module Int32     = Caml.Int32
  module Int64     = Caml.Int64
  module Lazy      = Caml.Lazy
  module Lexing    = Caml.Lexing
  module List      = Caml.StdLabels.List
  module Map       = Caml.MoreLabels.Map
  module Nativeint = Caml.Nativeint
  module Obj       = Caml.Obj
  module Parsing   = Caml.Parsing
  module Printexc  = Caml.Printexc
  module Printf    = Caml.Printf
  module Queue     = Caml.Queue
  module Random    = Caml.Random
  module Scanf     = Caml.Scanf
  module Set       = Caml.MoreLabels.Set
  module Stack     = Caml.Stack
  module Stream    = Caml.Stream
  module String    = Caml.StdLabels.Bytes
  module Sys       = Caml.Sys

  include Caml.Pervasives
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

module Polymorphic_compare = struct
  (* Polymorphic compiler primitives can't be aliases as this doesn't play well with
     inlining.  When aliased without a type annotation, the compiler will implement them
     using the generic code doing a C call.  And it is this code that will be inlined. As
     a result we have to copy the [external ...] declaration here.

     {[
       let ( <   ) = Caml.( <   )
       let ( <=  ) = Caml.( <=  )
       let ( <>  ) = Caml.( <>  )
       let ( =   ) = Caml.( =   )
       let ( >   ) = Caml.( >   )
       let ( >=  ) = Caml.( >=  )
     ]} *)
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
  let max        (x : int) y = Poly.max        x y
  let min        (x : int) y = Poly.min        x y
end

include Int_replace_polymorphic_compare

let ( <.  ) (x : float) y = Poly.( <  ) x y
let ( <=. ) (x : float) y = Poly.( <= ) x y
let ( =.  ) (x : float) y = Poly.( =  ) x y
let ( >.  ) (x : float) y = Poly.( >  ) x y
let ( >=. ) (x : float) y = Poly.( >= ) x y

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
let decr            = Caml.decr
let exit            = Caml.exit
let failwith        = Caml.failwith
let float_of_int    = Caml.float_of_int
let float_of_string = Caml.float_of_string
let fst             = Caml.fst
let incr            = Caml.incr
let int_of_float    = Caml.int_of_float
let invalid_arg     = Caml.invalid_arg
let max_int         = Caml.max_int
let min_int         = Caml.min_int
let raise           = Caml.raise
let snd             = Caml.snd
let succ            = Caml.succ
let string_of_int   = Caml.string_of_int

let phys_equal = Caml.( == )

(* [am_testing] is used in a few places to behave differently when in testing mode, such
   as in [random.ml].  [am_testing] is implemented using [Base_am_testing], a weak C/js
   primitive that returns [false], but when linking an inline-test-runner executable, is
   overridden by another primitive that returns [true]. *)
external am_testing : unit -> bool = "Base_am_testing"
let am_testing = am_testing ()
