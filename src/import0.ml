(* This module is included in [Import].  It is aimed at modules that define the standard
   combinators for [sexp_of], [of_sexp], [compare] and [hash] and are included in
   [Import]. *)

include (
  Shadow_stdlib :
    module type of struct
      include Shadow_stdlib
    end
    with type 'a ref := 'a ref
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
    with module Nativeint := Shadow_stdlib.Nativeint
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

type 'a ref = 'a Stdlib.ref = { mutable contents : 'a }

(* Reshuffle [Stdlib] so that we choose the modules using labels when available. *)
module Stdlib = struct
  include Stdlib
  include Stdlib.StdLabels
  include Stdlib.MoreLabels
end

external ( |> ) : 'a -> (('a -> 'b)[@local_opt]) -> 'b = "%revapply"

(* These need to be declared as an external to get the lazy behavior *)
external ( && ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool = "%sequand"
external ( || ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool = "%sequor"
external not : (bool[@local_opt]) -> bool = "%boolnot"

(* We use [Obj.magic] here as other implementations generate a conditional jump and the
   performance difference is noticeable. *)
let bool_to_int (x : bool) : int = Stdlib.Obj.magic x

(* This needs to be declared as an external for the warnings to work properly *)
external ignore : _ -> unit = "%ignore"

let ( != ) = Stdlib.( != )
let ( * ) = Stdlib.( * )
let ( ** ) = Stdlib.( ** )
let ( *. ) = Stdlib.( *. )
let ( + ) = Stdlib.( + )
let ( +. ) = Stdlib.( +. )
let ( - ) = Stdlib.( - )
let ( -. ) = Stdlib.( -. )
let ( / ) = Stdlib.( / )
let ( /. ) = Stdlib.( /. )

module Poly = Poly0 (** @canonical Base.Poly *)

module Int_replace_polymorphic_compare = struct
  (* Declared as externals so that the compiler skips the caml_apply_X wrapping even when
     compiling without cross library inlining. *)
  external ( = ) : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%equal"
  external ( <> ) : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%notequal"
  external ( < ) : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%lessthan"
  external ( > ) : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%greaterthan"
  external ( <= ) : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%lessequal"
  external ( >= ) : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%greaterequal"
  external compare : (int[@local_opt]) -> (int[@local_opt]) -> int = "%compare"
  external compare__local : (int[@local_opt]) -> (int[@local_opt]) -> int = "%compare"
  external equal : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%equal"
  external equal__local : (int[@local_opt]) -> (int[@local_opt]) -> bool = "%equal"

  let ascending (x : int) y = compare x y
  let descending (x : int) y = compare y x
  let max (x : int) y = Bool0.select (x >= y) x y
  let min (x : int) y = Bool0.select (x <= y) x y
end

include Int_replace_polymorphic_compare

module Int32_replace_polymorphic_compare = struct
  let ( < ) (x : Stdlib.Int32.t) y = Poly.( < ) x y
  let ( <= ) (x : Stdlib.Int32.t) y = Poly.( <= ) x y
  let ( <> ) (x : Stdlib.Int32.t) y = Poly.( <> ) x y
  let ( = ) (x : Stdlib.Int32.t) y = Poly.( = ) x y
  let ( > ) (x : Stdlib.Int32.t) y = Poly.( > ) x y
  let ( >= ) (x : Stdlib.Int32.t) y = Poly.( >= ) x y
  let ascending (x : Stdlib.Int32.t) y = Poly.ascending x y
  let descending (x : Stdlib.Int32.t) y = Poly.descending x y
  let compare (x : Stdlib.Int32.t) y = Poly.compare x y
  let compare__local (x : Stdlib.Int32.t) y = Poly.compare x y
  let equal (x : Stdlib.Int32.t) y = Poly.equal x y
  let equal__local (x : Stdlib.Int32.t) y = Poly.equal x y
  let max (x : Stdlib.Int32.t) y = Bool0.select (x >= y) x y
  let min (x : Stdlib.Int32.t) y = Bool0.select (x <= y) x y
end

module Int64_replace_polymorphic_compare = struct
  (* Declared as externals so that the compiler skips the caml_apply_X wrapping even when
     compiling without cross library inlining. *)
  external ( = )
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> bool
    = "%equal"

  external ( <> )
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> bool
    = "%notequal"

  external ( < )
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> bool
    = "%lessthan"

  external ( > )
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> bool
    = "%greaterthan"

  external ( <= )
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> bool
    = "%lessequal"

  external ( >= )
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> bool
    = "%greaterequal"

  external compare
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> int
    = "%compare"

  external compare__local
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> int
    = "%compare"

  external equal
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> bool
    = "%equal"

  external equal__local
    :  (Stdlib.Int64.t[@local_opt])
    -> (Stdlib.Int64.t[@local_opt])
    -> bool
    = "%equal"

  let ascending (x : Stdlib.Int64.t) y = Poly.ascending x y
  let descending (x : Stdlib.Int64.t) y = Poly.descending x y
  let max (x : Stdlib.Int64.t) y = Bool0.select (x >= y) x y
  let min (x : Stdlib.Int64.t) y = Bool0.select (x <= y) x y
end

module Nativeint_replace_polymorphic_compare = struct
  let ( < ) (x : Stdlib.Nativeint.t) y = Poly.( < ) x y
  let ( <= ) (x : Stdlib.Nativeint.t) y = Poly.( <= ) x y
  let ( <> ) (x : Stdlib.Nativeint.t) y = Poly.( <> ) x y
  let ( = ) (x : Stdlib.Nativeint.t) y = Poly.( = ) x y
  let ( > ) (x : Stdlib.Nativeint.t) y = Poly.( > ) x y
  let ( >= ) (x : Stdlib.Nativeint.t) y = Poly.( >= ) x y
  let ascending (x : Stdlib.Nativeint.t) y = Poly.ascending x y
  let descending (x : Stdlib.Nativeint.t) y = Poly.descending x y
  let compare (x : Stdlib.Nativeint.t) y = Poly.compare x y
  let compare__local (x : Stdlib.Nativeint.t) y = Poly.compare x y
  let equal (x : Stdlib.Nativeint.t) y = Poly.equal x y
  let equal__local (x : Stdlib.Nativeint.t) y = Poly.equal x y
  let max (x : Stdlib.Nativeint.t) y = Bool0.select (x >= y) x y
  let min (x : Stdlib.Nativeint.t) y = Bool0.select (x <= y) x y
end

module Bool_replace_polymorphic_compare = struct
  let ( < ) (x : bool) y = Poly.( < ) x y
  let ( <= ) (x : bool) y = Poly.( <= ) x y
  let ( <> ) (x : bool) y = Poly.( <> ) x y
  let ( = ) (x : bool) y = Poly.( = ) x y
  let ( > ) (x : bool) y = Poly.( > ) x y
  let ( >= ) (x : bool) y = Poly.( >= ) x y
  let ascending (x : bool) y = Poly.ascending x y
  let descending (x : bool) y = Poly.descending x y
  let compare (x : bool) y = Poly.compare x y
  let compare__local (x : bool) y = Poly.compare x y
  let equal (x : bool) y = Poly.equal x y
  let equal__local (x : bool) y = Poly.equal x y
  let max (x : bool) y = Bool0.select (x >= y) x y
  let min (x : bool) y = Bool0.select (x <= y) x y
end

module Char_replace_polymorphic_compare = struct
  let ( < ) (x : char) y = Poly.( < ) x y
  let ( <= ) (x : char) y = Poly.( <= ) x y
  let ( <> ) (x : char) y = Poly.( <> ) x y
  let ( = ) (x : char) y = Poly.( = ) x y
  let ( > ) (x : char) y = Poly.( > ) x y
  let ( >= ) (x : char) y = Poly.( >= ) x y
  let ascending (x : char) y = Poly.ascending x y
  let descending (x : char) y = Poly.descending x y
  let compare (x : char) y = Poly.compare x y
  let compare__local (x : char) y = Poly.compare x y
  let equal (x : char) y = Poly.equal x y
  let equal__local (x : char) y = Poly.equal x y
  let max (x : char) y = Bool0.select (x >= y) x y
  let min (x : char) y = Bool0.select (x <= y) x y
end

module Uchar_replace_polymorphic_compare = struct
  open struct
    external i : (Stdlib.Uchar.t[@local_opt]) -> int = "%identity"
  end

  let ( < ) (x : Stdlib.Uchar.t) y = Int_replace_polymorphic_compare.( < ) (i x) (i y)
  let ( <= ) (x : Stdlib.Uchar.t) y = Int_replace_polymorphic_compare.( <= ) (i x) (i y)
  let ( <> ) (x : Stdlib.Uchar.t) y = Int_replace_polymorphic_compare.( <> ) (i x) (i y)
  let ( = ) (x : Stdlib.Uchar.t) y = Int_replace_polymorphic_compare.( = ) (i x) (i y)
  let ( > ) (x : Stdlib.Uchar.t) y = Int_replace_polymorphic_compare.( > ) (i x) (i y)
  let ( >= ) (x : Stdlib.Uchar.t) y = Int_replace_polymorphic_compare.( >= ) (i x) (i y)

  let ascending (x : Stdlib.Uchar.t) y =
    Int_replace_polymorphic_compare.ascending (i x) (i y)
  ;;

  let descending (x : Stdlib.Uchar.t) y =
    Int_replace_polymorphic_compare.descending (i x) (i y)
  ;;

  let compare (x : Stdlib.Uchar.t) y = Int_replace_polymorphic_compare.compare (i x) (i y)
  let equal (x : Stdlib.Uchar.t) y = Int_replace_polymorphic_compare.equal (i x) (i y)

  let compare__local (x : Stdlib.Uchar.t) y =
    Int_replace_polymorphic_compare.compare__local (i x) (i y)
  ;;

  let equal__local (x : Stdlib.Uchar.t) y =
    Int_replace_polymorphic_compare.equal__local (i x) (i y)
  ;;

  let max (x : Stdlib.Uchar.t) y = Bool0.select (x >= y) x y
  let min (x : Stdlib.Uchar.t) y = Bool0.select (x <= y) x y
end

module Float_replace_polymorphic_compare = struct
  external ( < ) : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%lessthan"
  external ( <= ) : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%lessequal"
  external ( <> ) : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%notequal"
  external ( = ) : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%equal"
  external ( > ) : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%greaterthan"
  external ( >= ) : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%greaterequal"
  external equal : (float[@local_opt]) -> (float[@local_opt]) -> bool = "%equal"
  external compare : (float[@local_opt]) -> (float[@local_opt]) -> int = "%compare"

  let ascending (x : float) y = Poly.ascending x y
  let descending (x : float) y = Poly.descending x y
  let compare__local (x : float) y = Poly.compare x y
  let equal__local (x : float) y = Poly.equal x y
  let max (x : float) y = Bool0.select (x >= y) x y
  let min (x : float) y = Bool0.select (x <= y) x y
end

module String_replace_polymorphic_compare = struct
  let ( < ) (x : string) y = Poly.( < ) x y
  let ( <= ) (x : string) y = Poly.( <= ) x y
  let ( <> ) (x : string) y = Poly.( <> ) x y
  let ( = ) (x : string) y = Poly.( = ) x y
  let ( > ) (x : string) y = Poly.( > ) x y
  let ( >= ) (x : string) y = Poly.( >= ) x y
  let ascending (x : string) y = Poly.ascending x y
  let descending (x : string) y = Poly.descending x y
  let compare (x : string) y = Poly.compare x y
  let compare__local (x : string) y = Poly.compare x y
  let equal (x : string) y = Poly.equal x y
  let equal__local (x : string) y = Poly.equal x y
  let max (x : string) y = Bool0.select (x >= y) x y
  let min (x : string) y = Bool0.select (x <= y) x y
end

module Bytes_replace_polymorphic_compare = struct
  let ( < ) (x : bytes) y = Poly.( < ) x y
  let ( <= ) (x : bytes) y = Poly.( <= ) x y
  let ( <> ) (x : bytes) y = Poly.( <> ) x y
  let ( = ) (x : bytes) y = Poly.( = ) x y
  let ( > ) (x : bytes) y = Poly.( > ) x y
  let ( >= ) (x : bytes) y = Poly.( >= ) x y
  let ascending (x : bytes) y = Poly.ascending x y
  let descending (x : bytes) y = Poly.descending x y
  let compare (x : bytes) y = Poly.compare x y
  let compare__local (x : bytes) y = Poly.compare x y
  let equal (x : bytes) y = Poly.equal x y
  let equal__local (x : bytes) y = Poly.equal x y
  let max (x : bytes) y = Bool0.select (x >= y) x y
  let min (x : bytes) y = Bool0.select (x <= y) x y
end

(* This needs to be defined as an external so that the compiler can specialize it as a
   direct set or caml_modify. *)
external ( := ) : ('a ref[@local_opt]) -> 'a -> unit = "%setfield0"

(* These need to be defined as an external otherwise the compiler won't unbox
   references. *)
external ( ! ) : ('a ref[@local_opt]) -> 'a = "%field0"
external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"

let ( @ ) = Stdlib.( @ )
let ( ^ ) = Stdlib.( ^ )
let ( ~- ) = Stdlib.( ~- )
let ( ~-. ) = Stdlib.( ~-. )
let ( asr ) = Stdlib.( asr )
let ( land ) = Stdlib.( land )
let lnot = Stdlib.lnot
let ( lor ) = Stdlib.( lor )
let ( lsl ) = Stdlib.( lsl )
let ( lsr ) = Stdlib.( lsr )
let ( lxor ) = Stdlib.( lxor )
let ( mod ) = Stdlib.( mod )
let abs = Stdlib.abs
let failwith = Stdlib.failwith
let fst = Stdlib.fst
let invalid_arg = Stdlib.invalid_arg
let snd = Stdlib.snd

(* [raise] needs to be defined as an external as the compiler automatically replaces
   '%raise' by '%reraise' when appropriate. *)
external raise : exn -> _ = "%raise"
external phys_equal : ('a[@local_opt]) -> ('a[@local_opt]) -> bool = "%eq"
external decr : (int ref[@local_opt]) -> unit = "%decr"
external incr : (int ref[@local_opt]) -> unit = "%incr"

(* Used by sexp_conv, which float0 depends on through option. *)
let float_of_string = Stdlib.float_of_string

(* [am_testing] is used in a few places to behave differently when in testing mode, such
   as in [random.ml].  [am_testing] is implemented using [Base_am_testing], a weak C/js
   primitive that returns [false], but when linking an inline-test-runner executable, is
   overridden by another primitive that returns [true]. *)
external am_testing : unit -> bool = "Base_am_testing"

let am_testing = am_testing ()
