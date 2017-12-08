(** This module is the toplevel of the Base library, it is what you get when you do [open
    Base].

    The recommended way to use Base is to build with [-open Base].  Files compiled this
    way will have the environment described in this file as initial environment. *)

(*_ We hide this from the web docs because the line wrapping is bad, making it
  pretty much inscrutable. *)
(**/**)

(** The intent is to shadow all of INRIA's standard library.  Modules below would cause
    compilation errors without being removed from [Shadow_stdlib] before inclusion. *)
include (Shadow_stdlib
         : module type of struct include Shadow_stdlib end
         (* Modules defined in Base *)
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
         with module Random    := Caml.Random
         with module Set       := Caml.Set
         with module String    := Caml.String
         with module Sys       := Caml.Sys
         with module Uchar     := Caml.Uchar

         (* Support for generated lexers *)
         with module Lexing    := Caml.Lexing

         with type ('a, 'b, 'c) format              := ('a, 'b, 'c) format
         with type ('a, 'b, 'c, 'd) format4         := ('a, 'b, 'c, 'd) format4
         with type ('a, 'b, 'c, 'd, 'e, 'f) format6 := ('a, 'b, 'c, 'd, 'e, 'f) format6

         with type 'a ref := 'a ref
        )

(**/**)

open! Import

module Applicative               = Applicative
module Applicative_intf          = Applicative_intf
module Array                     = Array
module Avltree                   = Avltree
module Backtrace                 = Backtrace
module Binary_search             = Binary_search
module Binary_searchable         = Binary_searchable
module Binary_searchable_intf    = Binary_searchable_intf
module Blit                      = Blit
module Blit_intf                 = Blit_intf
module Bool                      = Bool
module Buffer                    = Buffer
module Bytes                     = Bytes
module Char                      = Char
module Commutative_group         = Commutative_group
module Comparable                = Comparable
module Comparable_intf           = Comparable_intf
module Comparator                = Comparator
module Comparisons               = Comparisons
module Container                 = Container
module Container_intf            = Container_intf
module Either                    = Either
module Either_intf               = Either_intf
module Equal                     = Equal
module Error                     = Error
module Exn                       = Exn
module Field                     = Field
module Float                     = Float
module Floatable                 = Floatable
module Fn                        = Fn
module Hash                      = Hash
module Hash_intf                 = Hash_intf
module Hash_set                  = Hash_set
module Hash_set_intf             = Hash_set_intf
module Hasher                    = Hasher
module Hashtbl                   = Hashtbl
module Hashtbl_intf              = Hashtbl_intf
module Heap_block                = Heap_block
module Identifiable              = Identifiable
module Indexed_container         = Indexed_container
module Info                      = Info
module Info_intf                 = Info_intf
module Int                       = Int
module Int32                     = Int32
module Int63                     = Int63
module Int64                     = Int64
module Int_intf                  = Int_intf
module Intable                   = Intable
module Invariant                 = Invariant
module Lazy                      = Lazy
module List                      = List
module Map                       = Map
module Map_intf                  = Map_intf
module Maybe_bound               = Maybe_bound
module Monad                     = Monad
module Nativeint                 = Nativeint
module Obj_array                 = Obj_array
module Option                    = Option
module Or_error                  = Or_error
module Ordered_collection_common = Ordered_collection_common
module Ordering                  = Ordering
module Poly                      = Poly
module Polymorphic_compare       = Polymorphic_compare
module Popcount                  = Popcount
module Pretty_printer            = Pretty_printer
module Printf                    = Printf
module Linked_queue              = Linked_queue
module Queue_intf                = Queue_intf
module Random                    = Random
module Ref                       = Ref
module Result                    = Result
module Sequence                  = Sequence
module Set                       = Set
module Set_intf                  = Set_intf
module Sexpable                  = Sexpable
module Sign                      = Sign
module Source_code_position      = Source_code_position
module Staged                    = Staged
module String                    = String
module Stringable                = Stringable
module String_dict               = String_dict
module Sys                       = Sys
module T                         = T
module Type_equal                = Type_equal
module Unit                      = Unit
module Uchar                     = Uchar
module Validate                  = Validate
module Variant                   = Variant
module With_return               = With_return
module Word_size                 = Word_size

(* Avoid a level of indirection for uses of the signatures defined in [T]. *)
include T (** @inline *)

(* This is a hack so that odoc creates better documentation. *)
module Sexp = struct
  include Sexp_with_comparable (** @inline *)
end


(**/**)
module Exported_for_specific_uses = struct
  module Fieldslib       = Fieldslib
  module Ppx_hash_lib    = Ppx_hash_lib
  module Sexplib         = Sexplib
  module Variantslib     = Variantslib
  module Ppx_compare_lib = Ppx_compare_lib
  let am_testing = am_testing
end
(**/**)

module Export = struct
  (* [deriving hash] is missing for [array] and [ref] since these types are mutable.
     (string is also mutable, but we pretend it isn't for hashing purposes) *)
  type 'a array  = 'a Array.  t [@@deriving_inline compare,       sexp]
  let compare_array : 'a . ('a -> 'a -> int) -> 'a array -> 'a array -> int =
    Array.compare
  let array_of_sexp : 'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a array
    = Array.t_of_sexp
  let sexp_of_array : 'a . ('a -> Sexplib.Sexp.t) -> 'a array -> Sexplib.Sexp.t
    = Array.sexp_of_t
  [@@@end]
  type bool      = Bool.      t [@@deriving_inline compare, hash, sexp]
  let compare_bool : bool -> bool -> int = Bool.compare
  let (hash_fold_bool :
         Ppx_hash_lib.Std.Hash.state -> bool -> Ppx_hash_lib.Std.Hash.state) =
    Bool.hash_fold_t

  and (hash_bool : bool -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Bool.hash  in fun x  -> func x

  let bool_of_sexp : Sexplib.Sexp.t -> bool = Bool.t_of_sexp
  let sexp_of_bool : bool -> Sexplib.Sexp.t = Bool.sexp_of_t
  [@@@end]
  type char      = Char.      t [@@deriving_inline compare, hash, sexp]
  let compare_char : char -> char -> int = Char.compare
  let (hash_fold_char :
         Ppx_hash_lib.Std.Hash.state -> char -> Ppx_hash_lib.Std.Hash.state) =
    Char.hash_fold_t

  and (hash_char : char -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Char.hash  in fun x  -> func x

  let char_of_sexp : Sexplib.Sexp.t -> char = Char.t_of_sexp
  let sexp_of_char : char -> Sexplib.Sexp.t = Char.sexp_of_t
  [@@@end]
  type exn       = Exn.       t [@@deriving_inline                sexp_of]
  let sexp_of_exn : exn -> Sexplib.Sexp.t = Exn.sexp_of_t
  [@@@end]
  type float     = Float.     t [@@deriving_inline compare, hash, sexp]
  let compare_float : float -> float -> int = Float.compare
  let (hash_fold_float :
         Ppx_hash_lib.Std.Hash.state -> float -> Ppx_hash_lib.Std.Hash.state) =
    Float.hash_fold_t

  and (hash_float : float -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Float.hash  in fun x  -> func x

  let float_of_sexp : Sexplib.Sexp.t -> float = Float.t_of_sexp
  let sexp_of_float : float -> Sexplib.Sexp.t = Float.sexp_of_t
  [@@@end]
  type int       = Int.       t [@@deriving_inline compare, hash, sexp]
  let compare_int : int -> int -> int = Int.compare
  let (hash_fold_int :
         Ppx_hash_lib.Std.Hash.state -> int -> Ppx_hash_lib.Std.Hash.state) =
    Int.hash_fold_t

  and (hash_int : int -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Int.hash  in fun x  -> func x

  let int_of_sexp : Sexplib.Sexp.t -> int = Int.t_of_sexp
  let sexp_of_int : int -> Sexplib.Sexp.t = Int.sexp_of_t
  [@@@end]
  type int32     = Int32.     t [@@deriving_inline compare, hash, sexp]
  let compare_int32 : int32 -> int32 -> int = Int32.compare
  let (hash_fold_int32 :
         Ppx_hash_lib.Std.Hash.state -> int32 -> Ppx_hash_lib.Std.Hash.state) =
    Int32.hash_fold_t

  and (hash_int32 : int32 -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Int32.hash  in fun x  -> func x

  let int32_of_sexp : Sexplib.Sexp.t -> int32 = Int32.t_of_sexp
  let sexp_of_int32 : int32 -> Sexplib.Sexp.t = Int32.sexp_of_t
  [@@@end]
  type int64     = Int64.     t [@@deriving_inline compare, hash, sexp]
  let compare_int64 : int64 -> int64 -> int = Int64.compare
  let (hash_fold_int64 :
         Ppx_hash_lib.Std.Hash.state -> int64 -> Ppx_hash_lib.Std.Hash.state) =
    Int64.hash_fold_t

  and (hash_int64 : int64 -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Int64.hash  in fun x  -> func x

  let int64_of_sexp : Sexplib.Sexp.t -> int64 = Int64.t_of_sexp
  let sexp_of_int64 : int64 -> Sexplib.Sexp.t = Int64.sexp_of_t
  [@@@end]
  type 'a list   = 'a List.   t [@@deriving_inline compare, hash, sexp]
  let compare_list : 'a . ('a -> 'a -> int) -> 'a list -> 'a list -> int =
    List.compare
  let hash_fold_list :
    'a .
      (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> 'a list -> Ppx_hash_lib.Std.Hash.state
    = List.hash_fold_t
  let list_of_sexp : 'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a list =
    List.t_of_sexp
  let sexp_of_list : 'a . ('a -> Sexplib.Sexp.t) -> 'a list -> Sexplib.Sexp.t =
    List.sexp_of_t
  [@@@end]
  type nativeint = Nativeint. t [@@deriving_inline compare, hash, sexp]
  let compare_nativeint : nativeint -> nativeint -> int = Nativeint.compare
  let (hash_fold_nativeint :
         Ppx_hash_lib.Std.Hash.state -> nativeint -> Ppx_hash_lib.Std.Hash.state) =
    Nativeint.hash_fold_t

  and (hash_nativeint : nativeint -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Nativeint.hash  in fun x  -> func x

  let nativeint_of_sexp : Sexplib.Sexp.t -> nativeint = Nativeint.t_of_sexp
  let sexp_of_nativeint : nativeint -> Sexplib.Sexp.t = Nativeint.sexp_of_t
  [@@@end]
  type 'a option = 'a Option. t [@@deriving_inline compare, hash, sexp]
  let compare_option : 'a . ('a -> 'a -> int) -> 'a option -> 'a option -> int
    = Option.compare
  let hash_fold_option :
    'a .
      (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> 'a option -> Ppx_hash_lib.Std.Hash.state
    = Option.hash_fold_t
  let option_of_sexp :
    'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a option =
    Option.t_of_sexp
  let sexp_of_option :
    'a . ('a -> Sexplib.Sexp.t) -> 'a option -> Sexplib.Sexp.t =
    Option.sexp_of_t
  [@@@end]
  type 'a ref    = 'a Ref.    t [@@deriving_inline compare,       sexp]
  let compare_ref : 'a . ('a -> 'a -> int) -> 'a ref -> 'a ref -> int =
    Ref.compare
  let ref_of_sexp : 'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a ref =
    Ref.t_of_sexp
  let sexp_of_ref : 'a . ('a -> Sexplib.Sexp.t) -> 'a ref -> Sexplib.Sexp.t =
    Ref.sexp_of_t
  [@@@end]
  type string    = String.    t [@@deriving_inline compare, hash, sexp]
  let compare_string : string -> string -> int = String.compare
  let (hash_fold_string :
         Ppx_hash_lib.Std.Hash.state -> string -> Ppx_hash_lib.Std.Hash.state) =
    String.hash_fold_t

  and (hash_string : string -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = String.hash  in fun x  -> func x

  let string_of_sexp : Sexplib.Sexp.t -> string = String.t_of_sexp
  let sexp_of_string : string -> Sexplib.Sexp.t = String.sexp_of_t
  [@@@end]
  type bytes     = Bytes.     t [@@deriving_inline compare,       sexp]
  let compare_bytes : bytes -> bytes -> int = Bytes.compare
  let bytes_of_sexp : Sexplib.Sexp.t -> bytes = Bytes.t_of_sexp
  let sexp_of_bytes : bytes -> Sexplib.Sexp.t = Bytes.sexp_of_t
  [@@@end]
  type unit      = Unit.      t [@@deriving_inline compare, hash, sexp]
  let compare_unit : unit -> unit -> int = Unit.compare
  let (hash_fold_unit :
         Ppx_hash_lib.Std.Hash.state -> unit -> Ppx_hash_lib.Std.Hash.state) =
    Unit.hash_fold_t

  and (hash_unit : unit -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Unit.hash  in fun x  -> func x

  let unit_of_sexp : Sexplib.Sexp.t -> unit = Unit.t_of_sexp
  let sexp_of_unit : unit -> Sexplib.Sexp.t = Unit.sexp_of_t
  [@@@end]

  (** Format stuff *)
  type nonrec ('a, 'b, 'c) format = ('a, 'b, 'c) format
  type nonrec ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) format4
  type nonrec ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) format6

  (** {2 Sexp}

      Exporting the ad-hoc types that are recognized by [ppx_sexp_*] converters.
      [sexp_array], [sexp_list], and [sexp_option] allow a record field to be absent when
      converting from a sexp, and if absent, the field will take a default value of the
      appropriate type:

      {v
        sexp_array   [||]
        sexp_bool    false
        sexp_list    []
        sexp_option  None
      v}

      [sexp_opaque] causes the conversion to sexp to produce the atom [<opaque>].

      For more documentation, see sexplib/README.md. *)

  type 'a sexp_array  = 'a array
  type 'a sexp_list   = 'a list
  type 'a sexp_opaque = 'a
  type 'a sexp_option = 'a option

  (** List operators *)
  include List.Infix

  (** Int operators and comparisons *)
  include Int.O
  include Int_replace_polymorphic_compare

  (** Float operators *)
  include Float.O_dot

  (** Reverse application operator. [x |> g |> f] is equivalent to [f (g (x))]. *)
  (* This is declared as an external to be optimized away in more contexts. *)
  external ( |> ) : 'a -> ( 'a -> 'b) -> 'b = "%revapply"

  (** Application operator. [g @@ f @@ x] is equivalent to [g (f (x))]. *)
  external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

  (** Boolean operations *)
  (* These need to be declared as an external to get the lazy behavior *)
  external ( && ) : bool -> bool -> bool = "%sequand"
  external ( || ) : bool -> bool -> bool = "%sequor"
  external not : bool -> bool = "%boolnot"

  (* This must be declared as an external for the warnings to work properly. *)
  external ignore : _ -> unit = "%ignore"

  (** Common string operations *)
  let ( ^ ) = String.( ^ )

  (** Reference operations *)
  (* Declared as an externals so that the compiler skips the caml_modify when possible and
     to keep reference unboxing working *)
  external ( ! ) : 'a ref -> 'a = "%field0"
  external ref : 'a -> 'a ref = "%makemutable"
  external ( := ) : 'a ref -> 'a -> unit = "%setfield0"

  (** Pair operations *)
  let fst = fst
  let snd = snd

  (** Exceptions stuff *)
  let failwith    = failwith
  let invalid_arg = invalid_arg
  let raise       = raise
  let raise_s     = Error.raise_s

  (** Misc *)
  let phys_equal = phys_equal

  external force : 'a Lazy.t -> 'a = "%lazy_force"
end

include Export
include Container_intf.Export (** @inline *)

(* Various things to cleanup that were used without going through Base. *)
module Not_exposed_properly = struct
  module Int63_emul          = Int63_emul
  module Float0              = Float0
  module Import              = Import
  module Int_conversions     = Int_conversions
  module Int_math            = Int_math
  module Pow_overflow_bounds = Pow_overflow_bounds
  module Sexp_conv           = Sexplib0.Sexp_conv
end

(* We perform these side effects here because we want them to run for any code that uses
   [Base].  If this were in another module in [Base] that was not used in some program,
   then the side effects might not be run in that program.  This will run as long as the
   program refers to at least one value directly in [Base]; referring to values in
   [Base.Bool], for example, is not sufficient. *)
let () =
  Backtrace.initialize_module ();
;;
