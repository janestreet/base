(* This module is the toplevel of the Base library, it is what you get when you do [open
   Base].

   The recommended way to use Base is to build with [-open Base].  Files compiled this way
   will have the environment described in this file as initial environment. *)

include (Shadow_stdlib
         : module type of struct include Shadow_stdlib end
         (* Modules defined in Base *)
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
         with module Sys       := Caml.Sys
         with module Uchar     := Caml.Uchar

         (* Support for generated lexers *)
         with module Lexing    := Caml.Lexing

         with type ('a, 'b, 'c) format              := ('a, 'b, 'c) format
         with type ('a, 'b, 'c, 'd) format4         := ('a, 'b, 'c, 'd) format4
         with type ('a, 'b, 'c, 'd, 'e, 'f) format6 := ('a, 'b, 'c, 'd, 'e, 'f) format6

         with type 'a ref := 'a ref
        )

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
module Char                      = Char
module Commutative_group         = Commutative_group
module Comparable                = Comparable
module Comparable_intf           = Comparable_intf
module Comparator                = Comparator
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
module Option                    = Option
module Or_error                  = Or_error
module Ordered_collection_common = Ordered_collection_common
module Ordering                  = Ordering
module Poly                      = Poly
module Polymorphic_compare       = Polymorphic_compare
module Polymorphic_compare_intf  = Polymorphic_compare_intf
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
  let array_of_sexp : 'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a array
    =
    let _tp_loc = "src/base.ml.Export.array"  in
    fun _of_a  -> fun t  -> (Array.t_of_sexp _of_a) t
  let sexp_of_array : 'a . ('a -> Sexplib.Sexp.t) -> 'a array -> Sexplib.Sexp.t
    = fun _of_a  -> fun v  -> (Array.sexp_of_t _of_a) v
  let compare_array : 'a . ('a -> 'a -> int) -> 'a array -> 'a array -> int =
    fun _cmp__a  ->
    fun a__001_  -> fun b__002_  -> Array.compare _cmp__a a__001_ b__002_

  [@@@end]
  type bool      = Bool.      t [@@deriving_inline compare, hash, sexp]
  let bool_of_sexp : Sexplib.Sexp.t -> bool =
    let _tp_loc = "src/base.ml.Export.bool"  in fun t  -> Bool.t_of_sexp t
  let sexp_of_bool : bool -> Sexplib.Sexp.t = fun v  -> Bool.sexp_of_t v
  let (hash_fold_bool :
         Ppx_hash_lib.Std.Hash.state -> bool -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> Bool.hash_fold_t hsv arg
  let (hash_bool : bool -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_bool (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_bool : bool -> bool -> int =
    fun a__005_  -> fun b__006_  -> Bool.compare a__005_ b__006_
  [@@@end]
  type char      = Char.      t [@@deriving_inline compare, hash, sexp]
  let char_of_sexp : Sexplib.Sexp.t -> char =
    let _tp_loc = "src/base.ml.Export.char"  in fun t  -> Char.t_of_sexp t
  let sexp_of_char : char -> Sexplib.Sexp.t = fun v  -> Char.sexp_of_t v
  let (hash_fold_char :
         Ppx_hash_lib.Std.Hash.state -> char -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> Char.hash_fold_t hsv arg
  let (hash_char : char -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_char (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_char : char -> char -> int =
    fun a__007_  -> fun b__008_  -> Char.compare a__007_ b__008_
  [@@@end]
  type exn       = Exn.       t [@@deriving_inline                sexp_of]
  let sexp_of_exn : exn -> Sexplib.Sexp.t = fun v  -> Exn.sexp_of_t v
  [@@@end]
  type float     = Float.     t [@@deriving_inline compare, hash, sexp]
  let float_of_sexp : Sexplib.Sexp.t -> float =
    let _tp_loc = "src/base.ml.Export.float"  in fun t  -> Float.t_of_sexp t
  let sexp_of_float : float -> Sexplib.Sexp.t = fun v  -> Float.sexp_of_t v
  let (hash_fold_float :
         Ppx_hash_lib.Std.Hash.state -> float -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> Float.hash_fold_t hsv arg
  let (hash_float : float -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_float (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_float : float -> float -> int =
    fun a__009_  -> fun b__010_  -> Float.compare a__009_ b__010_
  [@@@end]
  type int       = Int.       t [@@deriving_inline compare, hash, sexp]
  let int_of_sexp : Sexplib.Sexp.t -> int =
    let _tp_loc = "src/base.ml.Export.int"  in fun t  -> Int.t_of_sexp t
  let sexp_of_int : int -> Sexplib.Sexp.t = fun v  -> Int.sexp_of_t v
  let (hash_fold_int :
         Ppx_hash_lib.Std.Hash.state -> int -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> Int.hash_fold_t hsv arg
  let (hash_int : int -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_int (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_int : int -> int -> int =
    fun a__011_  -> fun b__012_  -> Int.compare a__011_ b__012_
  [@@@end]
  type int32     = Int32.     t [@@deriving_inline compare, hash, sexp]
  let int32_of_sexp : Sexplib.Sexp.t -> int32 =
    let _tp_loc = "src/base.ml.Export.int32"  in fun t  -> Int32.t_of_sexp t
  let sexp_of_int32 : int32 -> Sexplib.Sexp.t = fun v  -> Int32.sexp_of_t v
  let (hash_fold_int32 :
         Ppx_hash_lib.Std.Hash.state -> int32 -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> Int32.hash_fold_t hsv arg
  let (hash_int32 : int32 -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_int32 (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_int32 : int32 -> int32 -> int =
    fun a__013_  -> fun b__014_  -> Int32.compare a__013_ b__014_
  [@@@end]
  type int64     = Int64.     t [@@deriving_inline compare, hash, sexp]
  let int64_of_sexp : Sexplib.Sexp.t -> int64 =
    let _tp_loc = "src/base.ml.Export.int64"  in fun t  -> Int64.t_of_sexp t
  let sexp_of_int64 : int64 -> Sexplib.Sexp.t = fun v  -> Int64.sexp_of_t v
  let (hash_fold_int64 :
         Ppx_hash_lib.Std.Hash.state -> int64 -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> Int64.hash_fold_t hsv arg
  let (hash_int64 : int64 -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_int64 (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_int64 : int64 -> int64 -> int =
    fun a__015_  -> fun b__016_  -> Int64.compare a__015_ b__016_
  [@@@end]
  type 'a list   = 'a List.   t [@@deriving_inline compare, hash, sexp]
  let list_of_sexp : 'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a list =
    let _tp_loc = "src/base.ml.Export.list"  in
    fun _of_a  -> fun t  -> (List.t_of_sexp _of_a) t
  let sexp_of_list : 'a . ('a -> Sexplib.Sexp.t) -> 'a list -> Sexplib.Sexp.t =
    fun _of_a  -> fun v  -> (List.sexp_of_t _of_a) v
  let hash_fold_list :
    'a .
      (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> 'a list -> Ppx_hash_lib.Std.Hash.state
    =
    fun _hash_fold_a  ->
    fun hsv  ->
    fun arg  ->
      List.hash_fold_t (fun hsv  -> fun arg  -> _hash_fold_a hsv arg) hsv
        arg

  let compare_list : 'a . ('a -> 'a -> int) -> 'a list -> 'a list -> int =
    fun _cmp__a  ->
    fun a__017_  -> fun b__018_  -> List.compare _cmp__a a__017_ b__018_

  [@@@end]
  type nativeint = Nativeint. t [@@deriving_inline compare, hash, sexp]
  let nativeint_of_sexp : Sexplib.Sexp.t -> nativeint =
    let _tp_loc = "src/base.ml.Export.nativeint"  in
    fun t  -> Nativeint.t_of_sexp t
  let sexp_of_nativeint : nativeint -> Sexplib.Sexp.t =
    fun v  -> Nativeint.sexp_of_t v
  let (hash_fold_nativeint :
         Ppx_hash_lib.Std.Hash.state -> nativeint -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> Nativeint.hash_fold_t hsv arg
  let (hash_nativeint : nativeint -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_nativeint (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_nativeint : nativeint -> nativeint -> int =
    fun a__021_  -> fun b__022_  -> Nativeint.compare a__021_ b__022_
  [@@@end]
  type 'a option = 'a Option. t [@@deriving_inline compare, hash, sexp]
  let option_of_sexp :
    'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a option =
    let _tp_loc = "src/base.ml.Export.option"  in
    fun _of_a  -> fun t  -> (Option.t_of_sexp _of_a) t
  let sexp_of_option :
    'a . ('a -> Sexplib.Sexp.t) -> 'a option -> Sexplib.Sexp.t =
    fun _of_a  -> fun v  -> (Option.sexp_of_t _of_a) v
  let hash_fold_option :
    'a .
      (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state) ->
    Ppx_hash_lib.Std.Hash.state -> 'a option -> Ppx_hash_lib.Std.Hash.state
    =
    fun _hash_fold_a  ->
    fun hsv  ->
    fun arg  ->
      Option.hash_fold_t (fun hsv  -> fun arg  -> _hash_fold_a hsv arg) hsv
        arg

  let compare_option : 'a . ('a -> 'a -> int) -> 'a option -> 'a option -> int
    =
    fun _cmp__a  ->
    fun a__023_  -> fun b__024_  -> Option.compare _cmp__a a__023_ b__024_

  [@@@end]
  type 'a ref    = 'a Ref.    t [@@deriving_inline compare,       sexp]
  let ref_of_sexp : 'a . (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a ref =
    let _tp_loc = "src/base.ml.Export.ref"  in
    fun _of_a  -> fun t  -> (Ref.t_of_sexp _of_a) t
  let sexp_of_ref : 'a . ('a -> Sexplib.Sexp.t) -> 'a ref -> Sexplib.Sexp.t =
    fun _of_a  -> fun v  -> (Ref.sexp_of_t _of_a) v
  let compare_ref : 'a . ('a -> 'a -> int) -> 'a ref -> 'a ref -> int =
    fun _cmp__a  ->
    fun a__027_  -> fun b__028_  -> Ref.compare _cmp__a a__027_ b__028_

  [@@@end]
  type string    = String.    t [@@deriving_inline compare, hash, sexp]
  let string_of_sexp : Sexplib.Sexp.t -> string =
    let _tp_loc = "src/base.ml.Export.string"  in fun t  -> String.t_of_sexp t
  let sexp_of_string : string -> Sexplib.Sexp.t = fun v  -> String.sexp_of_t v
  let (hash_fold_string :
         Ppx_hash_lib.Std.Hash.state -> string -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> String.hash_fold_t hsv arg
  let (hash_string : string -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_string (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_string : string -> string -> int =
    fun a__031_  -> fun b__032_  -> String.compare a__031_ b__032_
  [@@@end]
  type unit      = Unit.      t [@@deriving_inline compare, hash, sexp]
  let unit_of_sexp : Sexplib.Sexp.t -> unit =
    let _tp_loc = "src/base.ml.Export.unit"  in fun t  -> Unit.t_of_sexp t
  let sexp_of_unit : unit -> Sexplib.Sexp.t = fun v  -> Unit.sexp_of_t v
  let (hash_fold_unit :
         Ppx_hash_lib.Std.Hash.state -> unit -> Ppx_hash_lib.Std.Hash.state) =
    fun hsv  -> fun arg  -> Unit.hash_fold_t hsv arg
  let (hash_unit : unit -> Ppx_hash_lib.Std.Hash.hash_value) =
    fun arg  ->
      Ppx_hash_lib.Std.Hash.get_hash_value
        (hash_fold_unit (Ppx_hash_lib.Std.Hash.create ()) arg)

  let compare_unit : unit -> unit -> int =
    fun a__033_  -> fun b__034_  -> Unit.compare a__033_ b__034_
  [@@@end]

  (** Format stuff *)
  type nonrec ('a, 'b, 'c) format = ('a, 'b, 'c) format
  type nonrec ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) format4
  type nonrec ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) format6

  (** List operators *)
  include List.Infix

  (** Int operators and comparisons *)
  include Int.O
  include Int_replace_polymorphic_compare

  (** Composition operator *)
  (* This need to be declared as an external to be optimized away in more contexts *)
  external ( |> ) : 'a -> ( 'a -> 'b) -> 'b = "%revapply"

  (** Boolean operations *)
  (* These need to be declared as an external to get the lazy behavior *)
  external ( && ) : bool -> bool -> bool = "%sequand"
  external ( || ) : bool -> bool -> bool = "%sequor"
  external not : bool -> bool = "%boolnot"

  (* This need to be declared as an external for the warnings to work properly *)
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


end

include Export

(* Various things to cleanup that were used without going through Base. *)
module Not_exposed_properly = struct
  module Int63_emul          = Int63_emul
  module Float0              = Float0
  module Import              = Import
  module Int_conversions     = Int_conversions
  module Int_math            = Int_math
  module Pow_overflow_bounds = Pow_overflow_bounds
  module Sexp_conv           = Sexp_conv
end

(* We perform these side effects here because we want them to run for any code that uses
   [Base].  If this were in another module in [Base] that was not used in some program,
   then the side effects might not be run in that program.  This will run as long as the
   program refers to at least one value directly in [Base]; referring to values in
   [Base.Bool], for example, is not sufficient. *)
let () =
  Backtrace.initialize_module ();
;;
