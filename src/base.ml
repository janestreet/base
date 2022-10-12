(** This module is the toplevel of the Base library; it's what you get when you write
    [open Base].

    The goal of Base is both to be a more complete standard library, with richer APIs,
    and to be more consistent in its design. For instance, in the standard library
    some things have modules and others don't; in Base, everything is a module.

    Base extends some modules and data structures from the standard library, like [Array],
    [Buffer], [Bytes], [Char], [Hashtbl], [Int32], [Int64], [Lazy], [List], [Map],
    [Nativeint], [Printf], [Random], [Set], [String], [Sys], and [Uchar]. One key
    difference is that Base doesn't use exceptions as much as the standard library and
    instead makes heavy use of the [Result] type, as in:

    {[ type ('a,'b) result = Ok of 'a | Error of 'b ]}

    Base also adds entirely new modules, most notably:

    - [Comparable], [Comparator], and [Comparisons] in lieu of polymorphic compare.
    - [Container], which provides a consistent interface across container-like data
      structures (arrays, lists, strings).
    - [Result], [Error], and [Or_error], supporting the or-error pattern.
*)

(*_ We hide this from the web docs because the line wrapping is bad, making it
  pretty much inscrutable. *)
(**/**)

(* The intent is to shadow all of INRIA's standard library.  Modules below would cause
   compilation errors without being removed from [Shadow_stdlib] before inclusion. *)

include (
  Shadow_stdlib :
    module type of struct
    include Shadow_stdlib
  end
  (* Modules defined in Base *)
  with module Array := Shadow_stdlib.Array
  with module Atomic := Shadow_stdlib.Atomic
  with module Bool := Shadow_stdlib.Bool
  with module Buffer := Shadow_stdlib.Buffer
  with module Bytes := Shadow_stdlib.Bytes
  with module Char := Shadow_stdlib.Char
  with module Either := Shadow_stdlib.Either
  with module Float := Shadow_stdlib.Float
  with module Hashtbl := Shadow_stdlib.Hashtbl
  with module In_channel := Shadow_stdlib.In_channel
  with module Int := Shadow_stdlib.Int
  with module Int32 := Shadow_stdlib.Int32
  with module Int64 := Shadow_stdlib.Int64
  with module Lazy := Shadow_stdlib.Lazy
  with module List := Shadow_stdlib.List
  with module Map := Shadow_stdlib.Map
  with module Nativeint := Shadow_stdlib.Nativeint
  with module Option := Shadow_stdlib.Option
  with module Out_channel := Shadow_stdlib.Out_channel
  with module Printf := Shadow_stdlib.Printf
  with module Queue := Shadow_stdlib.Queue
  with module Random := Shadow_stdlib.Random
  with module Result := Shadow_stdlib.Result
  with module Set := Shadow_stdlib.Set
  with module Stack := Shadow_stdlib.Stack
  with module String := Shadow_stdlib.String
  with module Sys := Shadow_stdlib.Sys
  with module Uchar := Shadow_stdlib.Uchar
  with module Unit := Shadow_stdlib.Unit
  (* Support for generated lexers *)
  with module Lexing := Shadow_stdlib.Lexing
  with type ('a, 'b, 'c) format := ('a, 'b, 'c) format
  with type ('a, 'b, 'c, 'd) format4 := ('a, 'b, 'c, 'd) format4
  with type ('a, 'b, 'c, 'd, 'e, 'f) format6 := ('a, 'b, 'c, 'd, 'e, 'f) format6
  with type 'a ref := 'a ref)
  [@ocaml.warning "-3"]

(**/**)

open! Import
module Applicative = Applicative
module Array = Array
module Avltree = Avltree
module Backtrace = Backtrace
module Binary_search = Binary_search
module Binary_searchable = Binary_searchable
module Blit = Blit
module Bool = Bool
module Buffer = Buffer
module Bytes = Bytes
module Char = Char
module Comparable = Comparable
module Comparator = Comparator
module Comparisons = Comparisons
module Container = Container
module Either = Either
module Equal = Equal
module Error = Error
module Exn = Exn
module Field = Field
module Float = Float
module Floatable = Floatable
module Fn = Fn
module Formatter = Formatter
module Hash = Hash
module Hash_set = Hash_set
module Hashable = Hashable
module Hasher = Hasher
module Hashtbl = Hashtbl
module Identifiable = Identifiable
module Indexed_container = Indexed_container
module Info = Info
module Int = Int
module Int_conversions = Int_conversions
module Int32 = Int32
module Int63 = Int63
module Int64 = Int64
module Intable = Intable
module Int_math = Int_math
module Invariant = Invariant
module Lazy = Lazy
module List = List
module Map = Map
module Maybe_bound = Maybe_bound
module Monad = Monad
module Nativeint = Nativeint
module Nothing = Nothing
module Option = Option
module Option_array = Option_array
module Or_error = Or_error
module Ordered_collection_common = Ordered_collection_common
module Ordering = Ordering
module Poly = Poly
module Polymorphic_compare = Poly [@@deprecated "[since 2018-11] use [Poly] instead"]

module Popcount = Popcount
[@@deprecated "[since 2018-10] use [popcount] functions in the individual int modules"]

module Pretty_printer = Pretty_printer
module Printf = Printf
module Linked_queue = Linked_queue
module Queue = Queue
module Random = Random
module Ref = Ref
module Result = Result
module Sequence = Sequence
module Set = Set
module Sexpable = Sexpable
module Sign = Sign
module Sign_or_nan = Sign_or_nan
module Source_code_position = Source_code_position
module Stack = Stack
module Staged = Staged
module String = String
module Stringable = Stringable
module Sys = Sys
module T = T
module Type_equal = Type_equal
module Uniform_array = Uniform_array
module Unit = Unit
module Uchar = Uchar
module Variant = Variant
module With_return = With_return
module Word_size = Word_size

(* Avoid a level of indirection for uses of the signatures defined in [T]. *)
include T

(* This is a hack so that odoc creates better documentation. *)
module Sexp = struct
  include Sexp_with_comparable (** @inline *)
end

(**/**)

module Exported_for_specific_uses = struct
  module Fieldslib = Fieldslib
  module Ppx_hash_lib = Ppx_hash_lib
  module Variantslib = Variantslib
  module Ppx_compare_lib = Ppx_compare_lib
  module Ppx_enumerate_lib = Ppx_enumerate_lib

  let am_testing = am_testing
end

(**/**)

module Export = struct
  (* [deriving hash] is missing for [array] and [ref] since these types are mutable. *)
  type 'a array = 'a Array.t [@@deriving_inline compare, equal, sexp, sexp_grammar]

  let compare_array : 'a. ('a -> 'a -> int) -> 'a array -> 'a array -> int = Array.compare
  let equal_array : 'a. ('a -> 'a -> bool) -> 'a array -> 'a array -> bool = Array.equal

  let array_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a array =
    Array.t_of_sexp
  ;;

  let sexp_of_array : 'a. ('a -> Sexplib0.Sexp.t) -> 'a array -> Sexplib0.Sexp.t =
    Array.sexp_of_t
  ;;

  let (array_sexp_grammar :
         'a Sexplib0.Sexp_grammar.t -> 'a array Sexplib0.Sexp_grammar.t)
    =
    fun _'a_sexp_grammar -> Array.t_sexp_grammar _'a_sexp_grammar
  ;;

  [@@@end]

  type bool = Bool.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_bool = (Bool.compare : bool -> bool -> int)
  let equal_bool = (Bool.equal : bool -> bool -> bool)

  let (hash_fold_bool :
         Ppx_hash_lib.Std.Hash.state -> bool -> Ppx_hash_lib.Std.Hash.state)
    =
    Bool.hash_fold_t

  and (hash_bool : bool -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Bool.hash in
    fun x -> func x
  ;;

  let bool_of_sexp = (Bool.t_of_sexp : Sexplib0.Sexp.t -> bool)
  let sexp_of_bool = (Bool.sexp_of_t : bool -> Sexplib0.Sexp.t)
  let (bool_sexp_grammar : bool Sexplib0.Sexp_grammar.t) = Bool.t_sexp_grammar

  [@@@end]

  type char = Char.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_char = (Char.compare : char -> char -> int)
  let equal_char = (Char.equal : char -> char -> bool)

  let (hash_fold_char :
         Ppx_hash_lib.Std.Hash.state -> char -> Ppx_hash_lib.Std.Hash.state)
    =
    Char.hash_fold_t

  and (hash_char : char -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Char.hash in
    fun x -> func x
  ;;

  let char_of_sexp = (Char.t_of_sexp : Sexplib0.Sexp.t -> char)
  let sexp_of_char = (Char.sexp_of_t : char -> Sexplib0.Sexp.t)
  let (char_sexp_grammar : char Sexplib0.Sexp_grammar.t) = Char.t_sexp_grammar

  [@@@end]

  type exn = Exn.t [@@deriving_inline sexp_of]

  let sexp_of_exn = (Exn.sexp_of_t : exn -> Sexplib0.Sexp.t)

  [@@@end]

  type float = Float.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_float = (Float.compare : float -> float -> int)
  let equal_float = (Float.equal : float -> float -> bool)

  let (hash_fold_float :
         Ppx_hash_lib.Std.Hash.state -> float -> Ppx_hash_lib.Std.Hash.state)
    =
    Float.hash_fold_t

  and (hash_float : float -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Float.hash in
    fun x -> func x
  ;;

  let float_of_sexp = (Float.t_of_sexp : Sexplib0.Sexp.t -> float)
  let sexp_of_float = (Float.sexp_of_t : float -> Sexplib0.Sexp.t)
  let (float_sexp_grammar : float Sexplib0.Sexp_grammar.t) = Float.t_sexp_grammar

  [@@@end]

  type int = Int.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_int = (Int.compare : int -> int -> int)
  let equal_int = (Int.equal : int -> int -> bool)

  let (hash_fold_int : Ppx_hash_lib.Std.Hash.state -> int -> Ppx_hash_lib.Std.Hash.state) =
    Int.hash_fold_t

  and (hash_int : int -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Int.hash in
    fun x -> func x
  ;;

  let int_of_sexp = (Int.t_of_sexp : Sexplib0.Sexp.t -> int)
  let sexp_of_int = (Int.sexp_of_t : int -> Sexplib0.Sexp.t)
  let (int_sexp_grammar : int Sexplib0.Sexp_grammar.t) = Int.t_sexp_grammar

  [@@@end]

  type int32 = Int32.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_int32 = (Int32.compare : int32 -> int32 -> int)
  let equal_int32 = (Int32.equal : int32 -> int32 -> bool)

  let (hash_fold_int32 :
         Ppx_hash_lib.Std.Hash.state -> int32 -> Ppx_hash_lib.Std.Hash.state)
    =
    Int32.hash_fold_t

  and (hash_int32 : int32 -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Int32.hash in
    fun x -> func x
  ;;

  let int32_of_sexp = (Int32.t_of_sexp : Sexplib0.Sexp.t -> int32)
  let sexp_of_int32 = (Int32.sexp_of_t : int32 -> Sexplib0.Sexp.t)
  let (int32_sexp_grammar : int32 Sexplib0.Sexp_grammar.t) = Int32.t_sexp_grammar

  [@@@end]

  type int64 = Int64.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_int64 = (Int64.compare : int64 -> int64 -> int)
  let equal_int64 = (Int64.equal : int64 -> int64 -> bool)

  let (hash_fold_int64 :
         Ppx_hash_lib.Std.Hash.state -> int64 -> Ppx_hash_lib.Std.Hash.state)
    =
    Int64.hash_fold_t

  and (hash_int64 : int64 -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Int64.hash in
    fun x -> func x
  ;;

  let int64_of_sexp = (Int64.t_of_sexp : Sexplib0.Sexp.t -> int64)
  let sexp_of_int64 = (Int64.sexp_of_t : int64 -> Sexplib0.Sexp.t)
  let (int64_sexp_grammar : int64 Sexplib0.Sexp_grammar.t) = Int64.t_sexp_grammar

  [@@@end]

  type 'a list = 'a List.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_list : 'a. ('a -> 'a -> int) -> 'a list -> 'a list -> int = List.compare
  let equal_list : 'a. ('a -> 'a -> bool) -> 'a list -> 'a list -> bool = List.equal

  let hash_fold_list :
    'a.
    (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> 'a list
    -> Ppx_hash_lib.Std.Hash.state
    =
    List.hash_fold_t
  ;;

  let list_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a list =
    List.t_of_sexp
  ;;

  let sexp_of_list : 'a. ('a -> Sexplib0.Sexp.t) -> 'a list -> Sexplib0.Sexp.t =
    List.sexp_of_t
  ;;

  let (list_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a list Sexplib0.Sexp_grammar.t) =
    fun _'a_sexp_grammar -> List.t_sexp_grammar _'a_sexp_grammar
  ;;

  [@@@end]

  type nativeint = Nativeint.t
  [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_nativeint = (Nativeint.compare : nativeint -> nativeint -> int)
  let equal_nativeint = (Nativeint.equal : nativeint -> nativeint -> bool)

  let (hash_fold_nativeint :
         Ppx_hash_lib.Std.Hash.state -> nativeint -> Ppx_hash_lib.Std.Hash.state)
    =
    Nativeint.hash_fold_t

  and (hash_nativeint : nativeint -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Nativeint.hash in
    fun x -> func x
  ;;

  let nativeint_of_sexp = (Nativeint.t_of_sexp : Sexplib0.Sexp.t -> nativeint)
  let sexp_of_nativeint = (Nativeint.sexp_of_t : nativeint -> Sexplib0.Sexp.t)

  let (nativeint_sexp_grammar : nativeint Sexplib0.Sexp_grammar.t) =
    Nativeint.t_sexp_grammar
  ;;

  [@@@end]

  type 'a option = 'a Option.t
  [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_option : 'a. ('a -> 'a -> int) -> 'a option -> 'a option -> int =
    Option.compare
  ;;

  let equal_option : 'a. ('a -> 'a -> bool) -> 'a option -> 'a option -> bool =
    Option.equal
  ;;

  let hash_fold_option :
    'a.
    (Ppx_hash_lib.Std.Hash.state -> 'a -> Ppx_hash_lib.Std.Hash.state)
    -> Ppx_hash_lib.Std.Hash.state
    -> 'a option
    -> Ppx_hash_lib.Std.Hash.state
    =
    Option.hash_fold_t
  ;;

  let option_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a option =
    Option.t_of_sexp
  ;;

  let sexp_of_option : 'a. ('a -> Sexplib0.Sexp.t) -> 'a option -> Sexplib0.Sexp.t =
    Option.sexp_of_t
  ;;

  let (option_sexp_grammar :
         'a Sexplib0.Sexp_grammar.t -> 'a option Sexplib0.Sexp_grammar.t)
    =
    fun _'a_sexp_grammar -> Option.t_sexp_grammar _'a_sexp_grammar
  ;;

  [@@@end]

  type 'a ref = 'a Ref.t [@@deriving_inline compare, equal, sexp, sexp_grammar]

  let compare_ref : 'a. ('a -> 'a -> int) -> 'a ref -> 'a ref -> int = Ref.compare
  let equal_ref : 'a. ('a -> 'a -> bool) -> 'a ref -> 'a ref -> bool = Ref.equal

  let ref_of_sexp : 'a. (Sexplib0.Sexp.t -> 'a) -> Sexplib0.Sexp.t -> 'a ref =
    Ref.t_of_sexp
  ;;

  let sexp_of_ref : 'a. ('a -> Sexplib0.Sexp.t) -> 'a ref -> Sexplib0.Sexp.t =
    Ref.sexp_of_t
  ;;

  let (ref_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a ref Sexplib0.Sexp_grammar.t) =
    fun _'a_sexp_grammar -> Ref.t_sexp_grammar _'a_sexp_grammar
  ;;

  [@@@end]

  type string = String.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_string = (String.compare : string -> string -> int)
  let equal_string = (String.equal : string -> string -> bool)

  let (hash_fold_string :
         Ppx_hash_lib.Std.Hash.state -> string -> Ppx_hash_lib.Std.Hash.state)
    =
    String.hash_fold_t

  and (hash_string : string -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = String.hash in
    fun x -> func x
  ;;

  let string_of_sexp = (String.t_of_sexp : Sexplib0.Sexp.t -> string)
  let sexp_of_string = (String.sexp_of_t : string -> Sexplib0.Sexp.t)
  let (string_sexp_grammar : string Sexplib0.Sexp_grammar.t) = String.t_sexp_grammar

  [@@@end]

  type bytes = Bytes.t [@@deriving_inline compare, equal, sexp, sexp_grammar]

  let compare_bytes = (Bytes.compare : bytes -> bytes -> int)
  let equal_bytes = (Bytes.equal : bytes -> bytes -> bool)
  let bytes_of_sexp = (Bytes.t_of_sexp : Sexplib0.Sexp.t -> bytes)
  let sexp_of_bytes = (Bytes.sexp_of_t : bytes -> Sexplib0.Sexp.t)
  let (bytes_sexp_grammar : bytes Sexplib0.Sexp_grammar.t) = Bytes.t_sexp_grammar

  [@@@end]

  type unit = Unit.t [@@deriving_inline compare, equal, hash, sexp, sexp_grammar]

  let compare_unit = (Unit.compare : unit -> unit -> int)
  let equal_unit = (Unit.equal : unit -> unit -> bool)

  let (hash_fold_unit :
         Ppx_hash_lib.Std.Hash.state -> unit -> Ppx_hash_lib.Std.Hash.state)
    =
    Unit.hash_fold_t

  and (hash_unit : unit -> Ppx_hash_lib.Std.Hash.hash_value) =
    let func = Unit.hash in
    fun x -> func x
  ;;

  let unit_of_sexp = (Unit.t_of_sexp : Sexplib0.Sexp.t -> unit)
  let sexp_of_unit = (Unit.sexp_of_t : unit -> Sexplib0.Sexp.t)
  let (unit_sexp_grammar : unit Sexplib0.Sexp_grammar.t) = Unit.t_sexp_grammar

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

  (** Float operators *)

  include Float.O_dot

  (* This is declared as an external to be optimized away in more contexts. *)

  (** Reverse application operator. [x |> g |> f] is equivalent to [f (g (x))]. *)
  external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"

  (** Application operator. [g @@ f @@ x] is equivalent to [g (f (x))]. *)
  external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"

  (** Boolean operations *)

  (* These need to be declared as an external to get the lazy behavior *)
  external ( && ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool = "%sequand"
  external ( || ) : (bool[@local_opt]) -> (bool[@local_opt]) -> bool = "%sequor"
  external not : (bool[@local_opt]) -> bool = "%boolnot"

  (* This must be declared as an external for the warnings to work properly. *)
  external ignore : _ -> unit = "%ignore"

  (** Common string operations *)
  let ( ^ ) = String.( ^ )

  (** Reference operations *)

  (* Declared as an externals so that the compiler skips the caml_modify when possible and
     to keep reference unboxing working *)
  external ( ! ) : ('a ref[@local_opt]) -> 'a = "%field0"
  external ref : 'a -> ('a ref[@local_opt]) = "%makemutable"
  external ( := ) : ('a ref[@local_opt]) -> 'a -> unit = "%setfield0"

  (** Pair operations *)

  let fst = fst
  let snd = snd

  (** Exceptions stuff *)

  (* Declared as an external so that the compiler may rewrite '%raise' as '%reraise'. *)
  external raise : exn -> _ = "%raise"

  let failwith = failwith
  let invalid_arg = invalid_arg
  let raise_s = Error.raise_s

  (** Misc *)

  let phys_equal = phys_equal

  external force : 'a Lazy.t -> 'a = "%lazy_force"
end

include Export

include Container_intf.Export (** @inline *)

exception Not_found_s = Not_found_s

(* We perform these side effects here because we want them to run for any code that uses
   [Base].  If this were in another module in [Base] that was not used in some program,
   then the side effects might not be run in that program.  This will run as long as the
   program refers to at least one value directly in [Base]; referring to values in
   [Base.Bool], for example, is not sufficient. *)
let () = Backtrace.initialize_module ()
