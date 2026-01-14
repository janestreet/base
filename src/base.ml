(** This module is the toplevel of the Base library; it's what you get when you write
    [open Base].

    The goal of Base is both to be a more complete standard library, with richer APIs, and
    to be more consistent in its design. For instance, in the standard library some things
    have modules and others don't; in Base, everything is a module.

    Base extends some modules and data structures from the standard library, like [Array],
    [Buffer], [Bytes], [Char], [Hashtbl], [Int32], [Int64], [Lazy], [List], [Map],
    [Nativeint], [Printf], [Random], [Set], [String], [Sys], and [Uchar]. One key
    difference is that Base doesn't use exceptions as much as the standard library and
    instead makes heavy use of the [Result] type, as in:

    {[
      type ('a, 'b) result =
        | Ok of 'a
        | Error of 'b
    ]}

    Base also adds entirely new modules, most notably:

    - [Comparable], [Comparator], and [Comparisons] in lieu of polymorphic compare.
    - [Container], which provides a consistent interface across container-like data
      structures (arrays, lists, strings).
    - [Result], [Error], and [Or_error], supporting the or-error pattern. *)

(*_ We hide this from the web docs because the line wrapping is bad, making it pretty much
    inscrutable. *)
(**/**)

(* The intent is to shadow all of INRIA's standard library. Modules below would cause
   compilation errors without being removed from [Shadow_stdlib] before inclusion. *)

include (
  Shadow_stdlib :
    module type of struct
      include Shadow_stdlib
    end
    (* Modules defined in Base *)
    with module Array := Shadow_stdlib.Array
    with module Bool := Shadow_stdlib.Bool
    with module Buffer := Shadow_stdlib.Buffer
    with module Bytes := Shadow_stdlib.Bytes
    with module Char := Shadow_stdlib.Char
    with module Condition := Shadow_stdlib.Condition
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
    with module Modes := Shadow_stdlib.Modes
    with module Nativeint := Shadow_stdlib.Nativeint
    with module Obj := Shadow_stdlib.Obj
    with module Option := Shadow_stdlib.Option
    with module Out_channel := Shadow_stdlib.Out_channel
    with module Printf := Shadow_stdlib.Printf
    with module Queue := Shadow_stdlib.Queue
    with module Random := Shadow_stdlib.Random
    with module Result := Shadow_stdlib.Result
    with module Set := Shadow_stdlib.Set
    with module Semaphore := Shadow_stdlib.Semaphore
    with module Stack := Shadow_stdlib.Stack
    with module String := Shadow_stdlib.String
    with module Sys := Shadow_stdlib.Sys
    with module Uchar := Shadow_stdlib.Uchar
    with module Unit := Shadow_stdlib.Unit
    (* OCaml 5-related modules we don't want to start shadowing yet. *)
    with module Domain := Shadow_stdlib.Domain
    with module Type := Shadow_stdlib.Type
    (* Support for generated lexers *)
    with module Lexing := Shadow_stdlib.Lexing
    with type ('a, 'b, 'c) format := ('a, 'b, 'c) format
    with type ('a, 'b, 'c, 'd) format4 := ('a, 'b, 'c, 'd) format4
    with type ('a, 'b, 'c, 'd, 'e, 'f) format6 := ('a, 'b, 'c, 'd, 'e, 'f) format6
    with type ('a : value_or_null) ref := 'a ref)
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
module Dynamic = Dynamic
module Either = Either
module Equal = Equal
module Error = Error_with_extras
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
module Iarray = Iarray
module Identifiable = Identifiable
module Indexed_container = Indexed_container
module Info = Info
module Int = Int
module Int32 = Int32
module Int63 = Int63
module Int64 = Int64
module Intable = Intable
module Int_math = Int_math
module Invariant = Invariant
module Dictionary_immutable = Dictionary_immutable
module Dictionary_mutable = Dictionary_mutable
module Lazy = Lazy
module List = List
module Map = Map
module Maybe_bound = Maybe_bound
module Modes = Modes
module Monad = Monad
module Nativeint = Nativeint
module Nothing = Nothing
module Obj = Obj
module Option = Option
module Option_array = Option_array
module Or_error = Or_error
module Or_null = Or_null
module Ordered_collection_common = Ordered_collection_common
module Ordering = Ordering
module Poly = Poly
module Portability_hacks = Basement.Portability_hacks
module Portable_lazy = Portable_lazy
module Pretty_printer = Pretty_printer
module Printf = Printf
module Linked_queue = Linked_queue
module Nonempty_list = Nonempty_list
module Queue = Queue
module Random = Random
module Ref = Ref
module Result = Result
module Sequence = Sequence
module Set = Set
module Sexp = Sexp
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
module Toplevel_value = Toplevel_value
module Type_equal = Type_equal
module Uniform_array = Uniform_array
module Unit = Unit
module Uchar = Uchar
module Variant = Variant
module With_return = With_return
module Word_size = Word_size

(* Avoid a level of indirection for uses of the signatures defined in [T]. *)
include T

(* [Int_string_conversions] is separated from [Int_conversions] for dependency reasons,
   but this separation is not important for clients. *)
module Int_conversions = struct
  include Int_conversions
  include Int_string_conversions
end

(**/**)

module Exported_for_specific_uses = struct
  module Globalize = Globalize
  module Integer_to_string = Integer_to_string
  module Obj_array = Obj_array

  let am_testing = am_testing
end

(**/**)

module Export = struct
  type ('a : any mod separable) array = 'a Array.t

  (* [deriving hash] is missing for [array] and [ref] since these types are mutable. *)
  [%%rederive.portable
    type ('a : value_or_null mod separable) array = 'a Array.t
    [@@deriving
      compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar]]

  type bool = Bool.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type char = Char.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type exn = Exn.t [@@deriving sexp_of]

  type float = Float.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type ('a : any mod separable) iarray = 'a Iarray.t

  [%%rederive.portable
    type 'a iarray = 'a Iarray.t
    [@@deriving
      compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]]

  type int = Int.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type int32 = Int32.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type int64 = Int64.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type ('a : value_or_null) list = 'a List.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type nativeint = Nativeint.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type ('a : value_or_null) option = 'a Option.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type%template nonrec ('a : k) option = ('a Option.t[@kind k])
  [@@deriving compare ~localize, equal ~localize, sexp ~stackify]
  [@@kind k = base_non_value]

  type ('a : value_or_null, 'b : value_or_null) result = ('a, 'b) Result.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type%template nonrec ('a : k, 'b : value_or_null) result = (('a, 'b) Result.t[@kind k])
  [@@deriving compare ~localize, equal ~localize, sexp ~stackify]
  [@@kind k = base_non_value]

  type ('a : value_or_null) ref = 'a Ref.t
  [@@deriving compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar]

  type string = String.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  type bytes = Bytes.t
  [@@deriving compare ~localize, equal ~localize, globalize, sexp ~stackify, sexp_grammar]

  type unit = Unit.t
  [@@deriving
    compare ~localize, equal ~localize, globalize, hash, sexp ~stackify, sexp_grammar]

  (** Format stuff *)

  type nonrec ('a, 'b, 'c) format = ('a, 'b, 'c) format
  type nonrec ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) format4
  type nonrec ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) format6

  (** List operators *)

  include List.Infix

  (** Iarray operators *)

  include Iarray.O

  (** Int operators and comparisons *)

  include Int.O
  include Int_replace_polymorphic_compare

  (** Float operators *)

  include Float.O_dot

  (* This is declared as an external to be optimized away in more contexts. *)

  (** Reverse application operator. [x |> g |> f] is equivalent to [f (g (x))]. *)
  external ( |> )
    : ('a : any) ('b : any).
    'a -> (('a -> 'b)[@local_opt]) -> 'b
    @@ portable
    = "%revapply"
  [@@layout_poly]

  (** Application operator. [g @@ f @@ x] is equivalent to [g (f (x))]. *)
  external ( @@ )
    : ('a : any) ('b : any).
    (('a -> 'b)[@local_opt]) -> 'a -> 'b
    @@ portable
    = "%apply"
  [@@layout_poly]

  (** Boolean operations *)

  (* These need to be declared as an external to get the lazy behavior *)
  external ( && )
    :  (bool[@local_opt])
    -> (bool[@local_opt])
    -> bool
    @@ portable
    = "%sequand"

  external ( || )
    :  (bool[@local_opt])
    -> (bool[@local_opt])
    -> bool
    @@ portable
    = "%sequor"

  external not : (bool[@local_opt]) -> bool @@ portable = "%boolnot"

  (* This must be declared as an external for the warnings to work properly. *)
  external ignore
    : ('a : any).
    ('a[@local_opt]) @ immutable once -> unit
    @@ portable
    = "%ignore"
  [@@layout_poly]

  (** Common string operations *)
  let ( ^ ) = String.( ^ )

  (** Reference operations *)

  (* Declared as an externals so that the compiler skips the caml_modify when possible and
     to keep reference unboxing working *)
  external ( ! )
    : ('a : value_or_null).
    ('a ref[@local_opt]) -> 'a
    @@ stateless
    = "%field0"

  external ref
    : ('a : value_or_null).
    'a -> ('a ref[@local_opt])
    @@ stateless
    = "%makemutable"

  external ( := )
    : ('a : value_or_null).
    ('a ref[@local_opt]) -> 'a -> unit
    @@ stateless
    = "%setfield0"

  (** Pair operations *)

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

  (** Exceptions stuff *)

  (* Declared as an external so that the compiler may rewrite '%raise' as '%reraise'. *)
  external raise
    : ('a : value_or_null).
    exn -> 'a @ portable unique
    @@ stateless
    = "%reraise"

  let failwith = failwith
  let invalid_arg = invalid_arg
  let raise_s = Error.raise_s

  (** Misc *)

  external phys_equal
    : ('a : value_or_null).
    ('a[@local_opt]) @ contended -> ('a[@local_opt]) @ contended -> bool
    @@ portable
    = "%eq"

  external force : ('a Lazy.t[@local_opt]) -> 'a @@ portable = "%lazy_force"

  (* Export ['a or_null] with constructors [Null] and [This] whenever Base is opened, so
     uses of those identifiers work in both upstream OCaml and OxCaml. *)

  type 'a or_null = 'a Or_null.t
  [@@or_null_reexport]
  [@@deriving compare ~localize, equal ~localize, globalize, hash, sexp ~stackify]
end

include Export

include Container.Export (** @inline *)

include Modes.Export (** @inline *)

exception Not_found_s = Not_found_s

(* We perform these side effects here because we want them to run for any code that uses
   [Base]. If this were in another module in [Base] that was not used in some program,
   then the side effects might not be run in that program. This will run as long as the
   program refers to at least one value directly in [Base]; referring to values in
   [Base.Bool], for example, is not sufficient. *)
let () = Backtrace.initialize_module ()
