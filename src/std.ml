open! Import

module Applicative               = Applicative
module Applicative_intf          = Applicative_intf
module Array                     = Base_array
module Array_permute             = Array_permute
module Avltree                   = Avltree
module Binary_search             = Binary_search
module Binary_searchable         = Binary_searchable
module Binary_searchable_intf    = Binary_searchable_intf
module Blit                      = Blit
module Blit_intf                 = Blit_intf
module Bool                      = Bool
module Caml                      = Caml
module Char                      = Base_char
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
module Hash_set                  = Base_hash_set
module Hash_set_intf             = Base_hash_set_intf
module Hashable                  = Hashable
module Hasher                    = Hasher
module Hashtbl                   = Base_hashtbl
module Hashtbl_intf              = Base_hashtbl_intf
module Heap_block                = Heap_block
module Identifiable              = Identifiable
module Info                      = Info
module Info_intf                 = Info_intf
module Int                       = Base_int
module Int32                     = Base_int32
module Int63                     = Base_int63
module Int64                     = Base_int64
module Int_intf                  = Int_intf
module Intable                   = Intable
module Invariant                 = Invariant
module Lazy                      = Base_lazy
module List                      = Base_list
module Map                       = Base_map
module Map_intf                  = Base_map_intf
module Maybe_bound               = Maybe_bound
module Monad                     = Monad
module Nativeint                 = Base_nativeint
module Option                    = Option
module Or_error                  = Or_error
module Ordered_collection_common = Ordered_collection_common
module Ordering                  = Ordering
module Poly                      = Poly
module Polymorphic_compare       = Polymorphic_compare
module Polymorphic_compare_intf  = Polymorphic_compare_intf
module Popcount                  = Popcount
module Pretty_printer            = Pretty_printer
module Printf                    = Base_printf
module Random                    = Base_random
module Result                    = Result
module Sequence                  = Sequence
module Set                       = Base_set
module Set_intf                  = Base_set_intf
module Sexpable                  = Sexpable
module Sign                      = Sign
module Source_code_position      = Source_code_position
module Staged                    = Staged
module String                    = Base_string
module Stringable                = Stringable
module T                         = T
module Validate                  = Validate
module Variant                   = Variant
module With_return               = With_return
module Word_size                 = Word_size

module Export = struct
  (* [deriving hash] is missing for [array] and [ref] since these types are mutable.
     (string is also mutable, but we pretend it isn't for hashing purposes) *)
  type 'a array  = 'a Array.  t [@@deriving                sexp]
  type bool      = Bool.      t [@@deriving compare, hash, sexp]
  type char      = Char.      t [@@deriving compare, hash, sexp]
  type exn       = Exn.       t [@@deriving                sexp_of]
  type float     = Float.     t [@@deriving compare, hash, sexp]
  type int       = Int.       t [@@deriving compare, hash, sexp]
  type int32     = Int32.     t [@@deriving compare, hash, sexp]
  type int64     = Int64.     t [@@deriving compare, hash, sexp]
  type 'a list   = 'a List.   t [@@deriving          hash, sexp]
  type nativeint = Nativeint. t [@@deriving compare, hash, sexp]
  type 'a option = 'a Option. t [@@deriving          hash, sexp]
  type string    = String.    t [@@deriving compare, hash, sexp]
(*
     type 'a ref    = 'a Ref.      t [@@deriving                sexp]
     type unit      = Unit.        t [@@deriving compare, hash, sexp]
  *)

  let raise_s = Error.raise_s

  (** Format stuff *)
  type nonrec ('a, 'b, 'c) format = ('a, 'b, 'c) format
  type nonrec ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'd) format4
  type nonrec ('a, 'b, 'c, 'd, 'e, 'f) format6 = ('a, 'b, 'c, 'd, 'e, 'f) format6

  (** List operators *)
  include List.Infix

  (** Int operators *)
  include Int.O

  (** Composition operator *)
  external ( |> ) : 'a -> ( 'a -> 'b) -> 'b = "%revapply"

  (** Boolean operations *)
  external ( && ) : bool -> bool -> bool = "%sequand"
  external ( || ) : bool -> bool -> bool = "%sequor"
  external not : bool -> bool = "%boolnot"

  (* This need to be delcared as an external for the warnings to work properly *)
  external ignore : _ -> unit = "%ignore"

  (** Common string operations *)
  let ( ^ ) = String.( ^ )

  (** Reference operations *)
  let ref = ref
  let ( ! ) = ( ! )
  let ( := ) = ( := )

  (** Pair operations *)
  let fst = fst
  let snd = snd

  (** Exceptions stuff *)
  external raise : exn -> _ = "%raise"
  let failwith = failwith
  let invalid_arg = invalid_arg

  (** Misc *)
  let phys_equal = ( == )
end

include Export
