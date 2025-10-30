(** OCaml record field. *)

(**/**)

module For_generated_code : sig
  (*_ don't use this by hand, it is only meant for ppx_fields_conv *)

  type ('perm, 'record, 'field) t =
    { force_variance : 'perm -> unit
    ; name : string
    ; setter : ('record -> 'field -> unit) option
    ; getter : 'record -> 'field
    ; fset : 'record -> 'field -> 'record
    }

  external opaque_identity : 'a. ('a[@local_opt]) -> ('a[@local_opt]) = "%opaque"
  [@@layout_poly]
end

(**/**)

(** ['record] is the type of the record. ['field] is the type of the values stored in the
    record field with name [name]. ['perm] is a way of restricting the operations that can
    be used. *)
type ('perm, 'record, 'field) t_with_perm =
  | Field of ('perm, 'record, 'field) For_generated_code.t
[@@unboxed]

(** A record field with no restrictions. *)
type ('record, 'field) t = ([ `Read | `Set_and_create ], 'record, 'field) t_with_perm

(** A record that can only be read, because it belongs to a private type. *)
type ('record, 'field) readonly_t = ([ `Read ], 'record, 'field) t_with_perm

val name : (_, _, _) t_with_perm -> string

[%%template:
[@@@kind.default
  k
  = ( base_or_null_with_imm
    , value & value & value & bits32
    , value & value & value & value & value
    , value & value & value & value & value & value
    , float64 & float64 & float64 & float64 & float64 & float64
    , (float64 & float64 & float64 & float64 & float64 & float64)
      & (float64 & float64 & float64 & float64 & float64 & float64)
      & (value & value & value & value & value & value)
      & value
      & value )]

val get : 'perm 'r 'a. ('perm, 'r, 'a) t_with_perm -> 'r -> 'a
val fset : 'a 'r. ([> `Set_and_create ], 'r, 'a) t_with_perm -> 'r -> 'a -> 'r

val setter
  : 'a 'r.
  ([> `Set_and_create ], 'r, 'a) t_with_perm -> ('r -> 'a -> unit) option

(** [map] and [updater] can't be polymorphic over 'r, as that requires passing/return
    [any] values. *)
val map : 'a 'r. ([> `Set_and_create ], 'r, 'a) t_with_perm -> 'r -> f:('a -> 'a) -> 'r

val updater
  : 'a 'r.
  ([> `Set_and_create ], 'r, 'a) t_with_perm -> ('r -> f:('a -> 'a) -> unit) option]

type ('perm, 'record, 'result) user =
  { f : 'field. ('perm, 'record, 'field) t_with_perm -> 'result }
