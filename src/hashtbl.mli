(** Hash tables, mutable lookup-tables supporting constant-time lookup and in-place
    modification.*)

include Hashtbl_intf.Hashtbl (** @inline *)
