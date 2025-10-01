@@ portable

type backend_type : value mod contended portable = Stdlib.Sys.backend_type =
  | Native
  | Bytecode
  | Other of string

val backend_type : backend_type
val interactive : bool ref
val os_type : string
val unix : bool
val win32 : bool
val cygwin : bool
val word_size_in_bits : int
val int_size_in_bits : int
val big_endian : bool
val max_string_length : int
val max_array_length : int
val runtime_variant : unit -> string
val runtime_parameters : unit -> string
val argv : string array
val get_argv : unit -> string array @@ nonportable
val ocaml_version : string
val enable_runtime_warnings : bool -> unit
val runtime_warnings_enabled : unit -> bool

module Make_immediate64
    (Imm : Stdlib.Sys.Immediate64.Immediate)
    (Non_imm : Stdlib.Sys.Immediate64.Non_immediate) : sig
  type t : immediate64

  type 'a repr =
    | Immediate : Imm.t repr
    | Non_immediate : Non_imm.t repr

  val repr : t repr
end

val getenv_exn : string -> string
val getenv : string -> string option

external%template opaque_identity
  : ('a : any).
  ('a[@local_opt]) @ c o p u -> ('a[@local_opt]) @ c o p u
  = "%opaque"
[@@layout_poly]
[@@mode
  c = (uncontended, shared, contended)
  , o = (many, once)
  , p = (nonportable, portable)
  , u = (aliased, unique)]

external opaque_identity_global : ('a : any). 'a -> 'a = "%opaque" [@@layout_poly]

(** [= Stdlib.Sys.Break] *)
exception Break
