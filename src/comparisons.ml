(** Interfaces for infix comparison operators and comparison functions. *)

open! Import

(** [Infix] lists the typical infix comparison operators. These functions are provided by
    [<M>.O] modules, i.e., modules that expose monomorphic infix comparisons over some
    [<M>.t]. *)
module type Infix = sig
  type t : any

  val ( >= ) : t -> t -> bool
  val ( <= ) : t -> t -> bool
  val ( = ) : t -> t -> bool
  val ( > ) : t -> t -> bool
  val ( < ) : t -> t -> bool
  val ( <> ) : t -> t -> bool
end

module type%template [@mode m = (global, local)] S = sig
  include Infix

  [%%template:
  [@@@mode.default m = (global, m)]

  val equal : t @ m -> t @ m -> bool

  (** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is
      less than [t2], and a positive integer if [t1] is greater than [t2]. *)
  val compare : t @ m -> t @ m -> int]

  val min : t -> t -> t
  val max : t -> t -> t
end

module type Infix_with_local_opt = sig
  type t

  external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
  external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
  external ( <> ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%notequal"
  external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
  external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
end

module type%template [@mode m = (global, local)] S_with_local_opt = sig
  include Infix_with_local_opt

  [%%template:
  [@@@mode.default m = (global, m)]

  external equal : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external compare : (t[@local_opt]) -> (t[@local_opt]) -> int = "%compare"]

  [%%template:
  [@@@mode.default m = (global, local)]

  val min : t @ m -> t @ m -> t @ m
  val max : t @ m -> t @ m -> t @ m]
end

module type Infix_with_zero_alloc = sig
  type t : any

  val ( >= ) : t -> t -> bool [@@zero_alloc]
  val ( <= ) : t -> t -> bool [@@zero_alloc]
  val ( = ) : t -> t -> bool [@@zero_alloc]
  val ( > ) : t -> t -> bool [@@zero_alloc]
  val ( < ) : t -> t -> bool [@@zero_alloc]
  val ( <> ) : t -> t -> bool [@@zero_alloc]
end

module type%template [@mode m = (global, local)] S_with_zero_alloc = sig
  include Infix_with_zero_alloc

  [%%template:
  [@@@mode.default m = (global, m)]

  val equal : t @ m -> t @ m -> bool [@@zero_alloc]

  (** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is
      less than [t2], and a positive integer if [t1] is greater than [t2]. *)
  val compare : t @ m -> t @ m -> int
  [@@zero_alloc]]

  val min : t -> t -> t [@@zero_alloc]
  val max : t -> t -> t [@@zero_alloc]
end

module type Infix_with_zero_alloc_strict = sig
  type t

  val ( >= ) : t -> t -> bool [@@zero_alloc strict]
  val ( <= ) : t -> t -> bool [@@zero_alloc strict]
  val ( = ) : t -> t -> bool [@@zero_alloc strict]
  val ( > ) : t -> t -> bool [@@zero_alloc strict]
  val ( < ) : t -> t -> bool [@@zero_alloc strict]
  val ( <> ) : t -> t -> bool [@@zero_alloc strict]
end

module type%template [@mode m = (global, local)] S_with_zero_alloc_strict = sig
  include Infix_with_zero_alloc_strict

  [%%template:
  [@@@mode.default m = (global, m)]

  val equal : t @ m -> t @ m -> bool [@@zero_alloc strict]

  (** [compare t1 t2] returns 0 if [t1] is equal to [t2], a negative integer if [t1] is
      less than [t2], and a positive integer if [t1] is greater than [t2]. *)
  val compare : t @ m -> t @ m -> int
  [@@zero_alloc strict]]

  val min : t -> t -> t [@@zero_alloc strict]
  val max : t -> t -> t [@@zero_alloc strict]
end
