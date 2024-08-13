[@@@warning "-preprocessor"]

module type S = sig
  type t [@@deriving_inline specialize_polymorphic_compare]

  [@@@ocaml.ppwarning "ppx_base_internal: intended only for use inside Base"]

  external ( = ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external ( <> ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%notequal"
  external ( < ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessthan"
  external ( > ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterthan"
  external ( <= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%lessequal"
  external ( >= ) : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%greaterequal"
  external compare : (t[@local_opt]) -> (t[@local_opt]) -> int = "%compare"
  external compare__local : (t[@local_opt]) -> (t[@local_opt]) -> int = "%compare"
  external equal : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  external equal__local : (t[@local_opt]) -> (t[@local_opt]) -> bool = "%equal"
  val ascending : t -> t -> int
  val descending : t -> t -> int
  val max : t -> t -> t
  val min : t -> t -> t

  [@@@end]
end

module type Replace_polymorphic_compare = sig
  module type S = S

  module Bool_replace_polymorphic_compare : S with type t := bool
  module Bytes_replace_polymorphic_compare : S with type t := bytes
  module Char_replace_polymorphic_compare : S with type t := char
  module Float_replace_polymorphic_compare : S with type t := float
  module Int32_replace_polymorphic_compare : S with type t := int32
  module Int64_replace_polymorphic_compare : S with type t := int64
  module Int_replace_polymorphic_compare : S with type t := int
  module Nativeint_replace_polymorphic_compare : S with type t := nativeint
  module String_replace_polymorphic_compare : S with type t := string
  module Uchar_replace_polymorphic_compare : S with type t := Stdlib.Uchar.t
end
