[@@@warning "-preprocessor"]

include Replace_polymorphic_compare_intf

module Bool_replace_polymorphic_compare = struct
  type t = bool [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module Bytes_replace_polymorphic_compare = struct
  type t = bytes [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module Char_replace_polymorphic_compare = struct
  type t = char [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module Float_replace_polymorphic_compare = struct
  type t = float [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module Int32_replace_polymorphic_compare = struct
  type t = int32 [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module Int64_replace_polymorphic_compare = struct
  type t = int64 [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module Int_replace_polymorphic_compare = struct
  type t = int [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module Nativeint_replace_polymorphic_compare = struct
  type t = nativeint [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module String_replace_polymorphic_compare = struct
  type t = string [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end

module Uchar_replace_polymorphic_compare = struct
  type t = Stdlib.Uchar.t [@@deriving_inline specialize_polymorphic_compare]

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

  let ascending x y = compare x y
  let descending x y = compare y x
  let max x y = Bool0.select (x >= y) x y
  let min x y = Bool0.select (x <= y) x y

  [@@@end]
end
