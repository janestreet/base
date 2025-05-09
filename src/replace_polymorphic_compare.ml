[@@@warning "-preprocessor"]

include Replace_polymorphic_compare_intf.Definitions

module Bool_replace_polymorphic_compare = struct
  type t = bool [@@deriving specialize_polymorphic_compare]
end

module Bytes_replace_polymorphic_compare = struct
  type t = bytes [@@deriving specialize_polymorphic_compare]
end

module Char_replace_polymorphic_compare = struct
  type t = char [@@deriving specialize_polymorphic_compare]
end

module Float_replace_polymorphic_compare = struct
  type t = float [@@deriving specialize_polymorphic_compare]
end

module Int32_replace_polymorphic_compare = struct
  type t = int32 [@@deriving specialize_polymorphic_compare]
end

module Int64_replace_polymorphic_compare = struct
  type t = int64 [@@deriving specialize_polymorphic_compare]
end

module Int_replace_polymorphic_compare = struct
  type t = int [@@deriving specialize_polymorphic_compare]
end

module Nativeint_replace_polymorphic_compare = struct
  type t = nativeint [@@deriving specialize_polymorphic_compare]
end

module String_replace_polymorphic_compare = struct
  type t = string [@@deriving specialize_polymorphic_compare]
end

module Uchar_replace_polymorphic_compare = struct
  type t = Stdlib.Uchar.t [@@deriving specialize_polymorphic_compare]
end
