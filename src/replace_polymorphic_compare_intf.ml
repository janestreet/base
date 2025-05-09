[@@@warning "-preprocessor"]

module Definitions = struct
  module type S = sig
    type t [@@deriving specialize_polymorphic_compare]
  end
end

module type Replace_polymorphic_compare = sig @@ portable
  include module type of struct
    include Definitions
  end

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
