open! Import0

module Definitions = struct
  module type S = sig
    type t
    type value

    val to_or_null : t -> value or_null
    val of_or_null : value or_null -> t
  end

  module type S_with_zero_alloc = sig
    type t
    type value

    val to_or_null : t -> value or_null [@@zero_alloc]
    val of_or_null : value or_null -> t [@@zero_alloc]
  end
end

module type Or_nullable = sig
  include module type of struct
    include Definitions
  end
end
