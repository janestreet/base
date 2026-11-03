open! Import0

module%template Definitions = struct
  [@@@mode.default m = (global, local)]

  module type S = sig
    type t
    type value

    [@@@mode.default m = (global, m)]

    val to_or_null : t @ m -> value or_null @ m
    val of_or_null : value or_null @ m -> t @ m
  end

  module type S_with_zero_alloc = sig
    type t
    type value

    [@@@mode.default m = (global, m)]

    val to_or_null : t @ m -> value or_null @ m [@@zero_alloc]
    val of_or_null : value or_null @ m -> t @ m [@@zero_alloc]
  end
end

module type Or_nullable = sig
  include module type of struct
    include Definitions
  end
end
