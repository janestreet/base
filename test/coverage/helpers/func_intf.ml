open! Base

module Definitions = struct
  module type With_compare = sig
    type t [@@deriving compare, equal]
  end
end

module type Func = sig
  include module type of struct
    include Definitions
  end

  type ('input, 'output) t =
    { initial : 'output
    ; transitions : ('input * 'output) list
    }
  [@@deriving equal, quickcheck, sexp_of]

  val inputs : ('input, 'output) t -> 'input list
  val outputs : ('input, 'output) t -> 'output list

  val map
    :  ('input1, 'output1) t
    -> i:('input1 -> 'input2)
    -> o:('output1 -> 'output2)
    -> ('input2, 'output2) t

  val apply
    :  ('input, 'output) t
    -> (module With_compare with type t = 'input)
    -> 'input
    -> 'output

  val apply2
    :  ('a, ('b, 'c) t) t
    -> (module With_compare with type t = 'a)
    -> (module With_compare with type t = 'b)
    -> 'a
    -> 'b
    -> 'c

  val apply3
    :  ('a, ('b, ('c, 'd) t) t) t
    -> (module With_compare with type t = 'a)
    -> (module With_compare with type t = 'b)
    -> (module With_compare with type t = 'c)
    -> 'a
    -> 'b
    -> 'c
    -> 'd

  val injective
    :  ('a, 'b) t
    -> (module With_compare with type t = 'a)
    -> (module Adjustable.S with type t = 'b)
    -> 'a list
    -> ('a, 'b) t
end
