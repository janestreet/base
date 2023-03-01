(* [Int0] defines integer functions that are primitives or can be simply
   defined in terms of [Stdlib]. [Int0] is intended to completely express the
   part of [Stdlib] that [Base] uses for integers -- no other file in Base other
   than int0.ml should use these functions directly through [Stdlib]. [Int0] has
   few dependencies, and so is available early in Base's build order.

   All Base files that need to use ints and come before [Base.Int] in build
   order should do:

   {[
     module Int  = Int0
   ]}

   Defining [module Int = Int0] is also necessary because it prevents ocamldep
   from mistakenly causing a file to depend on [Base.Int]. *)

let to_string = Stdlib.string_of_int
let of_string = Stdlib.int_of_string
let of_string_opt = Stdlib.int_of_string_opt
let to_float = Stdlib.float_of_int
let of_float = Stdlib.int_of_float
let max_value = Stdlib.max_int
let min_value = Stdlib.min_int
let succ = Stdlib.succ
let pred = Stdlib.pred
