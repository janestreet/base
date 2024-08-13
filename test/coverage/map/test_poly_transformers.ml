open! Base
module Implementation = Base.Map.Poly

let%expect_test "[Base.Map.Poly] creators/accessors" =
  let open Functor.Test_transformers (Instances.Poly) (Implementation) in
  [%expect {| Functor.Test_transformers: running tests. |}]
;;

include (Implementation : Functor.Transformers with module Types := Instances.Types.Poly)
