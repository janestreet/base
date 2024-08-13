open! Base
module Implementation = Base.Map.Poly

let%expect_test "[Base.Map.Poly] creators/accessors" =
  let open Functor.Test_accessors (Instances.Poly) (Implementation) in
  [%expect {| Functor.Test_accessors: running tests. |}]
;;

include (Implementation : Functor.Accessors with module Types := Instances.Types.Poly)
