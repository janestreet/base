open! Base
module Implementation = Base.Map.Poly

let%expect_test "[Base.Map.Poly] creators/accessors" =
  let open Functor.Test_creators (Instances.Poly) (Implementation) in
  [%expect {| Functor.Test_creators: running tests. |}]
;;

include (Implementation : Functor.Creators with module Types := Instances.Types.Poly)
