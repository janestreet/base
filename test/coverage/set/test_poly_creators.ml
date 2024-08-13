open! Base
module Implementation = Base.Set.Poly

let%expect_test "[Base.Set.Poly] creators/accessors" =
  let open Functor.Test_creators (Instances.Poly) (Implementation) in
  [%expect {| Functor.Test_creators: running tests. |}]
;;

include (Implementation : Functor.Creators with module Types := Instances.Types.Poly)
