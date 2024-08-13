open! Base
module Implementation = Base.Set.Using_comparator

let%expect_test "[Base.Set.Using_comparator] creators/accessors" =
  let open Functor.Test_creators (Instances.Using_comparator) (Implementation) in
  [%expect {| Functor.Test_creators: running tests. |}]
;;

include (
  Implementation : Functor.Creators with module Types := Instances.Types.Using_comparator)
