open! Base
module Implementation = Base.Map.Using_comparator

let%expect_test "[Base.Map.Using_comparator] creators/accessors" =
  let open Functor.Test_accessors (Instances.Using_comparator) (Implementation) in
  [%expect {| Functor.Test_accessors: running tests. |}]
;;

include (
  Implementation : Functor.Accessors with module Types := Instances.Types.Using_comparator)
