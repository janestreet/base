open! Base
module Implementation = Base.Set.Using_comparator.Tree

let%expect_test "[Base.Set.Using_comparator.Tree] creators/accessors" =
  let open Functor.Test_creators (Instances.Tree) (Implementation) in
  [%expect {| Functor.Test_creators: running tests. |}]
;;

include (Implementation : Functor.Creators with module Types := Instances.Types.Tree)
