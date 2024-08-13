open! Base
module Implementation = Base.Set.Using_comparator.Tree

let%expect_test "[Base.Set.Using_comparator.Tree] creators/accessors" =
  let open Functor.Test_transformers (Instances.Tree) (Implementation) in
  [%expect {| Functor.Test_transformers: running tests. |}]
;;

include (Implementation : Functor.Transformers with module Types := Instances.Types.Tree)
