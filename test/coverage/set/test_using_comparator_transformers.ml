open! Base
module Implementation = Base.Set.Using_comparator

let%expect_test "[Base.Set.Using_comparator] creators/accessors" =
  let open Functor.Test_transformers (Instances.Using_comparator) (Implementation) in
  [%expect {| Functor.Test_transformers: running tests. |}]
;;

include (
  Implementation :
    Functor.Transformers with module Types := Instances.Types.Using_comparator)
