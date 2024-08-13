open! Base
module Implementation = Base.Set.Poly

let%expect_test "[Base.Set.Poly] creators/accessors" =
  let open Functor.Test_transformers (Instances.Poly) (Implementation) in
  [%expect {| Functor.Test_transformers: running tests. |}]
;;

include (Implementation : Functor.Transformers with module Types := Instances.Types.Poly)
