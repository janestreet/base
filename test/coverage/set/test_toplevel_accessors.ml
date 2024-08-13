open! Base
module Implementation = Base.Set

let%expect_test "[Base.Set] creators/accessors" =
  let open Functor.Test_accessors (Instances.Toplevel) (Implementation) in
  [%expect {| Functor.Test_accessors: running tests. |}]
;;

include (Implementation : Functor.Accessors with module Types := Instances.Types.Toplevel)
