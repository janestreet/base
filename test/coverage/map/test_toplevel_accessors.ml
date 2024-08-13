open! Base
module Implementation = Base.Map

let%expect_test "[Base.Map] creators/accessors" =
  let open Functor.Test_accessors (Instances.Toplevel) (Implementation) in
  [%expect {| Functor.Test_accessors: running tests. |}]
;;

include (Implementation : Functor.Accessors with module Types := Instances.Types.Toplevel)
