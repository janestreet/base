open! Base
module Implementation = Base.Map

let%expect_test "[Base.Map] creators/accessors" =
  let open Functor.Test_creators (Instances.Toplevel) (Implementation) in
  [%expect {| Functor.Test_creators: running tests. |}]
;;

include (Implementation : Functor.Creators with module Types := Instances.Types.Toplevel)
