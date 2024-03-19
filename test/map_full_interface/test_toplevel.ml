open! Base
include Test_toplevel_intf.Definitions
include (Base.Map : S)

let%expect_test "[Base.Map] creators/accessors" =
  let open
    Functor.Test_creators_and_accessors (Types) (Base.Map)
      (struct
        include Functor.Instance (Int)

        let create f = f ((module Int) : _ Comparator.Module.t)
        let access x = x
      end) in
  [%expect {| |}]
;;
