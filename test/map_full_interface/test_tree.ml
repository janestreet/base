open! Base
include Test_tree_intf.Definitions
include (Base.Map.Using_comparator.Tree : S)

let%expect_test "[Base.Map.Using_comparator.Tree] creators/accessors" =
  let open
    Functor.Test_creators_and_accessors (Types) (Base.Map.Using_comparator.Tree)
      (struct
        include Functor.Instance_tree (Int)

        let create f = f ~comparator:Int.comparator
        let access f = f ~comparator:Int.comparator
      end) in
  [%expect {| |}]
;;
