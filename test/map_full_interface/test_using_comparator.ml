open! Base
include Test_using_comparator_intf.Definitions
include (Base.Map.Using_comparator : S)

let%expect_test "[Base.Map.Using_comparator] creators/accessors" =
  let open
    Functor.Test_creators_and_accessors (Types) (Base.Map.Using_comparator)
      (struct
        include Functor.Instance (Int)

        let create f = f ~comparator:Int.comparator
        let access x = x
      end) in
  [%expect {| |}]
;;
