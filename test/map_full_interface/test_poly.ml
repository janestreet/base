open! Base
include Test_poly_intf.Definitions
include (Base.Map.Poly : S)

let%expect_test "[Base.Map.Poly] creators/accessors" =
  let open
    Functor.Test_creators_and_accessors (Types) (Base.Map.Poly)
      (struct
        include Functor.Instance (Comparator.Poly)

        let create x = x
        let access x = x
      end) in
  [%expect {| |}]
;;
