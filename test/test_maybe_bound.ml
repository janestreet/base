open! Import
open! Maybe_bound

let%test_module "is_lower_bound" =
  (module struct
    let compare = Int.compare

    let%test _ = is_lower_bound Unbounded ~of_:Int.min_value ~compare

    let%test _ = not (is_lower_bound (Incl 2) ~of_:1 ~compare)
    let%test _ =      is_lower_bound (Incl 2) ~of_:2 ~compare
    let%test _ =      is_lower_bound (Incl 2) ~of_:3 ~compare

    let%test _ = not (is_lower_bound (Excl 2) ~of_:1 ~compare)
    let%test _ = not (is_lower_bound (Excl 2) ~of_:2 ~compare)
    let%test _ =      is_lower_bound (Excl 2) ~of_:3 ~compare
  end)

let%test_module "is_upper_bound" =
  (module struct
    let compare = Int.compare

    let%test _ = is_upper_bound Unbounded ~of_:Int.max_value ~compare

    let%test _ =      is_upper_bound (Incl 2) ~of_:1 ~compare
    let%test _ =      is_upper_bound (Incl 2) ~of_:2 ~compare
    let%test _ = not (is_upper_bound (Incl 2) ~of_:3 ~compare)

    let%test _ =      is_upper_bound (Excl 2) ~of_:1 ~compare
    let%test _ = not (is_upper_bound (Excl 2) ~of_:2 ~compare)
    let%test _ = not (is_upper_bound (Excl 2) ~of_:3 ~compare)
  end)

let%test_module "check_range" =
  (module struct
    let compare = Int.compare

    let tests (lower, upper) cases =
      List.iter cases ~f:(fun (n, comparison) ->
        [%test_result: interval_comparison]
          ~expect:comparison
          (compare_to_interval_exn n ~lower ~upper ~compare);
        [%test_result: bool]
          ~expect:(match comparison with In_range -> true | _ -> false)
          (interval_contains_exn n ~lower ~upper ~compare))

    let%test_unit _ =
      tests (Unbounded, Unbounded)
        [ (Int.min_value, In_range)
        ; (0,             In_range)
        ; (Int.max_value, In_range)
        ]

    let%test_unit _ =
      tests (Incl 2, Incl 4)
        [ (1, Below_lower_bound)
        ; (2, In_range)
        ; (3, In_range)
        ; (4, In_range)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Incl 2, Excl 4)
        [ (1, Below_lower_bound)
        ; (2, In_range)
        ; (3, In_range)
        ; (4, Above_upper_bound)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Excl 2, Incl 4)
        [ (1, Below_lower_bound)
        ; (2, Below_lower_bound)
        ; (3, In_range)
        ; (4, In_range)
        ; (5, Above_upper_bound)
        ]

    let%test_unit _ =
      tests (Excl 2, Excl 4)
        [ (1, Below_lower_bound)
        ; (2, Below_lower_bound)
        ; (3, In_range)
        ; (4, Above_upper_bound)
        ; (5, Above_upper_bound)
        ]
  end)
