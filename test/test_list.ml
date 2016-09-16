open! Import
open! List

let%test_module "reduce_balanced" = (module struct
  let test expect list =
    [%test_result: string option] ~expect
      (reduce_balanced ~f:(fun a b -> "(" ^ a  ^ "+" ^ b ^ ")") list)

  let%test_unit "length 0" =
    test None []

  let%test_unit "length 1" =
    test (Some "a") ["a"]

  let%test_unit "length 2" =
    test (Some "(a+b)") ["a"; "b"]

  let%test_unit "length 6" =
    test (Some "(((a+b)+(c+d))+(e+f))") ["a";"b";"c";"d";"e";"f"]

  let%test_unit "longer" =
    (* pairs (index, number of times f called on me) to check:
       1. f called on results in index order
       2. total number of calls on any element is low
       called on 2^n + 1 to demonstrate lack of balance (most elements are distance 7 from
       the tree root, but one is distance 1) *)
    let data = map (range 0 65) ~f:(fun i -> [(i, 0)]) in
    let f x y = map (x @ y) ~f:(fun (ix, cx) -> (ix, cx + 1)) in
    match reduce_balanced data ~f with
    | None -> failwith "None"
    | Some l ->
      [%test_result: int] ~expect:65 (List.length l);
      iteri l ~f:(fun actual_index (computed_index, num_f) ->
        let expected_num_f = if actual_index = 64 then 1 else 7 in
        [%test_result: int * int]
          ~expect:(actual_index, expected_num_f) (computed_index, num_f))
end)
