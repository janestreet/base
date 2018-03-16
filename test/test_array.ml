open! Import
open! Array

let%test_module "Binary_searchable" =
  (module Test_binary_searchable.Test1
       (struct
         include Array
         module For_test = struct
           let of_array = Fn.id
         end
       end))

let%test_module "Blit" =
  (module Test_blit.Test1
       (struct
         type 'a z = 'a
         include Array
         let create_bool ~len = create ~len false
       end)
       (Array))

let%test_module "Sort" =
  (module struct
    open Private.Sort

    let%test_module "Intro_sort.five_element_sort" =
      (module struct
        (* run [five_element_sort] on all permutations of an array of five elements *)

        let rec sprinkle x xs =
          (x :: xs) :: begin
            match xs with
            | [] -> []
            | x' :: xs' ->
              List.map (sprinkle x xs') ~f:(fun sprinkled -> x' :: sprinkled)
          end

        let rec permutations = function
          | [] -> [[]]
          | x :: xs ->
            List.concat_map (permutations xs) ~f:(fun perms -> sprinkle x perms)

        let all_perms = permutations [1;2;3;4;5]
        let%test _ = List.length all_perms = 120
        let%test _ = not (List.contains_dup ~compare:[%compare: int list] all_perms)

        let%test _ =
          List.for_all all_perms ~f:(fun l ->
            let arr = Array.of_list l in
            Intro_sort.five_element_sort arr ~compare:[%compare: int] 0 1 2 3 4;
            [%compare.equal: int t] arr [|1;2;3;4;5|])
      end)

    module Test (M : Private.Sort.Sort) = struct
      let random_data ~length ~range =
        let arr = Array.create ~len:length 0 in
        for i = 0 to length - 1 do
          arr.(i) <- Random.int range;
        done;
        arr
      ;;

      let assert_sorted arr =
        M.sort arr ~left:0 ~right:(Array.length arr - 1) ~compare:[%compare: int];
        let len = Array.length arr in
        let rec loop i prev =
          if i = len then true
          else if arr.(i) < prev then false
          else loop (i + 1) arr.(i)
        in
        loop 0 (-1)
      ;;

      let%test _ = assert_sorted (random_data ~length:0 ~range:100)
      let%test _ = assert_sorted (random_data ~length:1 ~range:100)
      let%test _ = assert_sorted (random_data ~length:100 ~range:1_000)
      let%test _ = assert_sorted (random_data ~length:1_000 ~range:1)
      let%test _ = assert_sorted (random_data ~length:1_000 ~range:10)
      let%test _ = assert_sorted (random_data ~length:1_000 ~range:1_000_000)
    end

    let%test_module _ = (module Test (Insertion_sort))
    let%test_module _ = (module Test (Heap_sort))
    let%test_module _ = (module Test (Intro_sort))
  end)

let%test _ = is_sorted [||] ~compare:[%compare: int]
let%test _ = is_sorted [|0|] ~compare:[%compare: int]
let%test _ = is_sorted [|0;1;2;2;4|] ~compare:[%compare: int]
let%test _ = not (is_sorted [|0;1;2;3;2|] ~compare:[%compare: int])

let%test_unit _ =
  List.iter
    ~f:(fun (t, expect) ->
      assert (Bool.equal expect (is_sorted_strictly (of_list t) ~compare:[%compare: int])))
    [ []         , true;
      [ 1 ]      , true;
      [ 1; 2 ]   , true;
      [ 1; 1 ]   , false;
      [ 2; 1 ]   , false;
      [ 1; 2; 3 ], true;
      [ 1; 1; 3 ], false;
      [ 1; 2; 2 ], false;
    ]
;;

let%test _ = foldi [||] ~init:13 ~f:(fun _ _ _ -> failwith "bad") = 13
let%test _ = foldi [| 13 |] ~init:17 ~f:(fun i ac x -> ac + i + x) = 30
let%test _ = foldi [| 13; 17 |] ~init:19 ~f:(fun i ac x -> ac + i + x) = 50

let%test _ = counti [|0;1;2;3;4|] ~f:(fun idx x -> idx = x) = 5
let%test _ = counti [|0;1;2;3;4|] ~f:(fun idx x -> idx = 4-x) = 1

let%test_unit _ =
  for i = 0 to 5 do
    let l1 = List.init i ~f:Fn.id in
    let l2 = List.rev (to_list (of_list_rev l1)) in
    assert ([%compare.equal: int list] l1 l2);
  done
;;

let%test_unit _ =
  List.iter
    ~f:(fun (t, len) ->
      assert (Exn.does_raise (fun () -> unsafe_truncate t ~len)))
    [ [| |]  , -1
    ; [| |]  , 0
    ; [| |]  , 1
    ; [| 1 |], -1
    ; [| 1 |], 0
    ; [| 1 |], 2
    ]
;;

let%test_unit _ =
  for orig_len = 1 to 5 do
    for new_len = 1 to orig_len do
      let t = init orig_len ~f:Fn.id in
      unsafe_truncate t ~len:new_len;
      assert (length t = new_len);
      for i = 0 to new_len - 1 do
        assert (t.(i) = i);
      done;
    done;
  done
;;

let (=) = Polymorphic_compare.(=)

let%test _ = filter_opt [|Some 1; None; Some 2; None; Some 3|] = [|1; 2; 3|]
let%test _ = filter_opt [|Some 1; None; Some 2|] = [|1; 2|]
let%test _ = filter_opt [|Some 1|] = [|1|]
let%test _ = filter_opt [|None|] = [||]
let%test _ = filter_opt [||] = [||]

let%test _ = fold2_exn [||] [||] ~init:13 ~f:(fun _ -> failwith "fail") = 13
let%test _ = fold2_exn [| 1 |] [| "1" |] ~init:[] ~f:(fun ac a b -> (a, b) :: ac) = [ 1, "1" ]

let%test _ = filter [| 0; 1 |] ~f:(fun n -> n < 2) = [| 0; 1 |]
let%test _ = filter [| 0; 1 |] ~f:(fun n -> n < 1) = [| 0 |]
let%test _ = filter [| 0; 1 |] ~f:(fun n -> n < 0) = [||]

let%test _ = existsi [||] ~f:(fun _ _ -> true) = false
let%test _ = existsi [|0;1;2;3|] ~f:(fun i x -> i <> x) = false
let%test _ = existsi [|0;1;3;3|] ~f:(fun i x -> i <> x) = true

let%test _ = for_alli [||] ~f:(fun _ _ -> false) = true
let%test _ = for_alli [|0;1;2;3|] ~f:(fun i x -> i = x) = true
let%test _ = for_alli [|0;1;3;3|] ~f:(fun i x -> i = x) = false

let%test _ = exists2_exn [||] [||] ~f:(fun _ _ -> true) = false
let%test _ = exists2_exn [|0;2;4;6|] [|0;2;4;6|] ~f:(fun x y -> x <> y) = false
let%test _ = exists2_exn [|0;2;4;8|] [|0;2;4;6|] ~f:(fun x y -> x <> y) = true
let%test _ = exists2_exn [|2;2;4;6|] [|0;2;4;6|] ~f:(fun x y -> x <> y) = true
let%test _ = for_all2_exn [||] [||] ~f:(fun _ _ -> false) = true
let%test _ = for_all2_exn [|0;2;4;6|] [|0;2;4;6|] ~f:(fun x y -> x = y) = true
let%test _ = for_all2_exn [|0;2;4;8|] [|0;2;4;6|] ~f:(fun x y -> x = y) = false
let%test _ = for_all2_exn [|2;2;4;6|] [|0;2;4;6|] ~f:(fun x y -> x = y) = false

let%test _ = equal [||] [||] ~equal:(=)
let%test _ = equal [| 1 |] [| 1 |] ~equal:(=)
let%test _ = equal [| 1; 2 |] [| 1; 2 |] ~equal:(=)
let%test _ = not (equal [||] [| 1 |] ~equal:(=))
let%test _ = not (equal [| 1 |] [||] ~equal:(=))
let%test _ = not (equal [| 1 |] [| 1; 2 |] ~equal:(=))
let%test _ = not (equal [| 1; 2 |] [| 1; 3 |] ~equal:(=))

let%test _ = find_mapi [|0;5;2;1;4|] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 0
let%test _ = find_mapi [|3;5;2;1;4|] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 4
let%test _ = find_mapi [|3;5;1;1;4|] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 8
let%test _ = find_mapi [|3;5;1;1;2|] ~f:(fun i x -> if i = x then Some (i+x) else None) = None

let%test_unit _ =
  List.iter
    ~f:(fun (l, expect) ->
      let t = of_list l in
      assert (Poly.equal expect (find_consecutive_duplicate t ~equal:Poly.equal)))
    [ []            , None
    ; [ 1 ]         , None
    ; [ 1; 1 ]      , Some (1, 1)
    ; [ 1; 2 ]      , None
    ; [ 1; 2; 1 ]   , None
    ; [ 1; 2; 2 ]   , Some (2, 2)
    ; [ 1; 1; 2; 2 ], Some (1, 1)
    ]
;;

let%test _ = random_element [| |] = None
let%test _ = random_element [| 0 |] = Some 0

let%test_unit _ =
  List.iter
    [ [||]
    ; [| 1 |]
    ; [| 1; 2; 3; 4; 5 |]
    ]
    ~f:(fun t ->
      assert (Sequence.to_array (to_sequence t) = t))
;;

let test_fold_map array ~init ~f ~expect =
  folding_map array ~init ~f = snd expect &&
  fold_map    array ~init ~f = expect

let test_fold_mapi array ~init ~f ~expect =
  folding_mapi array ~init ~f = snd expect &&
  fold_mapi    array ~init ~f = expect

let%test _ = test_fold_map [|1;2;3;4|] ~init:0
               ~f:(fun acc x -> let y = acc+x in y,y)
               ~expect:(10, [|1;3;6;10|])
let%test _ = test_fold_map [||] ~init:0
               ~f:(fun acc x -> let y = acc+x in y,y)
               ~expect:(0, [||])
let%test _ = test_fold_mapi [|1;2;3;4|] ~init:0
               ~f:(fun i acc x -> let y = acc+i*x in y,y)
               ~expect:(20, [|0;2;8;20|])
let%test _ = test_fold_mapi [||] ~init:0
               ~f:(fun i acc x -> let y = acc+i*x in y,y)
               ~expect:(0, [||])
