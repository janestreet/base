open! Import
open! List

let%test_module "reduce_balanced" =
  (module struct
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

let%test_module "range symmetries" =
  (module struct

    let basic ~stride ~start ~stop ~start_n ~stop_n ~result =
      [%compare.equal: int t] (range ~stride ~start ~stop start_n stop_n) result

    let test stride (start_n, start) (stop_n, stop) result =
      basic ~stride ~start ~stop ~start_n ~stop_n ~result
      && (* works for negative [start] and [stop] *)
      basic ~stride:(-stride)
        ~start_n:(-start_n)
        ~stop_n:(-stop_n)
        ~start
        ~stop
        ~result:(List.map result ~f:(fun x -> -x))

    let%test _ = test 1    ( 3, `inclusive) ( 1, `exclusive) []
    let%test _ = test 1    ( 3, `inclusive) ( 3, `exclusive) []
    let%test _ = test 1    ( 3, `inclusive) ( 4, `exclusive) [3]
    let%test _ = test 1    ( 3, `inclusive) ( 8, `exclusive) [3;4;5;6;7]
    let%test _ = test 3    ( 4, `inclusive) (10, `exclusive) [4;7]
    let%test _ = test 3    ( 4, `inclusive) (11, `exclusive) [4;7;10]
    let%test _ = test 3    ( 4, `inclusive) (12, `exclusive) [4;7;10]
    let%test _ = test 3    ( 4, `inclusive) (13, `exclusive) [4;7;10]
    let%test _ = test 3    ( 4, `inclusive) (14, `exclusive) [4;7;10;13]

    let%test _ = test (-1) ( 1, `inclusive) ( 3, `exclusive) []
    let%test _ = test (-1) ( 3, `inclusive) ( 3, `exclusive) []
    let%test _ = test (-1) ( 4, `inclusive) ( 3, `exclusive) [4]
    let%test _ = test (-1) ( 8, `inclusive) ( 3, `exclusive) [8;7;6;5;4]
    let%test _ = test (-3) (10, `inclusive) ( 4, `exclusive) [10;7]
    let%test _ = test (-3) (10, `inclusive) ( 3, `exclusive) [10;7;4]
    let%test _ = test (-3) (10, `inclusive) ( 2, `exclusive) [10;7;4]
    let%test _ = test (-3) (10, `inclusive) ( 1, `exclusive) [10;7;4]
    let%test _ = test (-3) (10, `inclusive) ( 0, `exclusive) [10;7;4;1]

    let%test _ = test 1    ( 3, `exclusive) ( 1, `exclusive) []
    let%test _ = test 1    ( 3, `exclusive) ( 3, `exclusive) []
    let%test _ = test 1    ( 3, `exclusive) ( 4, `exclusive) []
    let%test _ = test 1    ( 3, `exclusive) ( 8, `exclusive) [4;5;6;7]
    let%test _ = test 3    ( 4, `exclusive) (10, `exclusive) [7]
    let%test _ = test 3    ( 4, `exclusive) (11, `exclusive) [7;10]
    let%test _ = test 3    ( 4, `exclusive) (12, `exclusive) [7;10]
    let%test _ = test 3    ( 4, `exclusive) (13, `exclusive) [7;10]
    let%test _ = test 3    ( 4, `exclusive) (14, `exclusive) [7;10;13]

    let%test _ = test (-1) ( 1, `exclusive) ( 3, `exclusive) []
    let%test _ = test (-1) ( 3, `exclusive) ( 3, `exclusive) []
    let%test _ = test (-1) ( 4, `exclusive) ( 3, `exclusive) []
    let%test _ = test (-1) ( 8, `exclusive) ( 3, `exclusive) [7;6;5;4]
    let%test _ = test (-3) (10, `exclusive) ( 4, `exclusive) [7]
    let%test _ = test (-3) (10, `exclusive) ( 3, `exclusive) [7;4]
    let%test _ = test (-3) (10, `exclusive) ( 2, `exclusive) [7;4]
    let%test _ = test (-3) (10, `exclusive) ( 1, `exclusive) [7;4]
    let%test _ = test (-3) (10, `exclusive) ( 0, `exclusive) [7;4;1]

    let%test _ = test 1    ( 3, `inclusive) ( 1, `inclusive) []
    let%test _ = test 1    ( 3, `inclusive) ( 3, `inclusive) [3]
    let%test _ = test 1    ( 3, `inclusive) ( 4, `inclusive) [3;4]
    let%test _ = test 1    ( 3, `inclusive) ( 8, `inclusive) [3;4;5;6;7;8]
    let%test _ = test 3    ( 4, `inclusive) (10, `inclusive) [4;7;10]
    let%test _ = test 3    ( 4, `inclusive) (11, `inclusive) [4;7;10]
    let%test _ = test 3    ( 4, `inclusive) (12, `inclusive) [4;7;10]
    let%test _ = test 3    ( 4, `inclusive) (13, `inclusive) [4;7;10;13]
    let%test _ = test 3    ( 4, `inclusive) (14, `inclusive) [4;7;10;13]

    let%test _ = test (-1) ( 1, `inclusive) ( 3, `inclusive) []
    let%test _ = test (-1) ( 3, `inclusive) ( 3, `inclusive) [3]
    let%test _ = test (-1) ( 4, `inclusive) ( 3, `inclusive) [4;3]
    let%test _ = test (-1) ( 8, `inclusive) ( 3, `inclusive) [8;7;6;5;4;3]
    let%test _ = test (-3) (10, `inclusive) ( 4, `inclusive) [10;7;4]
    let%test _ = test (-3) (10, `inclusive) ( 3, `inclusive) [10;7;4]
    let%test _ = test (-3) (10, `inclusive) ( 2, `inclusive) [10;7;4]
    let%test _ = test (-3) (10, `inclusive) ( 1, `inclusive) [10;7;4;1]
    let%test _ = test (-3) (10, `inclusive) ( 0, `inclusive) [10;7;4;1]

    let%test _ = test 1    ( 3, `exclusive) ( 1, `inclusive) []
    let%test _ = test 1    ( 3, `exclusive) ( 3, `inclusive) []
    let%test _ = test 1    ( 3, `exclusive) ( 4, `inclusive) [4]
    let%test _ = test 1    ( 3, `exclusive) ( 8, `inclusive) [4;5;6;7;8]
    let%test _ = test 3    ( 4, `exclusive) (10, `inclusive) [7;10]
    let%test _ = test 3    ( 4, `exclusive) (11, `inclusive) [7;10]
    let%test _ = test 3    ( 4, `exclusive) (12, `inclusive) [7;10]
    let%test _ = test 3    ( 4, `exclusive) (13, `inclusive) [7;10;13]
    let%test _ = test 3    ( 4, `exclusive) (14, `inclusive) [7;10;13]

    let%test _ = test (-1) ( 1, `exclusive) ( 3, `inclusive) []
    let%test _ = test (-1) ( 3, `exclusive) ( 3, `inclusive) []
    let%test _ = test (-1) ( 4, `exclusive) ( 3, `inclusive) [3]
    let%test _ = test (-1) ( 8, `exclusive) ( 3, `inclusive) [7;6;5;4;3]
    let%test _ = test (-3) (10, `exclusive) ( 4, `inclusive) [7;4]
    let%test _ = test (-3) (10, `exclusive) ( 3, `inclusive) [7;4]
    let%test _ = test (-3) (10, `exclusive) ( 2, `inclusive) [7;4]
    let%test _ = test (-3) (10, `exclusive) ( 1, `inclusive) [7;4;1]
    let%test _ = test (-3) (10, `exclusive) ( 0, `inclusive) [7;4;1]

    let test_start_inc_exc stride start (stop, stop_inc_exc) result =
      test stride (start, `inclusive) (stop, stop_inc_exc) result
      && begin
        match result with
        | [] -> true
        | head :: tail ->
          head = start && test stride (start, `exclusive) (stop, stop_inc_exc) tail
      end

    let test_inc_exc stride start stop result =
      test_start_inc_exc stride start (stop, `inclusive) result
      && begin
        match List.rev result with
        | [] -> true
        | last :: all_but_last ->
          let all_but_last = List.rev all_but_last in
          if last = stop then
            test_start_inc_exc stride start (stop, `exclusive) all_but_last
          else
            true
      end

    let%test _ = test_inc_exc 1 4 10 [4;5;6;7;8;9;10]
    let%test _ = test_inc_exc 3 4 10 [4;7;10]
    let%test _ = test_inc_exc 3 4 11 [4;7;10]
    let%test _ = test_inc_exc 3 4 12 [4;7;10]
    let%test _ = test_inc_exc 3 4 13 [4;7;10;13]
    let%test _ = test_inc_exc 3 4 14 [4;7;10;13]

  end)

module Test_values = struct
  let long1 =
    let v = lazy (range 1 100_000) in
    fun () -> Lazy.force v

  let long2 =
    let v = lazy (range 2 100_001) in
    fun () -> Lazy.force v

  let l1 = [1;2;3;4;5;6;7;8;9;10]
end

let (=) = Polymorphic_compare.(=)
let (<>) = Polymorphic_compare.(<>)
let compare = Polymorphic_compare.compare

let%test _ = rev_append [1;2;3] [4;5;6] = [3;2;1;4;5;6]
let%test _ = rev_append [] [4;5;6] = [4;5;6]
let%test _ = rev_append [1;2;3] [] = [3;2;1]
let%test _ = rev_append [1] [2;3] = [1;2;3]
let%test _ = rev_append [1;2] [3] = [2;1;3]
let%test _ =
  let long = Test_values.long1 () in
  ignore (rev_append long long:int list);
  true

let%test _ =
  let long1 = Test_values.long1 () in
  let long2 = Test_values.long2 () in
  (map long1 ~f:(fun x -> x + 1)) = long2

let test_ordering n =
  let l = List.range 0 n in
  let r = ref [] in
  let _ : unit list = List.map l ~f:(fun x -> r := x :: !r) in
  [%test_eq: int list] l (List.rev !r)

let%test_unit _ = test_ordering 10
let%test_unit _ = test_ordering 1000
let%test_unit _ = test_ordering 1_000_000

let%test _ = for_all2_exn [] [] ~f:(fun _ _ -> assert false)

let%test _ = find_mapi [0;5;2;1;4] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 0
let%test _ = find_mapi [3;5;2;1;4] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 4
let%test _ = find_mapi [3;5;1;1;4] ~f:(fun i x -> if i = x then Some (i+x) else None) = Some 8
let%test _ = find_mapi [3;5;1;1;2] ~f:(fun i x -> if i = x then Some (i+x) else None) = None

let%test _ = for_alli [] ~f:(fun _ _ -> false) = true
let%test _ = for_alli [0;1;2;3] ~f:(fun i x -> i = x) = true
let%test _ = for_alli [0;1;3;3] ~f:(fun i x -> i = x) = false
let%test _ = existsi [] ~f:(fun _ _ -> true) = false
let%test _ = existsi [0;1;2;3] ~f:(fun i x -> i <> x) = false
let%test _ = existsi [0;1;3;3] ~f:(fun i x -> i <> x) = true

let%test _ = append [1;2;3] [4;5;6] = [1;2;3;4;5;6]
let%test _ = append [] [4;5;6] = [4;5;6]
let%test _ = append [1;2;3] [] = [1;2;3]
let%test _ = append [1] [2;3] = [1;2;3]
let%test _ = append [1;2] [3] = [1;2;3]
let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (append long long:int list)

let%test _ = map ~f:(fun x -> x) Test_values.l1 = Test_values.l1
let%test _ = map ~f:(fun x -> x) [] = []
let%test _ = map ~f:(fun x -> x +. 5.) [1.;2.;3.] = [6.;7.;8.]
let%test_unit _ =
  ignore (map ~f:(fun x -> x) (Test_values.long1 ()):int list)

let%test _ = map2_exn ~f:(fun a b -> a, b) [1;2;3] ['a';'b';'c']
             = [(1,'a'); (2,'b'); (3,'c')]
let%test _ = map2_exn ~f:(fun _ _ -> ()) [] [] = []
let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (map2_exn ~f:(fun _ _ -> ()) long long:unit list)

let%test _ = rev_map_append [1;2;3;4;5] [6] ~f:(fun x -> x) = [5;4;3;2;1;6]
let%test _ = rev_map_append [1;2;3;4;5] [6] ~f:(fun x -> 2 * x) = [10;8;6;4;2;6]
let%test _ = rev_map_append [] [6] ~f:(fun _ -> failwith "bug!") = [6]

let%test _ = fold_right ~f:(fun e acc -> e :: acc) Test_values.l1 ~init:[] =
             Test_values.l1
let%test _ = fold_right ~f:(fun e acc -> e ^ acc) ["1";"2"] ~init:"3" = "123"
let%test _ = fold_right ~f:(fun _ _ -> ()) [] ~init:() = ()
let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (fold_right ~f:(fun e acc -> e :: acc) long ~init:[])

let%test  _ =
  let l1 = Test_values.l1 in
  unzip (zip_exn l1 (List.rev l1)) = (l1, List.rev l1)
;;

let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (unzip (zip_exn long long))
;;

let%test _ = unzip  [(1,2)  ; (4,5)  ] = ([1; 4], [2; 5]        )
let%test _ = unzip3 [(1,2,3); (4,5,6)] = ([1; 4], [2; 5], [3; 6])

let%test _ = zip [1;2;3] [4;5;6] = Some [1,4;2,5;3,6]
let%test _ = zip [1] [4;5;6]     = None

let%test _ = mapi ~f:(fun i x -> (i,x))
               ["one";"two";"three";"four"] = [0,"one";1,"two";2,"three";3,"four"]
let%test _ = mapi ~f:(fun i x -> (i,x)) [] = []

let%test_module "group" =
  (module struct
    let%test _ = (group [1;2;3;4] ~break:(fun _ x -> x = 3) = [[1;2];[3;4]])

    let%test _ = (group [] ~break:(fun _ -> assert false)) = []

    let mis = ['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']
    let equal_letters =
      [['M'];['i'];['s';'s'];['i'];['s';'s'];['i'];['p';'p'];['i']]
    let single_letters =
      [['M';'i';'s';'s';'i';'s';'s';'i';'p';'p';'i']]
    let every_three =
      [['M'; 'i'; 's']; ['s'; 'i'; 's']; ['s'; 'i'; 'p']; ['p'; 'i' ]]

    let%test _ = (group ~break:(<>) mis) = equal_letters
    let%test _ = (group ~break:(fun _ _ -> false) mis) = single_letters
    let%test _ = (groupi ~break:(fun i _ _ -> i % 3 = 0) mis) = every_three
  end)

let%test_module "chunks_of" =
  (module struct

    let test length break_every =
      let l = List.init length ~f:Fn.id in
      let b = chunks_of l ~length:break_every in
      [%test_eq: int list] (List.concat b) l;
      List.iter b ~f:([%test_pred: int list] (fun batch ->
        List.length batch <= break_every));
    ;;

    let expect_exn length break_every =
      match test length break_every with
      | exception _ -> ()
      | () -> raise_s [%message "Didn't raise." (length : int) (break_every : int)]
    ;;

    let%test_unit _ =
      for n = 0 to 10 do
        for k = n + 2 downto 1 do
          test n k
        done
      done;
      expect_exn 1 0;
      expect_exn 1 (-1);
    ;;

    let%test _ = chunks_of [] ~length:1 = []
  end)

let%test _ = last_exn [1;2;3] = 3
let%test _ = last_exn [1] = 1
let%test _ = last_exn (Test_values.long1 ()) = 99_999

let%test _ = is_prefix []  ~prefix:[]  ~equal:(=)
let%test _ = is_prefix [1] ~prefix:[]  ~equal:(=)
let%test _ = is_prefix [1] ~prefix:[1] ~equal:(=)
let%test _ = not (is_prefix [1] ~prefix:[1;2] ~equal:(=))
let%test _ = not (is_prefix [1;3] ~prefix:[1;2] ~equal:(=))
let%test _ = is_prefix [1;2;3] ~prefix:[1;2] ~equal:(=)

let%test_unit _ =
  List.iter ~f:(fun (t, expect) ->
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

let%test _ = find_consecutive_duplicate [(0,'a');(1,'b');(2,'b')]
               ~equal:(fun (_, a) (_, b) -> Char.(=) a b) = Some ((1, 'b'), (2, 'b'))
;;

let%test _ = remove_consecutive_duplicates ~which_to_keep:`Last []
               ~equal:Int.(=)                                 = []
let%test _ = remove_consecutive_duplicates ~which_to_keep:`Last [5;5;5;5;5]
               ~equal:Int.(=)                                 = [5]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`Last [5;6;5;6;5;6]
               ~equal:Int.(=)                                 = [5;6;5;6;5;6]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`Last [5;5;6;6;5;5;8;8]
               ~equal:Int.(=)                                 = [5;6;5;8]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`Last [(0,1);(0,2);(2,2);(4,1)]
               ~equal:(fun (a,_) (b,_) -> Int.(=) a b)        = [      (0,2);(2,2);(4,1)]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`Last [(0,1);(2,2);(0,2);(4,1)]
               ~equal:(fun (a,_) (b,_) -> Int.(=) a b)        = [(0,1);(2,2);(0,2);(4,1)]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`Last [(0,1);(2,1);(0,2);(4,2)]
               ~equal:(fun (_,a) (_,b) -> Int.(=) a b)        = [      (2,1);      (4,2)]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`Last [(0,1);(2,2);(0,2);(4,1)]
               ~equal:(fun (_,a) (_,b) -> Int.(=) a b)        = [(0,1);      (0,2);(4,1)]

let%test _ = remove_consecutive_duplicates ~which_to_keep:`First []
               ~equal:Int.(=)                                  = []
let%test _ = remove_consecutive_duplicates ~which_to_keep:`First [5;5;5;5;5]
               ~equal:Int.(=)                                  = [5]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`First [5;6;5;6;5;6]
               ~equal:Int.(=)                                  = [5;6;5;6;5;6]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`First [5;5;6;6;5;5;8;8]
               ~equal:Int.(=)                                  = [5;6;5;8]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`First [(0,1);(0,2);(2,2);(4,1)]
               ~equal:(fun (a,_) (b,_) -> Int.(=) a b)         = [(0,1);      (2,2);(4,1)]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`First [(0,1);(2,2);(0,2);(4,1)]
               ~equal:(fun (a,_) (b,_) -> Int.(=) a b)         = [(0,1);(2,2);(0,2);(4,1)]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`First [(0,1);(2,1);(0,2);(4,2)]
               ~equal:(fun (_,a) (_,b) -> Int.(=) a b)         = [(0,1);      (0,2);     ]
let%test _ = remove_consecutive_duplicates ~which_to_keep:`First [(0,1);(2,2);(0,2);(4,1)]
               ~equal:(fun (_,a) (_,b) -> Int.(=) a b)         = [(0,1);(2,2);      (4,1)]

let%test _ = dedup_and_sort ~compare [] = []
let%test _ = dedup_and_sort ~compare [5;5;5;5;5] = [5]
let%test _ = length (dedup_and_sort ~compare [2;1;5;3;4]) = 5
let%test _ = length (dedup_and_sort ~compare [2;3;5;3;4]) = 4
let%test _ = length (dedup_and_sort [(0,1);(2,2);(0,2);(4,1)] ~compare:(fun (a,_) (b,_) ->
  Int.compare a b)) = 3
let%test _ = length (dedup_and_sort [(0,1);(2,2);(0,2);(4,1)] ~compare:(fun (_,a) (_,b) ->
  Int.compare a b)) = 2

let%test _ = find_a_dup ~compare [] = None
let%test _ = find_a_dup ~compare [3] = None
let%test _ = find_a_dup ~compare [3;4] = None
let%test _ = find_a_dup ~compare [3;3] = Some 3
let%test _ = find_a_dup ~compare [3;5;4;6;12] = None
let%test _ = find_a_dup ~compare [3;5;4;5;12] = Some 5
let%test _ = find_a_dup ~compare [3;5;12;5;12] = Some 5
let%test _ = find_a_dup ~compare:Poly.compare [(0,1);(2,2);(0,2);(4,1)] = None
let%test _ = (find_a_dup [(0,1);(2,2);(0,2);(4,1)]
                ~compare:(fun (_,a) (_,b) -> Int.compare a b)) <> None
let%test _ = let dup = find_a_dup [(0,1);(2,2);(0,2);(4,1)]
                         ~compare:(fun (a,_) (b,_) -> Int.compare a b)
  in
  match dup with
  | Some (0, _) -> true
  | _ -> false

let%test _ = contains_dup ~compare [] = false
let%test _ = contains_dup ~compare [3] = false
let%test _ = contains_dup ~compare [3;4] = false
let%test _ = contains_dup ~compare [3;3] = true
let%test _ = contains_dup ~compare [3;5;4;6;12] = false
let%test _ = contains_dup ~compare [3;5;4;5;12] = true
let%test _ = contains_dup ~compare [3;5;12;5;12] = true
let%test _ = contains_dup ~compare:Poly.compare [(0,1);(2,2);(0,2);(4,1)] = false
let%test _ = (contains_dup [(0,1);(2,2);(0,2);(4,1)]
                ~compare:(fun (_,a) (_,b) -> Int.compare a b)) = true

let%test _ = find_all_dups ~compare [] = []
let%test _ = find_all_dups ~compare [3] = []
let%test _ = find_all_dups ~compare [3;4] = []
let%test _ = find_all_dups ~compare [3;3] = [3]
let%test _ = find_all_dups ~compare [3;5;4;6;12] = []
let%test _ = find_all_dups ~compare [3;5;4;5;12] = [5]
let%test _ = find_all_dups ~compare [3;5;12;5;12] = [5;12]
let%test _ = find_all_dups ~compare:Poly.compare [(0,1);(2,2);(0,2);(4,1)] = []
let%test _ = length (find_all_dups [(0,1);(2,2);(0,2);(4,1)]
                       ~compare:(fun (_,a) (_,b) -> Int.compare a b)) = 2
let%test _ = length (find_all_dups [(0,1);(2,2);(0,2);(4,1)]
                       ~compare:(fun (a,_) (b,_) -> Int.compare a b)) = 1

let%test _ = counti [0;1;2;3;4] ~f:(fun idx x -> idx = x) = 5
let%test _ = counti [0;1;2;3;4] ~f:(fun idx x -> idx = 4-x) = 1

let%test _ = filter_map ~f:(fun x -> Some x) Test_values.l1 = Test_values.l1
let%test _ = filter_map ~f:(fun x -> Some x) [] = []
let%test _ = filter_map ~f:(fun _x -> None) [1.;2.;3.] = []
let%test _ = filter_map
               ~f:(fun x -> if (x > 0) then Some x else None) [1;-1;3] = [1;3]

let%test _ = filter_mapi ~f:(fun _i x -> Some x) Test_values.l1 = Test_values.l1
let%test _ = filter_mapi ~f:(fun _i x -> Some x) [] = []
let%test _ = filter_mapi ~f:(fun _i _x -> None) [1.;2.;3.] = []
let%test _ = filter_mapi ~f:(fun _i x -> if (x > 0) then Some x else None) [1;-1;3]
             = [1;3]
let%test _ = filter_mapi ~f:(fun i x -> if (i % 2=0) then Some x else None)
               [1;-1;3] = [1;3]

let%test _ = split_n [1;2;3;4;5;6] 3 = ([1;2;3],[4;5;6])
let%test _ = split_n [1;2;3;4;5;6] 100 = ([1;2;3;4;5;6],[])
let%test _ = split_n [1;2;3;4;5;6] 0 = ([],[1;2;3;4;5;6])
let%test _ = split_n [1;2;3;4;5;6] (-5) = ([],[1;2;3;4;5;6])

let%test_module "{take,drop,split}_while" =
  (module struct

    let pred = function
      | '0' .. '9' -> true
      | _ -> false

    let test xs prefix suffix =
      let (prefix1, suffix1) = split_while ~f:pred xs in
      let prefix2 = take_while xs ~f:pred in
      let suffix2 = drop_while xs ~f:pred in
      xs = prefix @ suffix
      && prefix = prefix1 && prefix = prefix2
      && suffix = suffix1 && suffix = suffix2

    let%test _ = test ['1';'2';'3';'a';'b';'c'] ['1';'2';'3'] ['a';'b';'c']
    let%test _ = test ['1';'2';    'a';'b';'c'] ['1';'2'    ] ['a';'b';'c']
    let%test _ = test ['1';        'a';'b';'c'] ['1'        ] ['a';'b';'c']
    let%test _ = test [            'a';'b';'c'] [           ] ['a';'b';'c']
    let%test _ = test ['1';'2';'3'            ] ['1';'2';'3'] [           ]
    let%test _ = test [                       ] [           ] [           ]

  end)

let%test _ = concat [] = []
let%test _ = concat [[]] = []
let%test _ = concat [[3]] = [3]
let%test _ = concat [[1;2;3;4]] = [1;2;3;4]
let%test _ = concat
               [[1;2;3;4];[5;6;7];[8;9;10];[];[11;12]]
             = [1;2;3;4;5;6;7;8;9;10;11;12]

let%test _ = is_sorted [] ~compare
let%test _ = is_sorted [1] ~compare
let%test _ = is_sorted [1; 2; 3; 4] ~compare
let%test _ = not (is_sorted [2; 1] ~compare)
let%test _ = not (is_sorted [1; 3; 2] ~compare)

let%test_unit _ =
  List.iter
    ~f:(fun (t, expect) -> assert (expect = is_sorted_strictly t ~compare))
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

let%test _ = random_element [] = None
let%test _ = random_element [0] = Some 0

let%test_module "transpose" =
  (module struct

    let round_trip a b = transpose a = Some b && transpose b = Some a

    let%test _ = round_trip [] []

    let%test _ = transpose [[]] = Some []
    let%test _ = transpose [[]; []] = Some []
    let%test _ = transpose [[]; []; []] = Some []

    let%test _ = round_trip [[1]] [[1]]

    let%test _ = round_trip [[1];
                             [2]] [[1; 2]]

    let%test _ = round_trip [[1];
                             [2];
                             [3]] [[1; 2; 3]]

    let%test _ = round_trip [[1; 2];
                             [3; 4]] [[1; 3];
                                      [2; 4]]

    let%test _ = round_trip [[1; 2; 3];
                             [4; 5; 6]] [[1; 4];
                                         [2; 5];
                                         [3; 6]]

    let%test _ = transpose [[]; [1]] = None

    let%test _ = transpose [[1;2];[3]] = None

  end)

let%test _ = intersperse [1;2;3] ~sep:0 = [1;0;2;0;3]
let%test _ = intersperse [1;2]   ~sep:0 = [1;0;2]
let%test _ = intersperse [1]     ~sep:0 = [1]
let%test _ = intersperse []      ~sep:0 = []

let test_fold_map list ~init ~f ~expect =
  folding_map list ~init ~f = snd expect &&
  fold_map    list ~init ~f = expect

let test_fold_mapi list ~init ~f ~expect =
  folding_mapi list ~init ~f = snd expect &&
  fold_mapi    list ~init ~f = expect

let%test _ = test_fold_map [1;2;3;4] ~init:0
               ~f:(fun acc x -> let y = acc+x in y,y)
               ~expect:(10, [1;3;6;10])
let%test _ = test_fold_map [] ~init:0
               ~f:(fun acc x -> let y = acc+x in y,y)
               ~expect:(0, [])
let%test _ = test_fold_mapi [1;2;3;4] ~init:0
               ~f:(fun i acc x -> let y = acc+i*x in y,y)
               ~expect:(20, [0;2;8;20])
let%test _ = test_fold_mapi [] ~init:0
               ~f:(fun i acc x -> let y = acc+i*x in y,y)
               ~expect:(0, [])
