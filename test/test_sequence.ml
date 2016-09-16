open! Import
open! Sequence

let%test_unit "of_lazy" =
  let t = range 0 100 in
  [%test_result: int list]
    (to_list (of_lazy (lazy t)))
    ~expect:(to_list t)

let%test_unit _ =
  let seq_of_seqs =
    unfold ~init:0 ~f:(fun i ->
      Some (unfold ~init:i ~f:(fun j -> Some ((i, j), j + 1)),
            i + 1))
  in
  [%test_result: (int * int) list]
    (to_list (take (interleave seq_of_seqs) 10))
    ~expect:[ 0,0
            ; 0,1 ; 1,1
            ; 0,2 ; 1,2 ; 2,2
            ; 0,3 ; 1,3 ; 2,3 ; 3,3
            ]

let%test_unit _ =
  let evens = unfold ~init:0 ~f:(fun i -> Some (i, i + 2)) in
  let vowels = cycle_list_exn ['a';'e';'i';'o';'u'] in
  [%test_result: (int * char) list]
    (to_list (take (interleaved_cartesian_product evens vowels) 10))
    ~expect:[ 0,'a'
            ; 0,'e' ; 2,'a'
            ; 0,'i' ; 2,'e' ; 4,'a'
            ; 0,'o' ; 2,'i' ; 4,'e' ; 6,'a'
            ]

let%test_module "Sequence.merge*" =
  (module struct
    let%test_unit _ =
      [%test_eq: int Merge_with_duplicates_element.t list]
        (to_list
           (merge_with_duplicates
              (of_list [ 1; 2; ])
              (of_list [ 2; 3; ])
              (* Can't use Core_int.compare because it would be a dependency cycle. *)
              ~cmp:Pervasives.compare))
        [ Left 1; Both (2, 2); Right 3; ]

    let%test_unit _ =
      [%test_eq: int Merge_with_duplicates_element.t list]
        (to_list
           (merge_with_duplicates
              (of_list [ 2; 1; ])
              (of_list [ 2; 3; ])
              ~cmp:Pervasives.compare))
        [ Both (2, 2); Left 1; Right 3; ]

    let%test_unit _ =
      [%test_eq: (int * string) list]
        (to_list
           (merge
              (of_list [ (0, "A"); (1, "A"); ])
              (of_list [ (1, "B"); (2, "B"); ])
              ~cmp:(fun a b -> [%compare: int] (fst a) (fst b))))
        [ (0, "A"); (1, "A"); (2, "B"); ]
  end)
