open! Import
open! Hash_set

let%test_module "Set Intersection" =
  (module struct
    let hashable = {
      Hashtbl.Hashable.
      hash = [%hash: string];
      compare = String.compare;
      sexp_of_t = fun t -> Atom t;
    }
    ;;

    let run_test first_contents second_contents ~expect =
      let of_list lst =
        let s = Using_hashable.create ~hashable () in
        List.iter lst ~f:(add s);
        s
      in
      let s1 = of_list first_contents in
      let s2 = of_list second_contents in
      let expect = of_list expect in
      let result = inter s1 s2 in
      iter result ~f:(fun x -> assert (mem expect x));
      iter expect ~f:(fun x -> assert (mem result x));
      let equal x y = 0 = String.compare x y in
      assert (List.equal ~equal (to_list result) (to_list expect));
      assert ((length result) = (length expect));
      (* Make sure the sets are unmodified by the inter *)
      assert ((List.length first_contents)  = length s1);
      assert ((List.length second_contents) = length s2)
    ;;

    let%test_unit "First smaller" =
      run_test  ["0";        "3"; "99"]
        ["0";"1";"2";"3"]
        ~expect:["0";        "3"]

    let%test_unit "Second smaller" =
      run_test  ["a";"b";"c";"d"]
        [    "b";    "d"]
        ~expect:[    "b";    "d"]

    let%test_unit "No intersection" =
      run_test ~expect:[] ["a";"b";"c";"d"] ["1";"2";"3";"4"]
  end)
