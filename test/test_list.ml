open! Import
open! List

[%%template
let config =
  { Base_quickcheck.Test.default_config with
    (* We're using some sizes that are ~30x bigger than the defaults; reduce the test
       count so these tests don't take too long. *)
    test_count = Base_quickcheck.Test.default_config.test_count / 10
  ; sizes =
      (* Make sure to include sizes that will trigger the max_non_tailcall check. *)
      (let big_size = List.Private.max_non_tailcall * 2 in
       Sequence.cycle_list_exn [ 0; 1; 2; 3; 30; big_size; big_size; big_size ])
  }
;;

module [@kind k = value] Harness = struct
  module Elt = struct
    include Int

    type boxed = int [@@deriving compare ~localize, quickcheck, sexp_of ~stackify]

    let box = Fn.id
    let unbox = Fn.id
  end

  module Harness = struct
    type t = Elt.boxed list [@@deriving compare ~localize, quickcheck, sexp_of ~stackify]

    let box : (Elt.t List.t[@kind k]) @ l -> t @ l = Fn.id
    [@@alloc a @ l = (stack_local, heap_global)]
    ;;

    let unbox = Fn.id
  end
end

module [@kind k = bits32] Harness = struct
  module Elt = struct
    include Int32_u

    type boxed = int32 [@@deriving compare ~localize, quickcheck, sexp_of ~stackify]

    let box = Int32_u.to_int32
    let unbox = Int32_u.of_int32
  end

  module Harness = struct
    type t = Elt.boxed list [@@deriving compare ~localize, quickcheck, sexp_of ~stackify]

    let box : (Elt.t List.t[@kind k]) @ l -> t @ l =
      (List.map [@kind k value_or_null] [@mode l] [@alloc a]) ~f:[%eta1 Int32_u.to_int32]
    [@@alloc a @ l = (stack_local, heap_global)]
    ;;

    let unbox = (List.map [@kind value_or_null k]) ~f:Int32_u.of_int32
  end
end

module Name = struct
  let[@kind value] kind = "value"
  let[@kind bits32] kind = "bits32"
  let[@alloc heap] alloc = "heap"
  let[@alloc stack] alloc = "stack"

  let name fname = Printf.sprintf "%s_%s_%s" fname (kind [@kind k]) (alloc [@alloc a])
  [@@kind k = (value, bits32)] [@@alloc a = (heap, stack)]
  ;;
end

module [@kind k = value] [@alloc a @ l = (stack_local, heap_global)] _ = struct
  open Harness [@kind k]

  let name = (Name.name [@kind k] [@alloc a])

  let%test_unit [%name name "is_empty"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual = (List.is_empty [@kind k]) (Harness.unbox list) in
      let expected = List.length list = 0 in
      [%test_eq: bool] actual expected)
  ;;

  let%test_unit [%name name "iter_until"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let n = 1 + Stdlib.List.length list in
      let actual = ref [] in
      let actual_res =
        (List.iter_until [@kind k] [@mode l l])
          (Harness.unbox list)
          ~f:(fun (x : Elt.t @ l) ->
            if Elt.(x % of_int_exn n = of_int_exn 0)
            then Continue_or_stop.Stop (Some (Elt.box x))
            else (
              actual := Elt.box x :: !actual;
              Continue_or_stop.Continue ()))
          ~finish:(fun () -> None)
      in
      let expected = ref [] in
      let expected_res =
        With_return.with_return_option (fun { return } ->
          Stdlib.List.iter
            (fun x ->
              if Elt.(x % of_int_exn n = of_int_exn 0)
              then return (Elt.box x)
              else expected := Elt.box x :: !expected)
            list)
      in
      ([%test_eq: Harness.t] [@alloc stack]) !actual (Harness.box !expected);
      ([%test_eq: Elt.boxed option] [@alloc stack]) actual_res expected_res [@nontail])
  ;;

  let%test_unit [%name name "fold_result"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let n = 1 + Stdlib.List.length list in
      let actual =
        (List.fold_result [@kind k value] [@mode l l])
          (Harness.unbox list)
          ~init:[]
          ~f:(fun acc (x : Elt.t @ l) ->
            if Elt.(x % of_int_exn n = of_int_exn 0)
            then Error (Elt.box x)
            else Ok (Elt.box x :: acc) [@exclave_if_stack a])
      in
      let expected =
        With_return.with_return (fun { return } ->
          Ok
            (Stdlib.List.fold_left
               (fun acc x ->
                 if Elt.(x % of_int_exn n = of_int_exn 0)
                 then return (Error x)
                 else Elt.box x :: acc)
               []
               list))
      in
      ([%test_eq: (Harness.t, Elt.boxed) Result.t] [@alloc stack])
        actual
        expected [@nontail])
  ;;

  let%test_unit [%name name "fold_until"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let n = 1 + Stdlib.List.length list in
      let actual =
        (List.fold_until [@kind k value] [@mode l l])
          (Harness.unbox list)
          ~init:[]
          ~f:(fun acc (x : Elt.t @ l) ->
            if Elt.(x % of_int_exn n = of_int_exn 0)
            then Stop (Error (Elt.box x))
            else Continue (Elt.box x :: acc) [@exclave_if_stack a])
          ~finish:(fun acc -> Ok acc [@exclave_if_stack a])
      in
      let expected =
        With_return.with_return (fun { return } ->
          Ok
            (Stdlib.List.fold_left
               (fun acc x ->
                 if Elt.(x % of_int_exn n = of_int_exn 0)
                 then return (Error x)
                 else Elt.box x :: acc)
               []
               list))
      in
      ([%test_eq: (Harness.t, Elt.boxed) Result.t] [@alloc stack])
        actual
        expected [@nontail])
  ;;

  let%test_unit [%name name "foldi_until"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let n = 1 + Stdlib.List.length list in
      let actual =
        (List.foldi_until [@kind k value] [@mode l l])
          (Harness.unbox list)
          ~init:[]
          ~f:(fun i acc (x : Elt.t @ l) ->
            if Elt.(x % of_int_exn n = of_int_exn 0)
            then Stop (Error (i, Elt.box x))
            else Continue (Elt.box x :: acc) [@exclave_if_stack a])
          ~finish:(fun i acc -> Ok (i, acc) [@exclave_if_stack a])
      in
      let expected =
        With_return.with_return (fun { return } ->
          Ok
            (Stdlib.List.fold_left
               (fun (i, acc) x ->
                 if Elt.(x % of_int_exn n = of_int_exn 0)
                 then return (Error (i, x))
                 else i + 1, Elt.box x :: acc)
               (0, [])
               list))
      in
      ([%test_eq: (int * Harness.t, int * Elt.boxed) Result.t] [@alloc stack])
        actual
        expected [@nontail])
  ;;

  let%test_unit [%name name "exists"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.compare x (Elt.of_int_exn 5) > 0 in
      let actual = (List.exists [@kind k] [@mode l]) (Harness.unbox list) ~f in
      let expected = Stdlib.List.exists (fun x -> f (Elt.unbox x)) list in
      [%test_eq: bool] actual expected)
  ;;

  let%test_unit [%name name "existsi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x = i % 2 = 0 && Elt.compare x (Elt.of_int_exn 3) > 0 in
      let actual = (List.existsi [@kind k] [@mode l]) list ~f in
      let _, expected =
        Stdlib.List.fold_left (fun (i, acc) x -> i + 1, acc || f i x) (0, false) list
      in
      [%test_eq: bool] actual expected)
  ;;

  let%test_unit [%name name "for_all"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.compare x (Elt.of_int 100) < 0 in
      let actual = (List.for_all [@kind k] [@mode l]) list ~f in
      let expected = Stdlib.List.for_all (fun x -> f (Elt.unbox x)) list in
      [%test_eq: bool] actual expected)
  ;;

  let%test_unit [%name name "for_alli"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x = i < 10 || Elt.compare x (Elt.of_int 0) >= 0 in
      let actual = (List.for_alli [@kind k] [@mode l]) list ~f in
      let _, expected =
        Stdlib.List.fold_left (fun (i, acc) x -> i + 1, acc && f i x) (0, true) list
      in
      [%test_eq: bool] actual expected)
  ;;

  let%test_unit [%name name "count"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.compare x (Elt.of_int 10) <= 0 in
      let actual = (List.count [@kind k] [@mode l]) list ~f in
      let expected =
        Stdlib.List.fold_left (fun acc x -> if f x then acc + 1 else acc) 0 list
      in
      [%test_eq: Int.t] actual expected)
  ;;

  let%test_unit [%name name "counti"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x = i % 3 = 0 && Elt.compare x (Elt.of_int 5) > 0 in
      let actual = (List.counti [@kind k] [@mode l]) list ~f in
      let _, expected =
        Stdlib.List.fold_left
          (fun (i, acc) x -> i + 1, if f i x then acc + 1 else acc)
          (0, 0)
          list
      in
      [%test_eq: Int.t] actual expected)
  ;;

  let%test_unit [%name name "sum"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual =
        (List.sum [@kind k] [@mode l l]) (module Int) (Harness.unbox list) ~f:Fn.id
      in
      let expected = Stdlib.List.fold_left (fun acc x -> acc + Elt.to_int x) 0 list in
      [%test_eq: Int.t] actual expected)
  ;;

  let%test_unit [%name name "find"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.compare x (Elt.of_int 7) > 0 in
      let actual = (List.find [@kind k] [@mode l]) (Harness.unbox list) ~f in
      let expected =
        match Stdlib.List.find_opt (fun x -> f (Elt.unbox x)) list with
        | Some x -> Some (Elt.unbox x)
        | None -> None
      in
      ([%test_eq: Elt.t option] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "find_or_null"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.compare x (Elt.of_int 7) > 0 in
      let actual =
        (List.find_or_null [@kind k] [@mode l]) (Harness.unbox list) ~f
        |> (Or_null.to_option [@mode l])
      in
      let expected =
        match Stdlib.List.find_opt (fun x -> f (Elt.unbox x)) list with
        | Some x -> Some (Elt.unbox x)
        | None -> None
      in
      ([%test_eq: Elt.t option] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "findi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x = i > 2 && Elt.compare x (Elt.of_int 4) <= 0 in
      let actual = (List.findi [@kind k] [@mode l]) (Harness.unbox list) ~f in
      let expected =
        Stdlib.List.find_mapi (fun i x -> if f i x then Some (i, x) else None) list
      in
      ([%test_eq: (int * Elt.t) option] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "find_map"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x =
        let v = Elt.to_int x in
        if v % 3 = 0 then Some (v * 2) else None
      in
      let actual = (List.find_map [@kind k] [@mode l l]) (Harness.unbox list) ~f in
      let expected = Stdlib.List.find_map f list in
      ([%test_eq: int option] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "find_mapi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x =
        let v = Elt.to_int x in
        if i % 2 = 1 && v > 5 then Some (i + v) else None
      in
      let actual = (List.find_mapi [@kind k] [@mode l l]) (Harness.unbox list) ~f in
      let expected = Stdlib.List.find_mapi (fun i x -> f i x) list in
      ([%test_eq: Elt.boxed option] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "to_list"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual = (List.to_list [@kind k] [@alloc a]) (Harness.unbox list) in
      let expected = list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "min_elt"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual =
        (List.min_elt [@kind k] [@mode l]) (Harness.unbox list) ~compare:Elt.compare
      in
      let expected =
        match Stdlib.List.sort Elt.compare list with
        | [] -> None
        | x :: _ -> Some x
      in
      ([%test_eq: Elt.t option] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "max_elt"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual =
        (List.max_elt [@kind k] [@mode l]) (Harness.unbox list) ~compare:Elt.compare
      in
      let expected =
        match Stdlib.List.sort (fun x y -> Elt.compare y x) list with
        | [] -> None
        | x :: _ -> Some x
      in
      ([%test_eq: Elt.t option] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "of_array"] =
    Base_quickcheck.Test.run_exn
      ~config
      (module struct
        type t = Elt.boxed array [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun array ->
        let actual = (List.of_array [@kind k] [@alloc a]) array in
        let expected = Stdlib.Array.to_list array in
        ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "concat"] =
    Base_quickcheck.Test.run_exn
      ~config
      (module struct
        type t = Harness.t list [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun lists ->
        let unboxed_lists = List.map lists ~f:Harness.unbox in
        let actual = (List.concat [@kind k] [@alloc a]) unboxed_lists in
        let expected = Stdlib.List.concat lists in
        ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "partition_tf"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.(x % of_int_exn 2 = of_int_exn 0) in
      let actual_true, actual_false =
        (List.partition_tf [@kind k] [@alloc a]) (Harness.unbox list) ~f
      in
      let actual_true = (Harness.box [@alloc a]) actual_true in
      let actual_false = (Harness.box [@alloc a]) actual_false in
      let expected_true, expected_false =
        Stdlib.List.partition (fun x -> f (Elt.unbox x)) list
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual_true expected_true [@nontail];
      ([%test_eq: Harness.t] [@alloc stack]) actual_false expected_false [@nontail])
  ;;

  let%test_unit [%name name "partitioni_tf"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x = i % 2 = 0 && Elt.(x > of_int_exn 5) in
      let actual_true, actual_false =
        (List.partitioni_tf [@kind k] [@alloc a]) (Harness.unbox list) ~f
      in
      let actual_true = (Harness.box [@alloc a]) actual_true in
      let actual_false = (Harness.box [@alloc a]) actual_false in
      let expected_true = Stdlib.List.filteri f list in
      let expected_false = Stdlib.List.filteri (fun i x -> not (f i x)) list in
      ([%test_eq: Harness.t] [@alloc stack]) actual_true expected_true [@nontail];
      ([%test_eq: Harness.t] [@alloc stack]) actual_false expected_false [@nontail])
  ;;

  let%test_unit [%name name "partition_map"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x : _ Either.t =
        if Elt.(x % of_int_exn 3 = of_int_exn 0)
        then First Elt.(x / of_int_exn 2)
        else Second x
      in
      let actual_first, actual_second =
        (List.partition_map [@kind k k] [@alloc a]) (Harness.unbox list) ~f
      in
      let actual_first = (Harness.box [@alloc a]) actual_first in
      let actual_second = (Harness.box [@alloc a]) actual_second in
      let expected_first, expected_second =
        Stdlib.List.partition_map
          (fun x ->
            match f (Elt.unbox x) with
            | First y -> Left (Elt.box y)
            | Second y -> Right (Elt.box y))
          list
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual_first expected_first [@nontail];
      ([%test_eq: Harness.t] [@alloc stack]) actual_second expected_second [@nontail])
  ;;

  let%test_unit [%name name "partition_mapi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x : _ Either.t =
        if i % 3 = 0 then First Elt.(x / of_int_exn 2) else Second x
      in
      let actual_first, actual_second =
        (List.partition_mapi [@kind k k] [@alloc a]) (Harness.unbox list) ~f
      in
      let actual_first = (Harness.box [@alloc a]) actual_first in
      let actual_second = (Harness.box [@alloc a]) actual_second in
      let index = ref 0 in
      let expected_first, expected_second =
        Stdlib.List.partition_map
          (fun x ->
            let i = !index in
            Int.incr index;
            match f i (Elt.unbox x) with
            | First y -> Left (Elt.box y)
            | Second y -> Right (Elt.box y))
          list
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual_first expected_first [@nontail];
      ([%test_eq: Harness.t] [@alloc stack]) actual_second expected_second [@nontail])
  ;;

  let%test_unit [%name name "fold_right"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual =
        (List.fold_right [@kind k value] [@mode l l])
          (Harness.unbox list)
          ~init:[]
          ~f:(fun (x : Elt.t @ l) acc -> Elt.box x :: acc [@exclave_if_stack a])
      in
      let expected = list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "filter_opt"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = if Int.(x % of_int_exn 2 = of_int_exn 0) then Some x else None in
      let actual =
        (Harness.box [@alloc a])
          (Harness.unbox list |> List.map ~f |> (List.filter_opt [@kind k] [@alloc a]))
      in
      let expected = Stdlib.List.filter_map (fun x -> Elt.box (f (Elt.unbox x))) list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "rev_filter_map"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = if Int.(x % of_int_exn 2 = of_int_exn 0) then Some x else None in
      let actual =
        (Harness.box [@alloc a]) (List.rev_filter_map (Harness.unbox list) ~f)
      in
      let expected =
        Stdlib.List.filter_map (fun x -> Elt.box (f (Elt.unbox x))) list
        |> Stdlib.List.rev
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "rev_filter_mapi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x = if i % 2 = 0 then Some Elt.(x / of_int_exn 2) else None in
      let actual =
        (Harness.box [@alloc a]) (List.rev_filter_mapi (Harness.unbox list) ~f)
      in
      let expected =
        Stdlib.List.mapi (fun i x -> Elt.box (f i (Elt.unbox x))) list
        |> Stdlib.List.filter_map Fn.id
        |> Stdlib.List.rev
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "nth"] =
    Base_quickcheck.Test.run_exn
      ~config
      (module struct
        type t = Harness.t * int [@@deriving quickcheck, sexp_of]

        let quickcheck_generator =
          let open Base_quickcheck.Generator.Let_syntax in
          let%bind harness_list = Harness.quickcheck_generator in
          let%bind idx_to_look_for =
            Base_quickcheck.Generator.small_positive_or_zero_int
          in
          return (harness_list, idx_to_look_for)
        ;;
      end)
      ~f:(fun (list, idx) ->
        let actual = (List.nth [@kind k k] [@mode l]) (Harness.unbox list) idx in
        let expected = Stdlib.List.nth_opt list idx in
        ([%test_eq: Elt.t option] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "nth_or_null"] =
    Base_quickcheck.Test.run_exn
      ~config
      (module struct
        type t = Harness.t * int [@@deriving quickcheck, sexp_of]

        let quickcheck_generator =
          let open Base_quickcheck.Generator.Let_syntax in
          let%bind harness_list = Harness.quickcheck_generator in
          let%bind idx_to_look_for =
            Base_quickcheck.Generator.small_positive_or_zero_int
          in
          return (harness_list, idx_to_look_for)
        ;;
      end)
      ~f:(fun (list, idx) ->
        let actual =
          (List.nth_or_null [@kind k k] [@mode l]) (Harness.unbox list) idx
          |> (Or_null.to_option [@mode l])
        in
        let expected = Stdlib.List.nth_opt list idx in
        ([%test_eq: Elt.t option] [@alloc stack]) actual expected [@nontail])
  ;;
end

module [@kind k = (value, bits32)] [@alloc a @ l = (stack_local, heap_global)] _ = struct
  open Harness [@kind k]

  let name = (Name.name [@kind k] [@alloc a])

  let%test_unit [%name name "length"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual = (List.length [@kind k]) (Harness.unbox list) in
      let expected = Stdlib.List.length list in
      [%test_eq: Int.t] actual expected)
  ;;

  let%test_unit [%name name "iter"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let output = ref [] in
      (List.iter [@kind k] [@mode l]) (Harness.unbox list) ~f:(fun (x : Elt.t @ l) ->
        output := Elt.box x :: !output);
      ([%test_eq: Harness.t] [@alloc stack]) !output (Stdlib.List.rev list))
  ;;

  let%test_unit [%name name "iteri"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual = ref [] in
      (List.iteri [@kind k] [@mode l]) (Harness.unbox list) ~f:(fun i (x : Elt.t @ l) ->
        actual := ( :: ) ((i, Elt.box x), !actual));
      let expected = ref [] in
      Stdlib.List.iteri (fun i x -> expected := ( :: ) ((i, x), !expected)) list;
      ([%test_eq: (int * Elt.boxed) list] [@alloc stack]) !actual !expected)
  ;;

  let%test_unit [%name name "fold"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual =
        (List.fold [@kind k value_or_null] [@mode l l])
          (Harness.unbox list)
          ~init:[]
          ~f:(fun acc (x : Elt.t @ l) -> Elt.box x :: acc [@exclave_if_stack a])
      in
      let expected = Stdlib.List.rev list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "foldi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual =
        (List.foldi [@kind k value_or_null] [@mode l l])
          (Harness.unbox list)
          ~init:[]
          ~f:(fun i acc (x : Elt.t @ l) -> (i, Elt.box x) :: acc [@exclave_if_stack a])
      in
      let expected = Stdlib.List.mapi (fun i x -> i, x) list |> Stdlib.List.rev in
      ([%test_eq: (int * Elt.boxed) list] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "mem"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let target = Elt.of_int_exn 0 in
      let actual =
        (List.mem [@kind k] [@mode l])
          (Harness.unbox list)
          target
          ~equal:(Elt.equal [@mode local])
      in
      let expected = Stdlib.List.mem (Elt.box target) list in
      [%test_eq: bool] actual expected)
  ;;

  let%test_unit [%name name "append"] =
    Base_quickcheck.Test.run_exn
      ~config
      (module struct
        type t = Harness.t * Harness.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (list1, list2) ->
        let actual =
          (Harness.box [@alloc a])
            ((List.append [@kind k] [@alloc a])
               (Harness.unbox list1)
               (Harness.unbox list2))
        in
        let expected = Stdlib.List.append list1 list2 in
        ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "map"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.(x / of_int_exn 2) in
      let actual =
        (Harness.box [@alloc a])
          ((List.map [@kind k k] [@mode l] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected = Stdlib.List.map (fun x -> Elt.box (f (Elt.unbox x))) list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "mapi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x =
        let div = i + 1 in
        Elt.(x % of_int_exn div)
      in
      let actual =
        (Harness.box [@alloc a])
          ((List.mapi [@kind k k] [@mode l] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected = Stdlib.List.mapi (fun i x -> Elt.box (f i (Elt.unbox x))) list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "filter"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.(x % of_int_exn 2 = of_int_exn 0) in
      let actual =
        (Harness.box [@alloc a])
          ((List.filter [@kind k] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected = Stdlib.List.filter (fun x -> f (Elt.unbox x)) list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "filteri"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x = i % 2 = 0 && Elt.(x % of_int_exn 2 = of_int_exn 0) in
      let actual =
        (Harness.box [@alloc a])
          ((List.filteri [@kind k] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected = Stdlib.List.filteri (fun i x -> f i (Elt.unbox x)) list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "filter_map"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x : (Elt.t Option.t[@kind k]) =
        if Elt.(x % of_int_exn 3 = of_int_exn 0)
        then Some Elt.(x / of_int_exn 2)
        else None
      in
      let actual =
        (Harness.box [@alloc a])
          ((List.filter_map [@kind k k] [@mode l] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected =
        Stdlib.List.filter_map
          (fun x ->
            match f (Elt.unbox x) with
            | Some y -> Some (Elt.box y)
            | None -> None)
          list
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "filter_mapi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x : (Elt.t Option.t[@kind k]) =
        if i % 2 = 1 && Elt.(x > of_int_exn 2) then Some Elt.(x % of_int_exn i) else None
      in
      let actual =
        (Harness.box [@alloc a])
          ((List.filter_mapi [@kind k k] [@mode l] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected =
        Stdlib.List.mapi
          (fun i x ->
            match f i (Elt.unbox x) with
            | None -> None
            | Some y -> Some (Elt.box y))
          list
        |> Stdlib.List.filter_map Fn.id
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "concat_map"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x : (Elt.t List.t[@kind k]) =
        if Elt.(x % of_int_exn 2 = of_int_exn 0)
        then [ x; Elt.(x / of_int_exn 2) ]
        else [ x ]
      in
      let actual =
        (Harness.box [@alloc a])
          ((List.concat_map [@kind k k] [@mode l] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected =
        Stdlib.List.concat_map (fun x -> Harness.box (f (Elt.unbox x))) list
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "concat_mapi"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f i x : (Elt.t List.t[@kind k]) =
        if i % 2 = 0 then [ x ] else [ x; Elt.(x % of_int_exn i) ]
      in
      let actual =
        (Harness.box [@alloc a])
          ((List.concat_mapi [@kind k k] [@mode l] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected =
        Stdlib.List.mapi (fun i x -> Harness.box (f i (Elt.unbox x))) list
        |> Stdlib.List.concat
      in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "init"] =
    Base_quickcheck.Test.run_exn
      ~config
      (module struct
        type t = int [@@deriving quickcheck, sexp_of]

        let quickcheck_generator = Base_quickcheck.Generator.small_positive_or_zero_int
      end)
      ~f:(fun len ->
        let f i = Elt.of_int_exn (i * 2) in
        let actual_result =
          try Ok ((Harness.box [@alloc a]) ((List.init [@kind k] [@alloc a]) len ~f)) with
          | exn -> Error exn
        in
        let expected_result =
          try Ok (Stdlib.List.init len (fun i -> Elt.box (f i))) with
          | exn -> Error exn
        in
        match actual_result, expected_result with
        | Ok actual, Ok expected ->
          ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail]
        | Error _, Error _ -> () (* Both raised exceptions, which is correct *)
        | _ -> assert false (* One raised exception but the other didn't *))
  ;;

  let%test_unit [%name name "rev"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let actual = (Harness.box [@alloc a]) ((List.rev [@kind k]) (Harness.unbox list)) in
      let expected = Stdlib.List.rev list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;

  let%test_unit [%name name "rev_map"] =
    Base_quickcheck.Test.run_exn ~config (module Harness) ~f:(fun list ->
      let f x = Elt.(x / of_int_exn 2) in
      let actual =
        (Harness.box [@alloc a])
          ((List.rev_map [@kind k k] [@mode l] [@alloc a]) (Harness.unbox list) ~f)
      in
      let expected = Stdlib.List.rev_map (fun x -> Elt.box (f (Elt.unbox x))) list in
      ([%test_eq: Harness.t] [@alloc stack]) actual expected [@nontail])
  ;;
end]

let%expect_test "find_exn" =
  let show f sexp_of_ok = print_s [%sexp (Result.try_with f : (ok, exn) Result.t)] in
  let test list =
    show
      (fun () ->
        List.find_exn list ~f:(function
          | This x -> Int.is_negative x
          | Null -> false))
      [%sexp_of: int or_null]
  in
  test [];
  [%expect {| (Error (Not_found_s "List.find_exn: not found")) |}];
  test [ This 1; This 2; This 3 ];
  [%expect {| (Error (Not_found_s "List.find_exn: not found")) |}];
  test [ This (-1); This (-2); This (-3) ];
  [%expect {| (Ok (-1)) |}];
  test [ Null; This (-2); This (-3) ];
  [%expect {| (Ok (-2)) |}]
;;

module%test [@name "reduce_balanced"] _ = struct
  let test expect list =
    [%test_result: string option]
      ~expect
      (reduce_balanced ~f:(fun a b -> "(" ^ a ^ "+" ^ b ^ ")") list)
  ;;

  let%test_unit "length 0" = test None []
  let%test_unit "length 1" = test (Some "a") [ "a" ]
  let%test_unit "length 2" = test (Some "(a+b)") [ "a"; "b" ]

  let%test_unit "length 6" =
    test (Some "(((a+b)+(c+d))+(e+f))") [ "a"; "b"; "c"; "d"; "e"; "f" ]
  ;;

  let%test_unit "longer" =
    (* pairs (index, number of times f called on me) to check:
       1. f called on results in index order
       2. total number of calls on any element is low called on 2^n + 1 to demonstrate
          lack of balance (most elements are distance 7 from the tree root, but one is
          distance 1) *)
    let data = map (range 0 65) ~f:(fun i -> [ i, 0 ]) in
    let f x y = map (x @ y) ~f:(fun (ix, cx) -> ix, cx + 1) in
    match reduce_balanced data ~f with
    | None -> failwith "None"
    | Some l ->
      [%test_result: int] ~expect:65 (List.length l);
      iteri l ~f:(fun actual_index (computed_index, num_f) ->
        let expected_num_f = if actual_index = 64 then 1 else 7 in
        [%test_result: int * int]
          ~expect:(actual_index, expected_num_f)
          (computed_index, num_f))
  ;;
end

module%test [@name "range symmetries"] _ = struct
  let basic ~stride ~start ~stop ~start_n ~stop_n ~result =
    [%compare.equal: int t] (range ~stride ~start ~stop start_n stop_n) result
  ;;

  let test stride (start_n, start) (stop_n, stop) result =
    basic ~stride ~start ~stop ~start_n ~stop_n ~result
    && (* works for negative [start] and [stop] *)
    basic
      ~stride:(-stride)
      ~start_n:(-start_n)
      ~stop_n:(-stop_n)
      ~start
      ~stop
      ~result:(List.map result ~f:(fun x -> -x))
  ;;

  let%test _ = test 1 (3, `inclusive) (1, `exclusive) []
  let%test _ = test 1 (3, `inclusive) (3, `exclusive) []
  let%test _ = test 1 (3, `inclusive) (4, `exclusive) [ 3 ]
  let%test _ = test 1 (3, `inclusive) (8, `exclusive) [ 3; 4; 5; 6; 7 ]
  let%test _ = test 3 (4, `inclusive) (10, `exclusive) [ 4; 7 ]
  let%test _ = test 3 (4, `inclusive) (11, `exclusive) [ 4; 7; 10 ]
  let%test _ = test 3 (4, `inclusive) (12, `exclusive) [ 4; 7; 10 ]
  let%test _ = test 3 (4, `inclusive) (13, `exclusive) [ 4; 7; 10 ]
  let%test _ = test 3 (4, `inclusive) (14, `exclusive) [ 4; 7; 10; 13 ]
  let%test _ = test (-1) (1, `inclusive) (3, `exclusive) []
  let%test _ = test (-1) (3, `inclusive) (3, `exclusive) []
  let%test _ = test (-1) (4, `inclusive) (3, `exclusive) [ 4 ]
  let%test _ = test (-1) (8, `inclusive) (3, `exclusive) [ 8; 7; 6; 5; 4 ]
  let%test _ = test (-3) (10, `inclusive) (4, `exclusive) [ 10; 7 ]
  let%test _ = test (-3) (10, `inclusive) (3, `exclusive) [ 10; 7; 4 ]
  let%test _ = test (-3) (10, `inclusive) (2, `exclusive) [ 10; 7; 4 ]
  let%test _ = test (-3) (10, `inclusive) (1, `exclusive) [ 10; 7; 4 ]
  let%test _ = test (-3) (10, `inclusive) (0, `exclusive) [ 10; 7; 4; 1 ]
  let%test _ = test 1 (3, `exclusive) (1, `exclusive) []
  let%test _ = test 1 (3, `exclusive) (3, `exclusive) []
  let%test _ = test 1 (3, `exclusive) (4, `exclusive) []
  let%test _ = test 1 (3, `exclusive) (8, `exclusive) [ 4; 5; 6; 7 ]
  let%test _ = test 3 (4, `exclusive) (10, `exclusive) [ 7 ]
  let%test _ = test 3 (4, `exclusive) (11, `exclusive) [ 7; 10 ]
  let%test _ = test 3 (4, `exclusive) (12, `exclusive) [ 7; 10 ]
  let%test _ = test 3 (4, `exclusive) (13, `exclusive) [ 7; 10 ]
  let%test _ = test 3 (4, `exclusive) (14, `exclusive) [ 7; 10; 13 ]
  let%test _ = test (-1) (1, `exclusive) (3, `exclusive) []
  let%test _ = test (-1) (3, `exclusive) (3, `exclusive) []
  let%test _ = test (-1) (4, `exclusive) (3, `exclusive) []
  let%test _ = test (-1) (8, `exclusive) (3, `exclusive) [ 7; 6; 5; 4 ]
  let%test _ = test (-3) (10, `exclusive) (4, `exclusive) [ 7 ]
  let%test _ = test (-3) (10, `exclusive) (3, `exclusive) [ 7; 4 ]
  let%test _ = test (-3) (10, `exclusive) (2, `exclusive) [ 7; 4 ]
  let%test _ = test (-3) (10, `exclusive) (1, `exclusive) [ 7; 4 ]
  let%test _ = test (-3) (10, `exclusive) (0, `exclusive) [ 7; 4; 1 ]
  let%test _ = test 1 (3, `inclusive) (1, `inclusive) []
  let%test _ = test 1 (3, `inclusive) (3, `inclusive) [ 3 ]
  let%test _ = test 1 (3, `inclusive) (4, `inclusive) [ 3; 4 ]
  let%test _ = test 1 (3, `inclusive) (8, `inclusive) [ 3; 4; 5; 6; 7; 8 ]
  let%test _ = test 3 (4, `inclusive) (10, `inclusive) [ 4; 7; 10 ]
  let%test _ = test 3 (4, `inclusive) (11, `inclusive) [ 4; 7; 10 ]
  let%test _ = test 3 (4, `inclusive) (12, `inclusive) [ 4; 7; 10 ]
  let%test _ = test 3 (4, `inclusive) (13, `inclusive) [ 4; 7; 10; 13 ]
  let%test _ = test 3 (4, `inclusive) (14, `inclusive) [ 4; 7; 10; 13 ]
  let%test _ = test (-1) (1, `inclusive) (3, `inclusive) []
  let%test _ = test (-1) (3, `inclusive) (3, `inclusive) [ 3 ]
  let%test _ = test (-1) (4, `inclusive) (3, `inclusive) [ 4; 3 ]
  let%test _ = test (-1) (8, `inclusive) (3, `inclusive) [ 8; 7; 6; 5; 4; 3 ]
  let%test _ = test (-3) (10, `inclusive) (4, `inclusive) [ 10; 7; 4 ]
  let%test _ = test (-3) (10, `inclusive) (3, `inclusive) [ 10; 7; 4 ]
  let%test _ = test (-3) (10, `inclusive) (2, `inclusive) [ 10; 7; 4 ]
  let%test _ = test (-3) (10, `inclusive) (1, `inclusive) [ 10; 7; 4; 1 ]
  let%test _ = test (-3) (10, `inclusive) (0, `inclusive) [ 10; 7; 4; 1 ]
  let%test _ = test 1 (3, `exclusive) (1, `inclusive) []
  let%test _ = test 1 (3, `exclusive) (3, `inclusive) []
  let%test _ = test 1 (3, `exclusive) (4, `inclusive) [ 4 ]
  let%test _ = test 1 (3, `exclusive) (8, `inclusive) [ 4; 5; 6; 7; 8 ]
  let%test _ = test 3 (4, `exclusive) (10, `inclusive) [ 7; 10 ]
  let%test _ = test 3 (4, `exclusive) (11, `inclusive) [ 7; 10 ]
  let%test _ = test 3 (4, `exclusive) (12, `inclusive) [ 7; 10 ]
  let%test _ = test 3 (4, `exclusive) (13, `inclusive) [ 7; 10; 13 ]
  let%test _ = test 3 (4, `exclusive) (14, `inclusive) [ 7; 10; 13 ]
  let%test _ = test (-1) (1, `exclusive) (3, `inclusive) []
  let%test _ = test (-1) (3, `exclusive) (3, `inclusive) []
  let%test _ = test (-1) (4, `exclusive) (3, `inclusive) [ 3 ]
  let%test _ = test (-1) (8, `exclusive) (3, `inclusive) [ 7; 6; 5; 4; 3 ]
  let%test _ = test (-3) (10, `exclusive) (4, `inclusive) [ 7; 4 ]
  let%test _ = test (-3) (10, `exclusive) (3, `inclusive) [ 7; 4 ]
  let%test _ = test (-3) (10, `exclusive) (2, `inclusive) [ 7; 4 ]
  let%test _ = test (-3) (10, `exclusive) (1, `inclusive) [ 7; 4; 1 ]
  let%test _ = test (-3) (10, `exclusive) (0, `inclusive) [ 7; 4; 1 ]

  let test_start_inc_exc stride start (stop, stop_inc_exc) result =
    test stride (start, `inclusive) (stop, stop_inc_exc) result
    &&
    match result with
    | [] -> true
    | head :: tail ->
      head = start && test stride (start, `exclusive) (stop, stop_inc_exc) tail
  ;;

  let test_inc_exc stride start stop result =
    test_start_inc_exc stride start (stop, `inclusive) result
    &&
    match List.rev result with
    | [] -> true
    | last :: all_but_last ->
      let all_but_last = List.rev all_but_last in
      if last = stop
      then test_start_inc_exc stride start (stop, `exclusive) all_but_last
      else true
  ;;

  let%test _ = test_inc_exc 1 4 10 [ 4; 5; 6; 7; 8; 9; 10 ]
  let%test _ = test_inc_exc 3 4 10 [ 4; 7; 10 ]
  let%test _ = test_inc_exc 3 4 11 [ 4; 7; 10 ]
  let%test _ = test_inc_exc 3 4 12 [ 4; 7; 10 ]
  let%test _ = test_inc_exc 3 4 13 [ 4; 7; 10; 13 ]
  let%test _ = test_inc_exc 3 4 14 [ 4; 7; 10; 13 ]
end

module Test_values = struct
  let long1 =
    let v = lazy (range 1 100_000) in
    fun () -> Lazy.force v
  ;;

  let long2 =
    let v = lazy (range 2 100_001) in
    fun () -> Lazy.force v
  ;;

  let l1 = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]
end

let%expect_test "create" =
  let test len elt = print_s [%sexp (create ~len elt : string list)] in
  test 0 "nothing";
  [%expect {| () |}];
  test 1 "something";
  [%expect {| (something) |}];
  test 2 "ha";
  [%expect {| (ha ha) |}];
  test 3 "ho";
  [%expect {| (ho ho ho) |}];
  require_does_raise (fun () -> test (-1) "oops");
  [%expect {| (Invalid_argument "List.create -1") |}]
;;

let%test_unit _ =
  [%test_result: int Or_null.t list]
    (rev_append [ This 1; This 2; This 3; Null ] [ Null; This 4; Null ])
    ~expect:[ Null; This 3; This 2; This 1; Null; This 4; Null ]
;;

let%test_unit _ = [%test_result: int list] (rev_append [] [ 4; 5; 6 ]) ~expect:[ 4; 5; 6 ]
let%test_unit _ = [%test_result: int list] (rev_append [ 1; 2; 3 ] []) ~expect:[ 3; 2; 1 ]
let%test_unit _ = [%test_result: int list] (rev_append [ 1 ] [ 2; 3 ]) ~expect:[ 1; 2; 3 ]
let%test_unit _ = [%test_result: int list] (rev_append [ 1; 2 ] [ 3 ]) ~expect:[ 2; 1; 3 ]

let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (rev_append long long : int list)
;;

let%test_unit _ =
  let long1 = Test_values.long1 () in
  let long2 = Test_values.long2 () in
  [%test_result: int list] (map long1 ~f:(fun x -> x + 1)) ~expect:long2
;;

let test_ordering n =
  let l = List.range 0 n in
  let r = ref [] in
  let (_ : unit list) = List.map l ~f:(fun x -> r := x :: !r) in
  [%test_eq: int list] l (List.rev !r)
;;

let%test_unit _ = test_ordering 10
let%test_unit _ = test_ordering 1000
let%test_unit _ = test_ordering 1_000_000
let%test _ = for_all2_exn [] [] ~f:(fun _ _ -> assert false)

let find_mapi_f i x =
  match x with
  | This x when i = x -> Some (i + x)
  | _ -> None
;;

let%test_unit _ =
  [%test_result: int option]
    (find_mapi [ This 0; Null; Null; This 1; This 4 ] ~f:find_mapi_f)
    ~expect:(Some 0)
;;

let%test_unit _ =
  [%test_result: int option]
    (find_mapi [ This 3; Null; This 2; Null; This 4 ] ~f:find_mapi_f)
    ~expect:(Some 4)
;;

let%test_unit _ =
  [%test_result: int option]
    (find_mapi [ This 3; This 5; Null; Null; This 4 ] ~f:find_mapi_f)
    ~expect:(Some 8)
;;

let%test_unit _ =
  [%test_result: int option]
    (find_mapi [ This 3; This 5; Null; This 1; Null ] ~f:find_mapi_f)
    ~expect:None
;;

let%test_unit _ = [%test_result: bool] (for_alli [] ~f:(fun _ _ -> false)) ~expect:true

let for_alli_f i x =
  match x with
  | This x when i = x -> true
  | _ -> false
;;

let%test_unit _ =
  [%test_result: bool]
    (for_alli [ This 0; This 1; This 2; This 3 ] ~f:for_alli_f)
    ~expect:true
;;

let%test_unit _ =
  [%test_result: bool]
    (for_alli [ This 0; This 1; This 3; This 3 ] ~f:for_alli_f)
    ~expect:false
;;

let%test_unit _ =
  [%test_result: bool]
    (for_alli [ This 0; This 1; Null; This 3 ] ~f:for_alli_f)
    ~expect:false
;;

let%test_unit _ = [%test_result: bool] (existsi [] ~f:(fun _ _ -> true)) ~expect:false

let existsi_f i x =
  match x with
  | This x when i <> x -> true
  | _ -> false
;;

let%test_unit _ =
  [%test_result: bool]
    (existsi [ This 0; This 1; This 2; This 3 ] ~f:existsi_f)
    ~expect:false
;;

let%test_unit _ =
  [%test_result: bool]
    (existsi [ Null; This 1; This 3; This 3 ] ~f:existsi_f)
    ~expect:true
;;

let%test_unit _ =
  [%test_result: int Or_null.t list]
    (append [ Null; This 2; Null ] [ Null; This 5; This 6 ])
    ~expect:[ Null; This 2; Null; Null; This 5; This 6 ]
;;

let%test_unit _ =
  [%test_result: int Or_null.t list]
    (append [] [ Null; This 5; This 6 ])
    ~expect:[ Null; This 5; This 6 ]
;;

let%test_unit _ =
  [%test_result: int Or_null.t list]
    (append [ This 1; This 2; Null ] [])
    ~expect:[ This 1; This 2; Null ]
;;

let%test_unit _ =
  [%test_result: int Or_null.t list]
    (append [ Null ] [ Null; This 3 ])
    ~expect:[ Null; Null; This 3 ]
;;

let%test_unit _ =
  [%test_result: int Or_null.t list]
    (append [ This 1; Null ] [ This 3 ])
    ~expect:[ This 1; Null; This 3 ]
;;

let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (append long long : int list)
;;

let%test_unit _ =
  [%test_result: int list] (map ~f:Fn.id Test_values.l1) ~expect:Test_values.l1
;;

let%test_unit _ = [%test_result: int list] (map ~f:Fn.id []) ~expect:[]

let%test_unit _ =
  [%test_result: float list]
    (map ~f:(fun x -> x +. 5.) [ 1.; 2.; 3. ])
    ~expect:[ 6.; 7.; 8. ]
;;

let%test_unit _ = ignore (map ~f:Fn.id (Test_values.long1 ()) : int list)

let%test_unit _ =
  [%test_result: (int * char) list]
    (map2_exn ~f:(fun a b -> a, b) [ 1; 2; 3 ] [ 'a'; 'b'; 'c' ])
    ~expect:[ 1, 'a'; 2, 'b'; 3, 'c' ]
;;

let%test_unit _ = [%test_result: _ list] (map2_exn ~f:(fun _ _ -> ()) [] []) ~expect:[]

let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (map2_exn ~f:(fun _ _ -> ()) long long : unit list)
;;

let%test_unit _ =
  [%test_result: int list]
    (rev_map_append [ 1; 2; 3; 4; 5 ] [ 6 ] ~f:Fn.id)
    ~expect:[ 5; 4; 3; 2; 1; 6 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (rev_map_append [ 1; 2; 3; 4; 5 ] [ 6 ] ~f:(fun x -> 2 * x))
    ~expect:[ 10; 8; 6; 4; 2; 6 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (rev_map_append [] [ 6 ] ~f:(fun _ -> failwith "bug!"))
    ~expect:[ 6 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (fold_right ~f:(fun e acc -> e :: acc) Test_values.l1 ~init:[])
    ~expect:Test_values.l1
;;

let%test_unit _ =
  [%test_result: string]
    (fold_right ~f:(fun e acc -> e ^ acc) [ "1"; "2" ] ~init:"3")
    ~expect:"123"
;;

let%test_unit _ =
  [%test_result: unit] (fold_right ~f:(fun _ _ -> ()) [] ~init:()) ~expect:()
;;

let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (fold_right ~f:(fun e acc -> e :: acc) long ~init:[] : int list)
;;

let%test_unit _ =
  [%test_result: string]
    (fold_right2_exn
       ~f:(fun e1 e2 acc -> e1 ^ e2 ^ acc)
       [ "1"; "2" ]
       [ "a"; "b" ]
       ~init:"3c")
    ~expect:"1a2b3c"
;;

let%test_unit _ =
  [%test_result: string Or_unequal_lengths.t]
    (fold_right2
       ~f:(fun e1 e2 acc -> e1 ^ e2 ^ acc)
       [ "1"; "2" ]
       [ "a"; "b"; "c" ]
       ~init:"#")
    ~expect:Or_unequal_lengths.Unequal_lengths
;;

let%test_unit _ =
  let l1 = Test_values.l1 in
  [%test_result: int list * int list]
    (unzip (zip_exn l1 (List.rev l1)))
    ~expect:(l1, List.rev l1)
;;

let%test_unit _ =
  let long = Test_values.long1 () in
  ignore (unzip (zip_exn long long) : int list * int list)
;;

let%test_unit _ =
  [%test_result: int list * int list] (unzip [ 1, 2; 4, 5 ]) ~expect:([ 1; 4 ], [ 2; 5 ])
;;

let%test_unit _ =
  [%test_result: int list * int list * int list]
    (unzip3 [ 1, 2, 3; 4, 5, 6 ])
    ~expect:([ 1; 4 ], [ 2; 5 ], [ 3; 6 ])
;;

let%test_unit _ =
  [%test_result: (int * int) list Or_unequal_lengths.t]
    (zip [ 1; 2; 3 ] [ 4; 5; 6 ])
    ~expect:(Ok [ 1, 4; 2, 5; 3, 6 ])
;;

let%test_unit _ =
  [%test_result: (int * int) list Or_unequal_lengths.t]
    (zip [ 1 ] [ 4; 5; 6 ])
    ~expect:Unequal_lengths
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (zip_exn [ 1; 2; 3 ] [ 4; 5; 6 ])
    ~expect:[ 1, 4; 2, 5; 3, 6 ]
;;

let%expect_test _ =
  show_raise (fun () -> zip_exn [ 1 ] [ 4; 5; 6 ]);
  [%expect {| (raised (Invalid_argument "length mismatch in zip_exn: 1 <> 3")) |}]
;;

let%test_unit _ =
  [%test_result: (int * int * int) list Or_unequal_lengths.t]
    (zip3 [ 1; 2; 3 ] [ 4; 5; 6 ] [ 7; 8; 9 ])
    ~expect:(Ok [ 1, 4, 7; 2, 5, 8; 3, 6, 9 ])
;;

let%test_unit _ =
  [%test_result: (int * int * int) list Or_unequal_lengths.t]
    (zip3 [ 1 ] [ 4; 5 ] [ 7 ])
    ~expect:Unequal_lengths
;;

let%test_unit _ =
  [%test_result: (int * int * int) list]
    (zip3_exn [ 1; 2; 3 ] [ 4; 5; 6 ] [ 7; 8; 9 ])
    ~expect:[ 1, 4, 7; 2, 5, 8; 3, 6, 9 ]
;;

let%expect_test _ =
  show_raise (fun () -> zip3_exn [ 1 ] [ 4; 5 ] [ 7 ]);
  [%expect
    {| (raised (Invalid_argument "length mismatch in zip3_exn: 1 <> 2 || 2 <> 1")) |}]
;;

let%expect_test _ =
  show_raise (fun () ->
    rev_map3_exn [ 1 ] [ 4; 5; 6 ] [ 2; 3 ] ~f:(fun a b c -> a + b + c));
  [%expect
    {| (raised (Invalid_argument "length mismatch in rev_map3_exn: 1 <> 3 || 3 <> 2")) |}]
;;

let%test_unit _ =
  [%test_result: (int * string) list]
    (mapi ~f:(fun i x -> i, x) [ "one"; "two"; "three"; "four" ])
    ~expect:[ 0, "one"; 1, "two"; 2, "three"; 3, "four" ]
;;

let%test_unit _ = [%test_result: (int * _) list] (mapi ~f:(fun i x -> i, x) []) ~expect:[]

module%test [@name "group"] _ = struct
  let%test_unit _ =
    [%test_result: int list list]
      (group [ 1; 2; 3; 4 ] ~break:(fun _ x -> x = 3))
      ~expect:[ [ 1; 2 ]; [ 3; 4 ] ]
  ;;

  let%test_unit _ =
    [%test_result: int list list] (group [] ~break:(fun _ -> assert false)) ~expect:[]
  ;;

  let mis = [ 'M'; 'i'; 's'; 's'; 'i'; 's'; 's'; 'i'; 'p'; 'p'; 'i' ]

  let equal_letters =
    [ [ 'M' ]
    ; [ 'i' ]
    ; [ 's'; 's' ]
    ; [ 'i' ]
    ; [ 's'; 's' ]
    ; [ 'i' ]
    ; [ 'p'; 'p' ]
    ; [ 'i' ]
    ]
  ;;

  let single_letters = [ [ 'M'; 'i'; 's'; 's'; 'i'; 's'; 's'; 'i'; 'p'; 'p'; 'i' ] ]

  let every_three =
    [ [ 'M'; 'i'; 's' ]; [ 's'; 'i'; 's' ]; [ 's'; 'i'; 'p' ]; [ 'p'; 'i' ] ]
  ;;

  let%test_unit _ =
    [%test_result: char list list] (group ~break:Char.( <> ) mis) ~expect:equal_letters
  ;;

  let%test_unit _ =
    [%test_result: char list list]
      (group ~break:(fun _ _ -> false) mis)
      ~expect:single_letters
  ;;

  let%test_unit _ =
    [%test_result: char list list]
      (groupi ~break:(fun i _ _ -> i % 3 = 0) mis)
      ~expect:every_three
  ;;
end

module%test [@name "sort_and_group"] _ = struct
  let%expect_test _ =
    let compare a b =
      Comparable.lift String.compare ~f:(fun s -> String.rstrip ~drop:Char.is_digit s) a b
    in
    [%test_result: string list list]
      (sort_and_group [ "b1"; "c1"; "a1"; "a2"; "b2"; "a3" ] ~compare)
      ~expect:[ [ "a1"; "a2"; "a3" ]; [ "b1"; "b2" ]; [ "c1" ] ]
  ;;
end

module%test [@name "Assoc.group"] _ = struct
  let%expect_test _ =
    let test alist =
      let multi = Assoc.group alist ~equal:String.Caseless.equal in
      print_s [%sexp (multi : (string * int list) list)];
      let round_trip =
        List.concat_map multi ~f:(fun (key, data) ->
          List.map data ~f:(fun datum -> key, datum))
      in
      require_equal
        (module struct
          type t = (String.Caseless.t * int) list [@@deriving equal, sexp_of]
        end)
        alist
        round_trip
    in
    test [];
    [%expect {| () |}];
    test [ "a", 1; "A", 2 ];
    [%expect {| ((a (1 2))) |}];
    test [ "a", 1; "b", 2 ];
    [%expect
      {|
      ((a (1))
       (b (2)))
      |}];
    test [ "odd", 1; "even", 2; "Odd", 3; "Even", 4; "ODD", 5; "EVEN", 6 ];
    [%expect
      {|
      ((odd  (1))
       (even (2))
       (Odd  (3))
       (Even (4))
       (ODD  (5))
       (EVEN (6)))
      |}];
    test [ "odd", 1; "Odd", 3; "ODD", 5; "even", 2; "Even", 4; "EVEN", 6 ];
    [%expect
      {|
      ((odd  (1 3 5))
       (even (2 4 6)))
      |}]
  ;;
end

module%test [@name "Assoc.sort_and_group"] _ = struct
  let%expect_test _ =
    let test alist =
      let multi = Assoc.sort_and_group alist ~compare:String.Caseless.compare in
      print_s [%sexp (multi : (string * int list) list)];
      require_equal
        (module struct
          type t = (string * int list) list [@@deriving equal, sexp_of]
        end)
        multi
        (Map.to_alist (Map.of_alist_multi (module String.Caseless) alist))
    in
    test [];
    [%expect {| () |}];
    test [ "a", 1; "A", 2 ];
    [%expect {| ((a (1 2))) |}];
    test [ "a", 1; "b", 2 ];
    [%expect
      {|
      ((a (1))
       (b (2)))
      |}];
    test [ "odd", 1; "even", 2; "Odd", 3; "Even", 4; "ODD", 5; "EVEN", 6 ];
    [%expect
      {|
      ((even (2 4 6))
       (odd  (1 3 5)))
      |}]
  ;;
end

module%test [@name "chunk_evenly"] _ = struct
  let%expect_test "show bucketing behavior and ensure stable" =
    let sexps =
      List.init 10 ~f:(fun n ->
        let items = List.init n ~f:(fun i -> i) in
        let result = List.chunk_evenly items ~into:3 in
        let lengths = List.map result ~f:List.length in
        [%message (Int.to_string n) (lengths : int list) (result : int list list)])
    in
    Dynamic.with_temporarily sexp_style Sexp_style.simple_pretty ~f:(fun () ->
      print_s (List sexps));
    [%expect
      {|
      ((0 (lengths (0 0 0)) (result (() () ())))
       (1 (lengths (1 0 0)) (result ((0) () ())))
       (2 (lengths (1 1 0)) (result ((0) (1) ())))
       (3 (lengths (1 1 1)) (result ((0) (1) (2))))
       (4 (lengths (2 1 1)) (result ((0 1) (2) (3))))
       (5 (lengths (2 2 1)) (result ((0 1) (2 3) (4))))
       (6 (lengths (2 2 2)) (result ((0 1) (2 3) (4 5))))
       (7 (lengths (3 2 2)) (result ((0 1 2) (3 4) (5 6))))
       (8 (lengths (3 3 2)) (result ((0 1 2) (3 4 5) (6 7))))
       (9 (lengths (3 3 3)) (result ((0 1 2) (3 4 5) (6 7 8)))))
      |}]
  ;;

  let%expect_test "raises on invalid arguments" =
    let expect_raises into =
      match List.chunk_evenly [ 1; 2; 3 ] ~into with
      | _ -> failwith "did not raise"
      | exception exn -> Exn.to_string exn |> print_endline
    in
    expect_raises 0;
    [%expect {| (Invalid_argument "List.chunk_evenly: Expected [into] > 0, got 0") |}];
    expect_raises (-32);
    [%expect {| (Invalid_argument "List.chunk_evenly: Expected [into] > 0, got -32") |}]
  ;;
end

module%test [@name "chunks_of"] _ = struct
  let test length break_every =
    let l = List.init length ~f:Fn.id in
    let b = chunks_of l ~length:break_every in
    [%test_eq: int list] (List.concat b) l;
    List.iter
      b
      ~f:([%test_pred: int list] (fun batch -> List.length batch <= break_every))
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
    expect_exn 1 (-1)
  ;;

  let%test_unit _ = [%test_result: _ list list] (chunks_of [] ~length:1) ~expect:[]
end

let%test _ = last_exn [ 1; 2; 3 ] = 3
let%test _ = last_exn [ 1 ] = 1
let%test _ = last_exn (Test_values.long1 ()) = 99_999
let%test _ = is_prefix [] ~prefix:[] ~equal:( = )
let%test _ = is_prefix [ 1 ] ~prefix:[] ~equal:( = )
let%test _ = is_prefix [ 1 ] ~prefix:[ 1 ] ~equal:( = )
let%test _ = not (is_prefix [ 1 ] ~prefix:[ 1; 2 ] ~equal:( = ))
let%test _ = not (is_prefix [ 1; 3 ] ~prefix:[ 1; 2 ] ~equal:( = ))
let%test _ = is_prefix [ 1; 2; 3 ] ~prefix:[ 1; 2 ] ~equal:( = )
let%test _ = is_suffix [] ~suffix:[] ~equal:( = )
let%test _ = is_suffix [ 1 ] ~suffix:[] ~equal:( = )
let%test _ = is_suffix [ 1 ] ~suffix:[ 1 ] ~equal:( = )
let%test _ = not (is_suffix [ 1 ] ~suffix:[ 1; 2 ] ~equal:( = ))
let%test _ = not (is_suffix [ 1; 3 ] ~suffix:[ 1; 2 ] ~equal:( = ))
let%test _ = is_suffix [ 1; 2; 3 ] ~suffix:[ 2; 3 ] ~equal:( = )

let%test_unit _ =
  List.iter
    ~f:(fun (t, expect) ->
      assert (Poly.equal expect (find_consecutive_duplicate t ~equal:Poly.equal)))
    [ [], None
    ; [ 1 ], None
    ; [ 1; 1 ], Some (1, 1)
    ; [ 1; 2 ], None
    ; [ 1; 2; 1 ], None
    ; [ 1; 2; 2 ], Some (2, 2)
    ; [ 1; 1; 2; 2 ], Some (1, 1)
    ]
;;

let%test_unit _ =
  [%test_result: ((int * char) * (int * char)) option]
    (find_consecutive_duplicate
       [ 0, 'a'; 1, 'b'; 2, 'b' ]
       ~equal:(fun (_, a) (_, b) -> Char.( = ) a b))
    ~expect:(Some ((1, 'b'), (2, 'b')))
;;

let%test_unit _ =
  [%test_result: int list]
    (remove_consecutive_duplicates ~which_to_keep:`Last [] ~equal:Int.( = ))
    ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list]
    (remove_consecutive_duplicates
       ~which_to_keep:`Last
       [ 5; 5; 5; 5; 5 ]
       ~equal:Int.( = ))
    ~expect:[ 5 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (remove_consecutive_duplicates
       ~which_to_keep:`Last
       [ 5; 6; 5; 6; 5; 6 ]
       ~equal:Int.( = ))
    ~expect:[ 5; 6; 5; 6; 5; 6 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (remove_consecutive_duplicates
       ~which_to_keep:`Last
       [ 5; 5; 6; 6; 5; 5; 8; 8 ]
       ~equal:Int.( = ))
    ~expect:[ 5; 6; 5; 8 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (remove_consecutive_duplicates
       ~which_to_keep:`Last
       [ 0, 1; 0, 2; 2, 2; 4, 1 ]
       ~equal:(fun (a, _) (b, _) -> Int.( = ) a b))
    ~expect:[ 0, 2; 2, 2; 4, 1 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (remove_consecutive_duplicates
       ~which_to_keep:`Last
       [ 0, 1; 2, 2; 0, 2; 4, 1 ]
       ~equal:(fun (a, _) (b, _) -> Int.( = ) a b))
    ~expect:[ 0, 1; 2, 2; 0, 2; 4, 1 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (remove_consecutive_duplicates
       ~which_to_keep:`Last
       [ 0, 1; 2, 1; 0, 2; 4, 2 ]
       ~equal:(fun (_, a) (_, b) -> Int.( = ) a b))
    ~expect:[ 2, 1; 4, 2 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (remove_consecutive_duplicates
       ~which_to_keep:`Last
       [ 0, 1; 2, 2; 0, 2; 4, 1 ]
       ~equal:(fun (_, a) (_, b) -> Int.( = ) a b))
    ~expect:[ 0, 1; 0, 2; 4, 1 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (remove_consecutive_duplicates ~which_to_keep:`First [] ~equal:Int.( = ))
    ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list]
    (remove_consecutive_duplicates
       ~which_to_keep:`First
       [ 5; 5; 5; 5; 5 ]
       ~equal:Int.( = ))
    ~expect:[ 5 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (remove_consecutive_duplicates
       ~which_to_keep:`First
       [ 5; 6; 5; 6; 5; 6 ]
       ~equal:Int.( = ))
    ~expect:[ 5; 6; 5; 6; 5; 6 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (remove_consecutive_duplicates
       ~which_to_keep:`First
       [ 5; 5; 6; 6; 5; 5; 8; 8 ]
       ~equal:Int.( = ))
    ~expect:[ 5; 6; 5; 8 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (remove_consecutive_duplicates
       ~which_to_keep:`First
       [ 0, 1; 0, 2; 2, 2; 4, 1 ]
       ~equal:(fun (a, _) (b, _) -> Int.( = ) a b))
    ~expect:[ 0, 1; 2, 2; 4, 1 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (remove_consecutive_duplicates
       ~which_to_keep:`First
       [ 0, 1; 2, 2; 0, 2; 4, 1 ]
       ~equal:(fun (a, _) (b, _) -> Int.( = ) a b))
    ~expect:[ 0, 1; 2, 2; 0, 2; 4, 1 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (remove_consecutive_duplicates
       ~which_to_keep:`First
       [ 0, 1; 2, 1; 0, 2; 4, 2 ]
       ~equal:(fun (_, a) (_, b) -> Int.( = ) a b))
    ~expect:[ 0, 1; 0, 2 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (remove_consecutive_duplicates
       ~which_to_keep:`First
       [ 0, 1; 2, 2; 0, 2; 4, 1 ]
       ~equal:(fun (_, a) (_, b) -> Int.( = ) a b))
    ~expect:[ 0, 1; 2, 2; 4, 1 ]
;;

let%test_unit _ =
  [%test_result: int list] (dedup_and_sort ~compare:Int.compare []) ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list]
    (dedup_and_sort ~compare:Int.compare [ 5; 5; 5; 5; 5 ])
    ~expect:[ 5 ]
;;

let%test_unit _ =
  [%test_result: int]
    (length (dedup_and_sort ~compare:Int.compare [ 2; 1; 5; 3; 4 ]))
    ~expect:5
;;

let%test_unit _ =
  [%test_result: int]
    (length (dedup_and_sort ~compare:Int.compare [ 2; 3; 5; 3; 4 ]))
    ~expect:4
;;

let%test_unit _ =
  [%test_result: int]
    (length
       (dedup_and_sort
          [ 0, 1; 2, 2; 0, 2; 4, 1 ]
          ~compare:(fun (a, _) (b, _) -> Int.compare a b)))
    ~expect:3
;;

let%test_unit _ =
  [%test_result: int]
    (length
       (dedup_and_sort
          [ 0, 1; 2, 2; 0, 2; 4, 1 ]
          ~compare:(fun (_, a) (_, b) -> Int.compare a b)))
    ~expect:2
;;

let%test_unit _ =
  [%test_result: int option] (find_a_dup ~compare:Int.compare []) ~expect:None
;;

let%test_unit _ =
  [%test_result: int option] (find_a_dup ~compare:Int.compare [ 3 ]) ~expect:None
;;

let%test_unit _ =
  [%test_result: int option] (find_a_dup ~compare:Int.compare [ 3; 4 ]) ~expect:None
;;

let%test_unit _ =
  [%test_result: int option] (find_a_dup ~compare:Int.compare [ 3; 3 ]) ~expect:(Some 3)
;;

let%test_unit _ =
  [%test_result: int option]
    (find_a_dup ~compare:Int.compare [ 3; 5; 4; 6; 12 ])
    ~expect:None
;;

let%test_unit _ =
  [%test_result: int option]
    (find_a_dup ~compare:Int.compare [ 3; 5; 4; 5; 12 ])
    ~expect:(Some 5)
;;

let%test_unit _ =
  [%test_result: int option]
    (find_a_dup ~compare:Int.compare [ 3; 5; 12; 5; 12 ])
    ~expect:(Some 5)
;;

let%test_unit _ =
  [%test_result: (int * int) option]
    (find_a_dup ~compare:[%compare: int * int] [ 0, 1; 2, 2; 0, 2; 4, 1 ])
    ~expect:None
;;

let%test _ =
  find_a_dup [ 0, 1; 2, 2; 0, 2; 4, 1 ] ~compare:(fun (_, a) (_, b) -> Int.compare a b)
  |> Option.is_some
;;

let%test _ =
  let dup =
    find_a_dup [ 0, 1; 2, 2; 0, 2; 4, 1 ] ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  match dup with
  | Some (0, _) -> true
  | _ -> false
;;

let%test_unit _ =
  [%test_result: bool] (contains_dup ~compare:Int.compare []) ~expect:false
;;

let%test_unit _ =
  [%test_result: bool] (contains_dup ~compare:Int.compare [ 3 ]) ~expect:false
;;

let%test_unit _ =
  [%test_result: bool] (contains_dup ~compare:Int.compare [ 3; 4 ]) ~expect:false
;;

let%test_unit _ =
  [%test_result: bool] (contains_dup ~compare:Int.compare [ 3; 3 ]) ~expect:true
;;

let%test_unit _ =
  [%test_result: bool]
    (contains_dup ~compare:Int.compare [ 3; 5; 4; 6; 12 ])
    ~expect:false
;;

let%test_unit _ =
  [%test_result: bool] (contains_dup ~compare:Int.compare [ 3; 5; 4; 5; 12 ]) ~expect:true
;;

let%test_unit _ =
  [%test_result: bool]
    (contains_dup ~compare:Int.compare [ 3; 5; 12; 5; 12 ])
    ~expect:true
;;

let%test_unit _ =
  [%test_result: bool]
    (contains_dup ~compare:[%compare: int * int] [ 0, 1; 2, 2; 0, 2; 4, 1 ])
    ~expect:false
;;

let%test_unit _ =
  [%test_result: bool]
    (contains_dup
       [ 0, 1; 2, 2; 0, 2; 4, 1 ]
       ~compare:(fun (_, a) (_, b) -> Int.compare a b))
    ~expect:true
;;

let%test_unit _ =
  [%test_result: int list] (find_all_dups ~compare:Int.compare []) ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list] (find_all_dups ~compare:Int.compare [ 3 ]) ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list] (find_all_dups ~compare:Int.compare [ 3; 4 ]) ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list] (find_all_dups ~compare:Int.compare [ 3; 3 ]) ~expect:[ 3 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (find_all_dups ~compare:Int.compare [ 3; 5; 4; 6; 12 ])
    ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list]
    (find_all_dups ~compare:Int.compare [ 3; 5; 4; 5; 12 ])
    ~expect:[ 5 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (find_all_dups ~compare:Int.compare [ 3; 5; 12; 5; 12 ])
    ~expect:[ 5; 12 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (find_all_dups ~compare:[%compare: int * int] [ 0, 1; 2, 2; 0, 2; 4, 1 ])
    ~expect:[]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (find_all_dups [ 0, 1; 2, 2; 0, 2; 4, 1 ] ~compare:[%compare: _ * int])
    ~expect:[ 4, 1; 0, 2 ]
;;

let%test_unit _ =
  [%test_result: (int * int) list]
    (find_all_dups [ 0, 1; 2, 2; 0, 2; 4, 1 ] ~compare:[%compare: int * _])
    ~expect:[ 0, 2 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (filter_map ~f:(fun x -> Some x) Test_values.l1)
    ~expect:Test_values.l1
;;

let%test_unit _ = [%test_result: int list] (filter_map ~f:(fun x -> Some x) []) ~expect:[]

let%test_unit _ =
  [%test_result: int list] (filter_map ~f:(fun _x -> None) [ 1.; 2.; 3. ]) ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list]
    (filter_map ~f:(fun x -> if x > 0 then Some x else None) [ 1; -1; 3 ])
    ~expect:[ 1; 3 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (filter_mapi ~f:(fun _i x -> Some x) Test_values.l1)
    ~expect:Test_values.l1
;;

let%test_unit _ =
  [%test_result: int list] (filter_mapi ~f:(fun _i x -> Some x) []) ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list] (filter_mapi ~f:(fun _i _x -> None) [ 1.; 2.; 3. ]) ~expect:[]
;;

let%test_unit _ =
  [%test_result: int list]
    (filter_mapi ~f:(fun _i x -> if x > 0 then Some x else None) [ 1; -1; 3 ])
    ~expect:[ 1; 3 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (filter_mapi ~f:(fun i x -> if i % 2 = 0 then Some x else None) [ 1; -1; 3 ])
    ~expect:[ 1; 3 ]
;;

let%test_unit _ =
  [%test_result: int list * int list]
    (split_n [ 1; 2; 3; 4; 5; 6 ] 3)
    ~expect:([ 1; 2; 3 ], [ 4; 5; 6 ])
;;

let%test_unit _ =
  [%test_result: int list * int list]
    (split_n [ 1; 2; 3; 4; 5; 6 ] 100)
    ~expect:([ 1; 2; 3; 4; 5; 6 ], [])
;;

let%test_unit _ =
  [%test_result: int list * int list]
    (split_n [ 1; 2; 3; 4; 5; 6 ] 0)
    ~expect:([], [ 1; 2; 3; 4; 5; 6 ])
;;

let%test_unit _ =
  [%test_result: int list * int list]
    (split_n [ 1; 2; 3; 4; 5; 6 ] (-5))
    ~expect:([], [ 1; 2; 3; 4; 5; 6 ])
;;

let%test_unit _ =
  [%test_result: int list] (take [ 1; 2; 3; 4; 5; 6 ] 3) ~expect:[ 1; 2; 3 ]
;;

let%test_unit _ =
  [%test_result: int list] (take [ 1; 2; 3; 4; 5; 6 ] 100) ~expect:[ 1; 2; 3; 4; 5; 6 ]
;;

let%test_unit _ = [%test_result: int list] (take [ 1; 2; 3; 4; 5; 6 ] 0) ~expect:[]
let%test_unit _ = [%test_result: int list] (take [ 1; 2; 3; 4; 5; 6 ] (-5)) ~expect:[]

let%test_unit _ =
  [%test_result: int list] (drop [ 1; 2; 3; 4; 5; 6 ] 3) ~expect:[ 4; 5; 6 ]
;;

let%test_unit _ = [%test_result: int list] (drop [ 1; 2; 3; 4; 5; 6 ] 100) ~expect:[]

let%test_unit _ =
  [%test_result: int list] (drop [ 1; 2; 3; 4; 5; 6 ] 0) ~expect:[ 1; 2; 3; 4; 5; 6 ]
;;

let%test_unit _ =
  [%test_result: int list] (drop [ 1; 2; 3; 4; 5; 6 ] (-5)) ~expect:[ 1; 2; 3; 4; 5; 6 ]
;;

module%test [@name "{take,drop,split}_while"] _ = struct
  let pred = function
    | '0' .. '9' -> true
    | _ -> false
  ;;

  let test xs prefix suffix =
    let prefix1, suffix1 = split_while ~f:pred xs in
    let prefix2 = take_while xs ~f:pred in
    let suffix2 = drop_while xs ~f:pred in
    [%test_eq: char list] xs (prefix @ suffix);
    [%test_result: char list] ~expect:prefix prefix1;
    [%test_result: char list] ~expect:prefix prefix2;
    [%test_result: char list] ~expect:suffix suffix1;
    [%test_result: char list] ~expect:suffix suffix2
  ;;

  let%test_unit _ =
    test [ '1'; '2'; '3'; 'a'; 'b'; 'c' ] [ '1'; '2'; '3' ] [ 'a'; 'b'; 'c' ]
  ;;

  let%test_unit _ = test [ '1'; '2'; 'a'; 'b'; 'c' ] [ '1'; '2' ] [ 'a'; 'b'; 'c' ]
  let%test_unit _ = test [ '1'; 'a'; 'b'; 'c' ] [ '1' ] [ 'a'; 'b'; 'c' ]
  let%test_unit _ = test [ 'a'; 'b'; 'c' ] [] [ 'a'; 'b'; 'c' ]
  let%test_unit _ = test [ '1'; '2'; '3' ] [ '1'; '2'; '3' ] []
  let%test_unit _ = test [] [] []
end

let%test_unit _ = [%test_result: int list] (concat []) ~expect:[]
let%test_unit _ = [%test_result: int list] (concat [ [] ]) ~expect:[]
let%test_unit _ = [%test_result: int list] (concat [ [ 3 ] ]) ~expect:[ 3 ]

let%test_unit _ =
  [%test_result: int list] (concat [ [ 1; 2; 3; 4 ] ]) ~expect:[ 1; 2; 3; 4 ]
;;

let%test_unit _ =
  [%test_result: int list]
    (concat [ [ 1; 2; 3; 4 ]; [ 5; 6; 7 ]; [ 8; 9; 10 ]; []; [ 11; 12 ] ])
    ~expect:[ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12 ]
;;

let%test_unit _ = [%test_result: bool] (is_sorted [] ~compare:Int.compare) ~expect:true
let%test_unit _ = [%test_result: bool] (is_sorted [ 1 ] ~compare:Int.compare) ~expect:true

let%test_unit _ =
  [%test_result: bool] (is_sorted [ 1; 2; 3; 4 ] ~compare:Int.compare) ~expect:true
;;

let%test_unit _ =
  [%test_result: bool] (is_sorted [ 2; 1 ] ~compare:Int.compare) ~expect:false
;;

let%test_unit _ =
  [%test_result: bool] (is_sorted [ 1; 3; 2 ] ~compare:Int.compare) ~expect:false
;;

let%test_unit _ =
  List.iter
    ~f:(fun (t, expect) ->
      [%test_result: bool] ~expect (is_sorted_strictly t ~compare:Int.compare))
    [ [], true
    ; [ 1 ], true
    ; [ 1; 2 ], true
    ; [ 1; 1 ], false
    ; [ 2; 1 ], false
    ; [ 1; 2; 3 ], true
    ; [ 1; 1; 3 ], false
    ; [ 1; 2; 2 ], false
    ]
;;

let%test_unit _ = [%test_result: int option] (random_element []) ~expect:None
let%test_unit _ = [%test_result: int option] (random_element [ 0 ]) ~expect:(Some 0)

module%test [@name "transpose"] _ = struct
  let round_trip a b =
    [%test_result: int list list option] (transpose a) ~expect:(Some b);
    [%test_result: int list list option] (transpose b) ~expect:(Some a)
  ;;

  let%test_unit _ = round_trip [] []

  let%test_unit _ =
    [%test_result: int list list option] (transpose [ [] ]) ~expect:(Some [])
  ;;

  let%test_unit _ =
    [%test_result: int list list option] (transpose [ []; [] ]) ~expect:(Some [])
  ;;

  let%test_unit _ =
    [%test_result: int list list option] (transpose [ []; []; [] ]) ~expect:(Some [])
  ;;

  let%test_unit _ = round_trip [ [ 1 ] ] [ [ 1 ] ]
  let%test_unit _ = round_trip [ [ 1 ]; [ 2 ] ] [ [ 1; 2 ] ]
  let%test_unit _ = round_trip [ [ 1 ]; [ 2 ]; [ 3 ] ] [ [ 1; 2; 3 ] ]
  let%test_unit _ = round_trip [ [ 1; 2 ]; [ 3; 4 ] ] [ [ 1; 3 ]; [ 2; 4 ] ]

  let%test_unit _ =
    round_trip [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] [ [ 1; 4 ]; [ 2; 5 ]; [ 3; 6 ] ]
  ;;

  let%test_unit _ =
    round_trip
      [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]
      [ [ 1; 4; 7 ]; [ 2; 5; 8 ]; [ 3; 6; 9 ] ]
  ;;

  let%test_unit _ =
    round_trip
      [ [ 1; 2; 3; 4 ]; [ 5; 6; 7; 8 ]; [ 9; 10; 11; 12 ] ]
      [ [ 1; 5; 9 ]; [ 2; 6; 10 ]; [ 3; 7; 11 ]; [ 4; 8; 12 ] ]
  ;;

  let%test_unit _ =
    round_trip
      [ [ 1; 2; 3; 4 ]; [ 5; 6; 7; 8 ]; [ 9; 10; 11; 12 ]; [ 13; 14; 15; 16 ] ]
      [ [ 1; 5; 9; 13 ]; [ 2; 6; 10; 14 ]; [ 3; 7; 11; 15 ]; [ 4; 8; 12; 16 ] ]
  ;;

  let%test_unit _ =
    round_trip
      [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ]; [ 10; 11; 12 ] ]
      [ [ 1; 4; 7; 10 ]; [ 2; 5; 8; 11 ]; [ 3; 6; 9; 12 ] ]
  ;;

  let%test_unit _ =
    [%test_result: int list list option] (transpose [ []; [ 1 ] ]) ~expect:None
  ;;

  let%test_unit _ =
    [%test_result: int list list option] (transpose [ [ 1; 2 ]; [ 3 ] ]) ~expect:None
  ;;
end

let%test_unit _ =
  [%test_result: int list] (intersperse [ 1; 2; 3 ] ~sep:0) ~expect:[ 1; 0; 2; 0; 3 ]
;;

let%test_unit _ =
  [%test_result: int list] (intersperse [ 1; 2 ] ~sep:0) ~expect:[ 1; 0; 2 ]
;;

let%test_unit _ = [%test_result: int list] (intersperse [ 1 ] ~sep:0) ~expect:[ 1 ]
let%test_unit _ = [%test_result: int list] (intersperse [] ~sep:0) ~expect:[]

let test_fold_map list ~init ~f ~expect =
  [%test_result: int list] (folding_map list ~init ~f) ~expect:(snd expect);
  [%test_result: _ * int list] (fold_map list ~init ~f) ~expect
;;

let test_fold_mapi list ~init ~f ~expect =
  [%test_result: int list] (folding_mapi list ~init ~f) ~expect:(snd expect);
  [%test_result: _ * int list] (fold_mapi list ~init ~f) ~expect
;;

let%test_unit _ =
  test_fold_map
    [ 1; 2; 3; 4 ]
    ~init:0
    ~f:(fun acc x ->
      let y = acc + x in
      y, y)
    ~expect:(10, [ 1; 3; 6; 10 ])
;;

let%test_unit _ =
  test_fold_map
    []
    ~init:0
    ~f:(fun acc x ->
      let y = acc + x in
      y, y)
    ~expect:(0, [])
;;

let%test_unit _ =
  test_fold_mapi
    [ 1; 2; 3; 4 ]
    ~init:0
    ~f:(fun i acc x ->
      let y = acc + (i * x) in
      y, y)
    ~expect:(20, [ 0; 2; 8; 20 ])
;;

let%test_unit _ =
  test_fold_mapi
    []
    ~init:0
    ~f:(fun i acc x ->
      let y = acc + (i * x) in
      y, y)
    ~expect:(0, [])
;;

let test_partition_mapi list ~f ~expect =
  [%test_result: int list * int list] (partition_mapi list ~f) ~expect
;;

let%test_unit _ =
  test_partition_mapi
    []
    ~f:(fun i x -> if i = x then First (i + x) else Second (i - x))
    ~expect:([], [])
;;

let%test_unit _ =
  test_partition_mapi
    [ 3; 5; 2; 1; 4 ]
    ~f:(fun i x -> if i = x then First (i + x) else Second (i - x))
    ~expect:([ 4; 8 ], [ -3; -4; 2 ])
;;

let test_partitioni_tf list ~f ~expect =
  [%test_result: int list * int list] (partitioni_tf list ~f) ~expect
;;

let%test_unit _ = test_partitioni_tf [] ~f:(fun i x -> i = x && x > 2) ~expect:([], [])

let%test_unit _ =
  test_partitioni_tf
    [ 3; 5; 2; 1; 4 ]
    ~f:(fun i x -> i = x && x > 2)
    ~expect:([ 4 ], [ 3; 5; 2; 1 ])
;;

let%expect_test "drop_last" =
  let print_drop_last x = print_s [%sexp (List.drop_last x : int list option)] in
  print_drop_last [];
  [%expect {| () |}];
  print_drop_last [ 1 ];
  [%expect {| (()) |}];
  print_drop_last [ 1; 2; 3 ];
  [%expect {| ((1 2)) |}]
;;

let%expect_test "drop_last_exn" =
  let print_drop_last_exn x = print_s [%sexp (List.drop_last_exn x : int list)] in
  require_does_raise (fun () -> print_drop_last_exn []);
  [%expect {| (Failure "List.drop_last_exn: empty list") |}];
  require_does_not_raise (fun () -> print_drop_last_exn [ 1 ]);
  [%expect {| () |}]
;;

let%expect_test "[all_equal]" =
  let test list =
    print_s [%sexp (all_equal list ~equal:Char.Caseless.equal : char option)]
  in
  (* empty list *)
  test [];
  [%expect {| () |}];
  (* singleton *)
  test [ 'a' ];
  [%expect {| (a) |}];
  (* homogenous pairs (up to [equal]) *)
  test [ 'a'; 'a' ];
  [%expect {| (a) |}];
  test [ 'a'; 'A' ];
  [%expect {| (a) |}];
  test [ 'A'; 'a' ];
  [%expect {| (A) |}];
  (* heterogenous pairs *)
  test [ 'a'; 'b' ];
  [%expect {| () |}];
  test [ 'b'; 'a' ];
  [%expect {| () |}];
  (* heterogenous lists *)
  test [ 'a'; 'b'; 'a'; 'b'; 'a'; 'b' ];
  [%expect {| () |}];
  test [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ];
  [%expect {| () |}];
  (* homogenous lists (up to [equal]) *)
  test [ 'a'; 'a'; 'a'; 'a'; 'a'; 'a' ];
  [%expect {| (a) |}];
  test [ 'A'; 'a'; 'A'; 'a'; 'A'; 'a' ];
  [%expect {| (A) |}]
;;

let%expect_test "[Cartesian_product.apply] identity" =
  let test list =
    require_equal
      (module struct
        type t = char list [@@deriving equal, sexp_of]
      end)
      list
      (List.Cartesian_product.apply (return Fn.id) list)
  in
  test [];
  test [ 'a'; 'b'; 'c' ];
  test [ 'a'; 'z'; 'd'; 'b' ]
;;

let%expect_test "[Cartesian_product]" =
  (let%map.List.Cartesian_product letter = [ 'a'; 'b'; 'c' ]
   and number = [ 1; 2; 3 ]
   and solfege = [ "do"; "re"; "mi" ] in
   [%sexp (letter : char), (number : int), (solfege : string)])
  |> List.iter ~f:print_s;
  [%expect
    {|
    (a 1 do)
    (a 1 re)
    (a 1 mi)
    (a 2 do)
    (a 2 re)
    (a 2 mi)
    (a 3 do)
    (a 3 re)
    (a 3 mi)
    (b 1 do)
    (b 1 re)
    (b 1 mi)
    (b 2 do)
    (b 2 re)
    (b 2 mi)
    (b 3 do)
    (b 3 re)
    (b 3 mi)
    (c 1 do)
    (c 1 re)
    (c 1 mi)
    (c 2 do)
    (c 2 re)
    (c 2 mi)
    (c 3 do)
    (c 3 re)
    (c 3 mi)
    |}]
;;

let%expect_test "[compare__local] is the same as [compare]" =
  Base_quickcheck.Test.run_exn
    (module struct
      type t = int list * int list [@@deriving sexp_of, quickcheck]
    end)
    ~f:(fun (l1, l2) ->
      require_equal
        (module Int)
        (compare Int.compare l1 l2)
        (compare__local Int.compare__local l1 l2));
  [%expect {| |}]
;;

let%expect_test "[equal__local] is the same as [equal]" =
  Base_quickcheck.Test.run_exn
    (module struct
      type t = int list * int list [@@deriving sexp_of, quickcheck]
    end)
    ~f:(fun (l1, l2) ->
      require_equal
        (module Bool)
        (equal Int.equal l1 l2)
        (equal__local Int.equal__local l1 l2));
  [%expect {| |}]
;;

let%expect_test "list sort, dedup" =
  let slow_stable_sort list ~compare =
    let rec insert elt list =
      match list with
      | [] -> [ elt ]
      | head :: tail ->
        (match compare elt head with
         | c when c <= 0 -> elt :: list
         | _ -> head :: insert elt tail)
    in
    List.fold_right list ~init:[] ~f:insert
  in
  let slow_sort list ~compare =
    (* sort happens to behave the same as stable_sort *)
    let rec insert elt list =
      match list with
      | [] -> [ elt ]
      | head :: tail ->
        (match compare elt head with
         | c when c <= 0 -> elt :: list
         | _ -> head :: insert elt tail)
    in
    List.fold_right list ~init:[] ~f:insert
  in
  let slow_dedup_and_sort list ~compare =
    (* dedup_and_sort keeps the last element among duplicates *)
    let rec insert elt list =
      match list with
      | [] -> [ elt ]
      | head :: tail ->
        (match compare elt head with
         | 0 -> list
         | c when c < 0 -> elt :: list
         | _ -> head :: insert elt tail)
    in
    List.fold_right list ~init:[] ~f:insert
  in
  let slow_stable_dedup list ~compare =
    (* stable_dedup keeps the first element among duplicates *)
    let insert elt list =
      elt :: List.filter list ~f:(fun other -> compare elt other <> 0)
    in
    List.fold_right list ~init:[] ~f:insert
  in
  let module Char_list = struct
    open Base_quickcheck

    type t = char list [@@deriving equal, sexp_of]

    let quickcheck_generator = Generator.list_non_empty Generator.char_alpha

    let quickcheck_shrinker =
      let prev char = Char.of_int_exn (Int.pred (Char.to_int char)) in
      Shrinker.list
        (Shrinker.create (function
          | 'a' -> Sequence.empty
          | 'b' .. 'z' as char -> Sequence.singleton (prev char)
          | 'A' -> Sequence.singleton 'a'
          | 'B' .. 'Z' as char -> Sequence.of_list [ Char.lowercase char; prev char ]
          | _ -> Sequence.empty))
    ;;
  end
  in
  let compare = Char.Caseless.compare in
  quickcheck_m
    ~examples:[ []; [ 'a'; 'a' ]; [ 'a'; 'A' ]; [ 'A'; 'a' ]; [ 'A'; 'A' ] ]
    (module Char_list)
    ~f:(fun list ->
      require_equal
        (module Char_list)
        ~message:"sort mismatch"
        (List.sort list ~compare)
        (slow_sort list ~compare);
      require_equal
        (module Char_list)
        ~message:"stable_sort mismatch"
        (List.stable_sort list ~compare)
        (slow_stable_sort list ~compare);
      require_equal
        (module Char_list)
        ~message:"dedup_and_sort mismatch"
        (List.dedup_and_sort list ~compare)
        (slow_dedup_and_sort list ~compare);
      require_equal
        (module Char_list)
        ~message:"stable_dedup mismatch"
        (List.stable_dedup list ~compare)
        (slow_stable_dedup list ~compare))
;;

let%expect_test "[take], [drop], and [split]" =
  for whole_len = 0 to 3 do
    let whole = List.init whole_len ~f:Fn.id in
    let test name kind requested_length list =
      let expected_length = Int.clamp_exn requested_length ~min:0 ~max:whole_len in
      let phys_equal_whole = phys_equal list whole in
      let check problem bool =
        require
          bool
          ~if_false_then_print_s:
            [%lazy_message
              name
                ~problem
                (whole : int list)
                (requested_length : int)
                (expected_length : int)
                (phys_equal_whole : bool)
                (list : int list)]
      in
      check "wrong length" (List.length list = expected_length);
      if expected_length = whole_len then check "not phys_equal" phys_equal_whole;
      match kind with
      | `prefix ->
        check "is not a prefix" (List.is_prefix whole ~prefix:list ~equal:( = ))
      | `suffix ->
        check "is not a suffix" (List.is_suffix whole ~suffix:list ~equal:( = ))
    in
    for prefix_len = -1 to 2 do
      let suffix_len = whole_len - prefix_len in
      test "take" `prefix prefix_len (List.take whole prefix_len);
      test "drop" `suffix suffix_len (List.drop whole prefix_len);
      let prefix, suffix = List.split_n whole prefix_len in
      test "split prefix" `prefix prefix_len prefix;
      test "split suffix" `suffix suffix_len suffix
    done
  done;
  [%expect {| |}]
;;

let print_s sexp =
  Dynamic.with_temporarily sexp_style Sexp_style.simple_pretty ~f:(fun () -> print_s sexp)
;;

let%expect_test "[cartesian_product]" =
  let test xs ys = print_s [%sexp (cartesian_product xs ys : (int * int) list)] in
  test [] [];
  [%expect {| () |}];
  test [ 1; 2; 3 ] [];
  [%expect {| () |}];
  test [] [ 1; 2; 3 ];
  [%expect {| () |}];
  test [ 1 ] [ 2; 3; 4 ];
  [%expect {| ((1 2) (1 3) (1 4)) |}];
  test [ 1; 2; 3 ] [ 4 ];
  [%expect {| ((1 4) (2 4) (3 4)) |}];
  test [ 1; 2 ] [ 3; 4; 5 ];
  [%expect {| ((1 3) (1 4) (1 5) (2 3) (2 4) (2 5)) |}];
  test [ 1; 2; 3 ] [ 4; 5 ];
  [%expect {| ((1 4) (1 5) (2 4) (2 5) (3 4) (3 5)) |}];
  test [ 1; 2; 3 ] [ 4; 5; 6 ];
  [%expect {| ((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6)) |}]
;;

let%expect_test "[concat_map]" =
  let test list =
    List.concat_map list ~f:(fun n -> List.init n ~f:Int.succ)
    |> [%sexp_of: int list]
    |> print_s
  in
  test [];
  [%expect {| () |}];
  test [ 1 ];
  [%expect {| (1) |}];
  test [ 1; 2 ];
  [%expect {| (1 1 2) |}];
  test [ 1; 2; 3 ];
  [%expect {| (1 1 2 1 2 3) |}];
  test [ 1; 2; 3; 4 ];
  [%expect {| (1 1 2 1 2 3 1 2 3 4) |}];
  test [ 4; 5; 6 ];
  [%expect {| (1 2 3 4 1 2 3 4 5 1 2 3 4 5 6) |}]
;;

let%expect_test "[concat_mapi]" =
  let test list =
    List.concat_mapi list ~f:(fun i n -> List.init n ~f:(( + ) i))
    |> [%sexp_of: int list]
    |> print_s
  in
  test [];
  [%expect {| () |}];
  test [ 1 ];
  [%expect {| (0) |}];
  test [ 1; 2 ];
  [%expect {| (0 1 2) |}];
  test [ 1; 2; 3 ];
  [%expect {| (0 1 2 2 3 4) |}];
  test [ 1; 2; 3; 4 ];
  [%expect {| (0 1 2 2 3 4 3 4 5 6) |}];
  test [ 4; 5; 6 ];
  [%expect {| (0 1 2 3 1 2 3 4 5 2 3 4 5 6 7) |}]
;;

module%test [@name "filter{,i}"] _ = struct
  open Base_quickcheck

  module Int_list = struct
    type t = int list [@@deriving equal, sexp_of]
  end

  let%expect_test "[filter]" =
    quickcheck_m
      (module struct
        type t = int list * (int -> bool) [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (list, f) ->
        (* test [f] *)
        let pos = List.filter list ~f in
        require (List.for_all pos ~f);
        (* test [~f] *)
        let not_f = Fn.non f in
        let neg = List.filter list ~f:not_f in
        require (List.for_all neg ~f:not_f);
        (* test [f \/ ~f] *)
        let sort = sort ~compare:Int.compare in
        require_equal (module Int_list) (sort list) (sort (pos @ neg)))
  ;;

  let%expect_test "[filteri]" =
    quickcheck_m
      (module struct
        type t = int list * (int -> int -> bool) [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (list, f) ->
        let pos, neg =
          (* stash the original indices, so that we can retrieve them after filtering *)
          let list = mapi list ~f:(fun i x -> i, x) in
          let ignore_stash f : _ = fun i (_, x) -> f i x in
          let use_orig_index f : _ = fun (i, x) -> f i x in
          (* test [f] *)
          let pos = List.filteri list ~f:(ignore_stash f) in
          require (List.for_all pos ~f:(use_orig_index f));
          (* test [~f] *)
          let not_f i x = not (f i x) in
          let neg = List.filteri list ~f:(ignore_stash not_f) in
          require (List.for_all neg ~f:(use_orig_index not_f));
          pos, neg
        in
        (* test [f \/ ~f] *)
        let sort = sort ~compare:[%compare: int * _] in
        require_equal (module Int_list) list (sort (pos @ neg) |> map ~f:snd))
  ;;

  let%expect_test "[filteri ~f:(Fn.const f) = filter ~f]" =
    quickcheck_m
      (module struct
        type t = int list * (int -> bool) [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (list, f) ->
        require_equal
          (module Int_list)
          (filteri list ~f:(fun _ x -> f x))
          (filter list ~f))
  ;;

  let%expect_test "[filter]" =
    let test list =
      List.filter list ~f:(fun n -> n % 3 > 0) |> [%sexp_of: int list] |> print_s
    in
    test [];
    [%expect {| () |}];
    test [ 1 ];
    [%expect {| (1) |}];
    test [ 1; 2 ];
    [%expect {| (1 2) |}];
    test [ 1; 2; 3 ];
    [%expect {| (1 2) |}];
    test [ 1; 2; 3; 4 ];
    [%expect {| (1 2 4) |}];
    test [ 4; 5; 6 ];
    [%expect {| (4 5) |}]
  ;;

  let%expect_test "[filteri]" =
    let test list =
      List.filteri list ~f:(fun i n -> n > i) |> [%sexp_of: int list] |> print_s
    in
    test [];
    [%expect {| () |}];
    test [ 0 ];
    [%expect {| () |}];
    test [ 0; 1 ];
    [%expect {| () |}];
    test [ 0; 1; 2 ];
    [%expect {| () |}];
    test [ 1 ];
    [%expect {| (1) |}];
    test [ 1; 2 ];
    [%expect {| (1 2) |}];
    test [ 1; 2; 3 ];
    [%expect {| (1 2 3) |}];
    test [ 1; 0 ];
    [%expect {| (1) |}];
    test [ 2; 1; 0 ];
    [%expect {| (2) |}];
    test [ 3; 2; 1; 0 ];
    [%expect {| (3 2) |}]
  ;;
end

module%test [@name "count{,i}"] _ = struct
  let%expect_test "[count{,i} list ~f = List.length (filter{,i} list ~f)]" =
    quickcheck_m
      (module struct
        type t = int list * (int -> bool) [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (list, f) ->
        require_equal (module Int) (count list ~f) (length (filter list ~f)));
    quickcheck_m
      (module struct
        type t = int list * (int -> int -> bool) [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (list, f) ->
        require_equal (module Int) (counti list ~f) (length (filteri list ~f)))
  ;;

  let%test_unit _ =
    [%test_result: int] (counti [ 0; 1; 2; 3; 4 ] ~f:(fun idx x -> idx = x)) ~expect:5
  ;;

  let%test_unit _ =
    [%test_result: int] (counti [ 0; 1; 2; 3; 4 ] ~f:(fun idx x -> idx = 4 - x)) ~expect:1
  ;;
end

module%test [@name "{min,max}_elt"] _ = struct
  let test_in_list_and_forall ~tested_f ~holds_for_res_over_all_elem =
    quickcheck_m
      (module struct
        type t = int list [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun list ->
        let res = tested_f list ~compare:[%compare: int] in
        match res with
        | None -> require (is_empty list)
        | Some res ->
          require (mem list res ~equal:Int.equal);
          iter list ~f:(fun elem -> require (holds_for_res_over_all_elem ~res ~elem)))
  ;;

  let%expect_test "min_elt" =
    test_in_list_and_forall
      ~tested_f:min_elt
      ~holds_for_res_over_all_elem:(fun ~res ~elem -> res <= elem)
  ;;

  let%expect_test "max_elt" =
    test_in_list_and_forall
      ~tested_f:max_elt
      ~holds_for_res_over_all_elem:(fun ~res ~elem -> res >= elem)
  ;;
end

let%expect_test "regression test: [mem] doesn't stack overflow" =
  let n = 1_000_000 in
  let t = List.init n ~f:(fun i -> i) in
  assert (not (List.mem t n ~equal:Int.equal))
;;

let%expect_test "[map2]" =
  let test xs ys =
    map2 xs ys ~f:(fun x y -> x, y)
    |> [%sexp_of: (int * int) list Or_unequal_lengths.t]
    |> print_s
  in
  test [] [];
  [%expect {| (Ok ()) |}];
  test [ 1 ] [ 2 ];
  [%expect {| (Ok ((1 2))) |}];
  test [ 1; 2 ] [ 3; 4 ];
  [%expect {| (Ok ((1 3) (2 4))) |}];
  test [ 1; 2; 3 ] [ 4; 5; 6 ];
  [%expect {| (Ok ((1 4) (2 5) (3 6))) |}];
  test [] [ 1 ];
  [%expect {| Unequal_lengths |}];
  test [ 1 ] [];
  [%expect {| Unequal_lengths |}];
  test [ 1 ] [ 2; 3; 4 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2; 3 ] [ 4 ];
  [%expect {| Unequal_lengths |}]
;;

let%expect_test "[map3]" =
  let test xs ys zs =
    map3 xs ys zs ~f:(fun x y z -> x, y, z)
    |> [%sexp_of: (int * int * int) list Or_unequal_lengths.t]
    |> print_s
  in
  test [] [] [];
  [%expect {| (Ok ()) |}];
  test [ 1 ] [ 2 ] [ 3 ];
  [%expect {| (Ok ((1 2 3))) |}];
  test [ 1; 2 ] [ 3; 4 ] [ 5; 6 ];
  [%expect {| (Ok ((1 3 5) (2 4 6))) |}];
  test [ 1; 2; 3 ] [ 4; 5; 6 ] [ 7; 8; 9 ];
  [%expect {| (Ok ((1 4 7) (2 5 8) (3 6 9))) |}];
  test [] [] [ 1 ];
  [%expect {| Unequal_lengths |}];
  test [] [ 1 ] [];
  [%expect {| Unequal_lengths |}];
  test [] [] [ 1 ];
  [%expect {| Unequal_lengths |}];
  test [ 1 ] [ 2 ] [ 3; 4; 5 ];
  [%expect {| Unequal_lengths |}];
  test [ 1 ] [ 2; 3; 4 ] [ 5 ];
  [%expect {| Unequal_lengths |}];
  test [ 1; 2; 3 ] [ 4 ] [ 5 ];
  [%expect {| Unequal_lengths |}]
;;

let%expect_test "[merge]" =
  let module Int_list = struct
    type t = int list [@@deriving equal, sexp_of]
  end
  in
  let test_int xs ys =
    let list1 = merge xs ys ~compare:Int.compare in
    print_s [%sexp (list1 : int list)];
    let list2 = merge ys xs ~compare:Int.compare in
    require_equal (module Int_list) list1 list2
  in
  test_int [] [];
  [%expect {| () |}];
  test_int [] [ 1; 2; 3 ];
  [%expect {| (1 2 3) |}];
  test_int [ 1; 2 ] [ 3; 4; 5 ];
  [%expect {| (1 2 3 4 5) |}];
  test_int [ 1; 3 ] [ 2; 4; 5 ];
  [%expect {| (1 2 3 4 5) |}];
  test_int [ 1; 4 ] [ 2; 3; 5 ];
  [%expect {| (1 2 3 4 5) |}];
  test_int [ 1; 5 ] [ 2; 3; 4 ];
  [%expect {| (1 2 3 4 5) |}];
  test_int [ 1; 3; 5 ] [ 2; 4 ];
  [%expect {| (1 2 3 4 5) |}];
  test_int [ 1; 3; 4 ] [ 1; 2 ];
  [%expect {| (1 1 2 3 4) |}];
  let test_pair xs ys =
    let list1 = merge xs ys ~compare:[%compare: int * _] in
    print_s [%sexp (list1 : (int * string) list)];
    let list2 = merge ys xs ~compare:[%compare: int * _] in
    print_s [%sexp (list2 : (int * string) list)]
  in
  test_pair [] [];
  [%expect
    {|
    ()
    ()
    |}];
  test_pair [] [ 1, "a"; 2, "b"; 3, "c" ];
  [%expect
    {|
    ((1 a) (2 b) (3 c))
    ((1 a) (2 b) (3 c))
    |}];
  test_pair [ 1, "z"; 2, "y" ] [ 3, "x"; 4, "w"; 5, "v" ];
  [%expect
    {|
    ((1 z) (2 y) (3 x) (4 w) (5 v))
    ((1 z) (2 y) (3 x) (4 w) (5 v))
    |}];
  test_pair [ 1, "a"; 2, "b" ] [];
  [%expect
    {|
    ((1 a) (2 b))
    ((1 a) (2 b))
    |}];
  test_pair [ 1, "a"; 3, "b" ] [ 1, "b"; 2, "a" ];
  [%expect
    {|
    ((1 a) (1 b) (2 a) (3 b))
    ((1 b) (1 a) (2 a) (3 b))
    |}];
  test_pair [ 0, "!"; 1, "b"; 2, "a" ] [ 1, "a"; 2, "b" ];
  [%expect
    {|
    ((0 !) (1 b) (1 a) (2 a) (2 b))
    ((0 !) (1 a) (1 b) (2 b) (2 a))
    |}]
;;

let%expect_test "[sub]" =
  let test pos len list =
    match sub list ~pos ~len with
    | list -> print_s [%sexp (list : int list)]
    | exception exn -> print_s [%sexp "raised", (exn : exn)]
  in
  test 0 0 [];
  [%expect {| () |}];
  test 1 0 [];
  [%expect {| (raised (Invalid_argument List.sub)) |}];
  test 0 1 [];
  [%expect {| (raised (Invalid_argument List.sub)) |}];
  let list = [ 1; 2; 3; 4 ] in
  test 0 0 list;
  [%expect {| () |}];
  test 0 4 list;
  [%expect {| (1 2 3 4) |}];
  test 0 1 list;
  [%expect {| (1) |}];
  test 1 1 list;
  [%expect {| (2) |}];
  test 2 1 list;
  [%expect {| (3) |}];
  test 3 1 list;
  [%expect {| (4) |}];
  test 4 1 list;
  [%expect {| (raised (Invalid_argument List.sub)) |}];
  test 0 2 list;
  [%expect {| (1 2) |}];
  test 1 2 list;
  [%expect {| (2 3) |}];
  test 2 2 list;
  [%expect {| (3 4) |}];
  test 3 2 list;
  [%expect {| (raised (Invalid_argument List.sub)) |}];
  test 0 3 list;
  [%expect {| (1 2 3) |}];
  test 1 3 list;
  [%expect {| (2 3 4) |}];
  test 2 3 list;
  [%expect {| (raised (Invalid_argument List.sub)) |}];
  test 0 4 list;
  [%expect {| (1 2 3 4) |}];
  test 1 4 list;
  [%expect {| (raised (Invalid_argument List.sub)) |}];
  test 0 5 list;
  [%expect {| (raised (Invalid_argument List.sub)) |}];
  test (-1) 0 list;
  [%expect {| (raised (Invalid_argument List.sub)) |}];
  test 1 (-1) list;
  [%expect {| (raised (Invalid_argument List.sub)) |}]
;;

let%expect_test "[of_iter]" =
  let module M = struct
    type t = int list [@@deriving equal, quickcheck ~generator ~shrinker, sexp_of]
  end
  in
  quickcheck_m (module M) ~f:(fun list ->
    require_equal (module M) list (List.of_iter ~iter:(List.iter list)))
;;
