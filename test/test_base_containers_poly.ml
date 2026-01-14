open! Import
open Test_container

(* Tests of containers that are polymorphic over their element type. *)

include (Test_S1 (Array) : sig end)
include (Test_S1 (List) : sig end)
include (Test_S1 (Queue) : sig end)

(* Quickcheck-based expect tests *)

let%expect_test "Array" =
  Base_container_tests.test_indexed_container_s1_with_creators
    (module struct
      include Array

      type 'a t = 'a array [@@deriving quickcheck]

      (* [Array.concat] has a slightly different type than S1 expects *)
      let concat array = concat (Array.to_list array)
    end);
  [%expect
    {|
    Container: testing [length]
    Container: testing [is_empty]
    Container: testing [mem]
    Container: testing [iter]
    Container: testing [iter_until]
    Container: testing [fold]
    Container: testing [fold_result]
    Container: testing [fold_until]
    Container: testing [exists]
    Container: testing [for_all]
    Container: testing [count]
    Container: testing [sum]
    Container: testing [find]
    Container: testing [find_map]
    Container: testing [to_list]
    Container: testing [to_array]
    Container: testing [min_elt]
    Container: testing [max_elt]
    Container: testing [of_list]
    Container: testing [of_array]
    Container: testing [append]
    Container: testing [concat]
    Container: testing [map]
    Container: testing [filter]
    Container: testing [filter_map]
    Container: testing [concat_map]
    Container: testing [partition_tf]
    Container: testing [partition_map]
    Container: testing [foldi]
    Container: testing [foldi_until]
    Container: testing [iteri]
    Container: testing [iteri_until]
    Container: testing [existsi]
    Container: testing [for_alli]
    Container: testing [counti]
    Container: testing [findi]
    Container: testing [find_mapi]
    Container: testing [init]
    Container: testing [mapi]
    Container: testing [filteri]
    Container: testing [filter_mapi]
    Container: testing [concat_mapi]
    Container: testing [partitioni_tf]
    Container: testing [partition_mapi]
    |}]
;;

let%expect_test "List" =
  (Base_container_tests.test_indexed_container_s1_with_creators [@alloc stack])
    ~check_no_allocation:true
    (module struct
      include List

      type 'a t = 'a list [@@deriving quickcheck]
    end);
  [%expect
    {|
    Container: testing [length zero_alloc]
    Container: testing [is_empty zero_alloc]
    Container: testing [mem zero_alloc]
    Container: testing [mem__local zero_alloc]
    Container: testing [iter]
    Container: testing [iter zero_alloc]
    Container: testing [iter__local]
    Container: testing [iter__local zero_alloc]
    Container: testing [iter_until]
    Container: testing [iter_until__global__local zero_alloc]
    Container: testing [iter_until__local__global]
    Container: testing [iter_until__local__local zero_alloc]
    Container: testing [fold]
    Container: testing [fold zero_alloc]
    Container: testing [fold__global__local zero_alloc]
    Container: testing [fold__global__local zero_alloc]
    Container: testing [fold__local__global]
    Container: testing [fold__local__global zero_alloc]
    Container: testing [fold__local__local zero_alloc]
    Container: testing [fold__local__local zero_alloc]
    Container: testing [fold_result]
    Container: testing [fold_result__global__local zero_alloc]
    Container: testing [fold_result__local__global]
    Container: testing [fold_result__local__local zero_alloc]
    Container: testing [fold_until]
    Container: testing [fold_until__global__local zero_alloc]
    Container: testing [fold_until__local__global]
    Container: testing [fold_until__local__local zero_alloc]
    Container: testing [exists zero_alloc]
    Container: testing [exists__local zero_alloc]
    Container: testing [for_all zero_alloc]
    Container: testing [for_all__local zero_alloc]
    Container: testing [count zero_alloc]
    Container: testing [count__local zero_alloc]
    Container: testing [sum zero_alloc]
    Container: testing [sum__global__local zero_alloc]
    Container: testing [sum__local__global zero_alloc]
    Container: testing [sum__local__local zero_alloc]
    Container: testing [find]
    Container: testing [find__local zero_alloc]
    Container: testing [find_map]
    Container: testing [find_map__global__local zero_alloc]
    Container: testing [find_map__local__global]
    Container: testing [find_map__local__local zero_alloc]
    Container: testing [to_list]
    Container: testing [to_list__stack zero_alloc]
    Container: testing [to_array]
    Container: testing [min_elt]
    Container: testing [min_elt__local zero_alloc]
    Container: testing [max_elt]
    Container: testing [max_elt__local zero_alloc]
    Container: testing [of_list]
    Container: testing [of_list__stack zero_alloc]
    Container: testing [of_array]
    Container: testing [of_array__stack zero_alloc]
    Container: testing [append]
    Container: testing [append__stack zero_alloc]
    Container: testing [concat]
    Container: testing [concat__stack zero_alloc]
    Container: testing [map]
    Container: testing [map__stack zero_alloc]
    Container: testing [map__local]
    Container: testing [map__local__stack zero_alloc]
    Container: testing [filter]
    Container: testing [filter__stack zero_alloc]
    Container: testing [filter_map]
    Container: testing [filter_map__stack zero_alloc]
    Container: testing [filter_map__local]
    Container: testing [filter_map__local__stack zero_alloc]
    Container: testing [concat_map]
    Container: testing [concat_map__stack zero_alloc]
    Container: testing [concat_map__local]
    Container: testing [concat_map__local__stack zero_alloc]
    Container: testing [partition_tf]
    Container: testing [partition_tf__stack zero_alloc]
    Container: testing [partition_map]
    Container: testing [partition_map__stack zero_alloc]
    Container: testing [partition_map__local]
    Container: testing [partition_map__local__stack zero_alloc]
    Container: testing [foldi]
    Container: testing [foldi zero_alloc]
    Container: testing [foldi__global__local zero_alloc]
    Container: testing [foldi__global__local zero_alloc]
    Container: testing [foldi__local__global]
    Container: testing [foldi__local__global zero_alloc]
    Container: testing [foldi__local__local zero_alloc]
    Container: testing [foldi__local__local zero_alloc]
    Container: testing [foldi_until]
    Container: testing [foldi_until__global__local zero_alloc]
    Container: testing [foldi_until__local__global]
    Container: testing [foldi_until__local__local zero_alloc]
    Container: testing [iteri]
    Container: testing [iteri zero_alloc]
    Container: testing [iteri__local]
    Container: testing [iteri__local zero_alloc]
    Container: testing [iteri_until]
    Container: testing [iteri_until__global__local zero_alloc]
    Container: testing [iteri_until__local__global]
    Container: testing [iteri_until__local__local zero_alloc]
    Container: testing [existsi zero_alloc]
    Container: testing [existsi__local zero_alloc]
    Container: testing [for_alli zero_alloc]
    Container: testing [for_alli__local zero_alloc]
    Container: testing [counti zero_alloc]
    Container: testing [counti__local zero_alloc]
    Container: testing [findi]
    Container: testing [findi__local zero_alloc]
    Container: testing [find_mapi]
    Container: testing [find_mapi__global__local zero_alloc]
    Container: testing [find_mapi__local__global]
    Container: testing [find_mapi__local__local zero_alloc]
    Container: testing [init]
    Container: testing [init__stack zero_alloc]
    Container: testing [mapi]
    Container: testing [mapi__stack zero_alloc]
    Container: testing [mapi__local]
    Container: testing [mapi__local__stack zero_alloc]
    Container: testing [filteri]
    Container: testing [filteri__stack zero_alloc]
    Container: testing [filter_mapi]
    Container: testing [filter_mapi__stack zero_alloc]
    Container: testing [filter_mapi__local]
    Container: testing [filter_mapi__local__stack zero_alloc]
    Container: testing [concat_mapi]
    Container: testing [concat_mapi__stack zero_alloc]
    Container: testing [concat_mapi__local]
    Container: testing [concat_mapi__local__stack zero_alloc]
    Container: testing [partitioni_tf]
    Container: testing [partitioni_tf__stack zero_alloc]
    Container: testing [partition_mapi]
    Container: testing [partition_mapi__stack zero_alloc]
    Container: testing [partition_mapi__local]
    Container: testing [partition_mapi__local__stack zero_alloc]
    |}]
;;

let%expect_test "Set" =
  Base_container_tests.test_container_s0
    (module struct
      open Base_quickcheck

      module Elt = struct
        include Int

        type t = (int[@generator Generator.small_strictly_positive_int])
        [@@deriving compare, equal, quickcheck, sexp_of]
      end

      include Set

      type t = Set.M(Int).t [@@deriving sexp_of]

      let quickcheck_generator = Generator.set_t_m (module Elt) Elt.quickcheck_generator
      let quickcheck_observer = Observer.set_t Elt.quickcheck_observer
      let quickcheck_shrinker = Shrinker.set_t Elt.quickcheck_shrinker
      let min_elt t ~compare:_ = min_elt t
      let max_elt t ~compare:_ = max_elt t

      (* [find] and [find_map] use pre-order traversals (root -> left -> right), while all
         the other traversals are in-order (left -> root -> right). We patch them up here
         to behave like pre-order, while still using [Set.find] and [Set.find_map] for the
         searching so we're actually testing those functions. *)

      let rec find t ~f =
        match Set.find t ~f with
        | None -> None
        | Some elt as some ->
          let lt, _ = Set.split_lt_ge t elt in
          Option.first_some (find lt ~f) some
      ;;

      let rec find_map t ~f =
        match Set.find_map t ~f:(fun elt -> Option.map (f elt) ~f:(fun x -> elt, x)) with
        | None -> None
        | Some (elt, x) ->
          let lt, _ = Set.split_lt_ge t elt in
          Option.first_some (find_map lt ~f) (Some x)
      ;;
    end);
  [%expect
    {|
    Container: testing [length]
    Container: testing [is_empty]
    Container: testing [mem]
    Container: testing [iter]
    Container: testing [iter_until]
    Container: testing [fold]
    Container: testing [fold_result]
    Container: testing [fold_until]
    Container: testing [exists]
    Container: testing [for_all]
    Container: testing [count]
    Container: testing [sum]
    Container: testing [find]
    Container: testing [find_map]
    Container: testing [to_list]
    Container: testing [to_array]
    Container: testing [min_elt]
    Container: testing [max_elt]
    |}]
;;

let%expect_test "Queue" =
  Base_container_tests.test_indexed_container_s1
    (module struct
      include Queue
      open Base_quickcheck

      let quickcheck_generator quickcheck_generator_elt =
        [%generator: elt list] |> Generator.map ~f:Queue.of_list
      ;;

      let quickcheck_observer quickcheck_observer_elt =
        [%observer: elt list] |> Observer.unmap ~f:Queue.to_list
      ;;

      let quickcheck_shrinker quickcheck_shrinker_elt =
        [%shrinker: elt list] |> Shrinker.map ~f:Queue.of_list ~f_inverse:Queue.to_list
      ;;
    end);
  [%expect
    {|
    Container: testing [length]
    Container: testing [is_empty]
    Container: testing [mem]
    Container: testing [iter]
    Container: testing [iter_until]
    Container: testing [fold]
    Container: testing [fold_result]
    Container: testing [fold_until]
    Container: testing [exists]
    Container: testing [for_all]
    Container: testing [count]
    Container: testing [sum]
    Container: testing [find]
    Container: testing [find_map]
    Container: testing [to_list]
    Container: testing [to_array]
    Container: testing [min_elt]
    Container: testing [max_elt]
    Container: testing [foldi]
    Container: testing [foldi_until]
    Container: testing [iteri]
    Container: testing [iteri_until]
    Container: testing [existsi]
    Container: testing [for_alli]
    Container: testing [counti]
    Container: testing [findi]
    Container: testing [find_mapi]
    |}]
;;
