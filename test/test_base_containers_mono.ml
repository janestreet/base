open! Import
open Test_container

(* Tests of containers that are not polymorphic (i.e. have a fixed element type). *)

include (
  Test_S0 (struct
    include String

    let mem t c = mem t c

    module Elt = struct
      type t = char [@@deriving sexp]

      let of_int = Char.of_int_exn
      let to_int = Char.to_int
    end

    let of_list = of_char_list
  end) :
  sig end)

let%expect_test "Hash_set" =
  Base_container_tests.test_container_s0
    (module struct
      open Base_quickcheck

      module Elt = struct
        include Int

        type t = (int[@generator Generator.small_strictly_positive_int])
        [@@deriving compare, equal, quickcheck, sexp_of]
      end

      include Hash_set

      type t = Hash_set.M(Int).t [@@deriving sexp_of]

      let quickcheck_generator =
        Generator.map [%generator: Elt.t list] ~f:(Hash_set.of_list (module Int))
      ;;

      let quickcheck_observer = Observer.unmap [%observer: Elt.t list] ~f:Hash_set.to_list

      let quickcheck_shrinker =
        Shrinker.map
          [%shrinker: Elt.t list]
          ~f:(Hash_set.of_list (module Int))
          ~f_inverse:Hash_set.to_list
      ;;

      (* [to_list] and [to_array] proceed in the opposite order as everything else. This
         is likely a performance hack to reuse [fold] without adding a [List.rev]. It is
         not particularly problematic, since hash table order is already unpredictable due
         to hash functions. *)
      let to_list t = List.rev (to_list t)
      let to_array t = Array.rev (to_array t)
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

let%expect_test "String" =
  (Base_container_tests.test_indexed_container_s0_with_creators [@alloc stack])
    ~check_no_allocation:true
    (module struct
      include String

      module Elt = struct
        type t = char
        [@@deriving
          compare ~localize, equal ~localize, quickcheck, sexp_of ~stackify, globalize]
      end

      type t = string [@@deriving quickcheck]

      (* eta-expand due to [local_] types *)
      let mem t c = mem t c
      let append t1 t2 = append t1 t2

      [%%template
      (* leave off the [?sep] argument *)
      let[@alloc a = (heap, stack)] concat list =
        (concat [@alloc a]) list [@exclave_if_stack a]
      ;;

      let[@mode li = (global, local)] [@alloc a = (heap, stack)] concat_map list ~f =
        (concat_map [@mode li] [@alloc a]) list ~f [@exclave_if_stack a]
      ;;

      let[@mode li = (global, local)] [@alloc a = (heap, stack)] concat_mapi list ~f =
        (concat_mapi [@mode li] [@alloc a]) list ~f [@exclave_if_stack a]
      ;;]
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
