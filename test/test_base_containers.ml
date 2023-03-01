open! Import
open Test_container
include (Test_S1 (Array) : sig end)
include (Test_S1 (List) : sig end)
include (Test_S1 (Queue) : sig end)

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
    Container: testing [iteri]
    Container: testing [existsi]
    Container: testing [for_alli]
    Container: testing [counti]
    Container: testing [findi]
    Container: testing [find_mapi]
    Container: testing [init]
    Container: testing [mapi]
    Container: testing [filteri]
    Container: testing [filter_mapi]
    Container: testing [concat_mapi] |}]
;;

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
    Container: testing [max_elt] |}]
;;

let%expect_test "List" =
  Base_container_tests.test_indexed_container_s1_with_creators
    (module struct
      include List

      type 'a t = 'a list [@@deriving quickcheck]
    end);
  [%expect
    {|
    Container: testing [length]
    Container: testing [is_empty]
    Container: testing [mem]
    Container: testing [iter]
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
    Container: testing [iteri]
    Container: testing [existsi]
    Container: testing [for_alli]
    Container: testing [counti]
    Container: testing [findi]
    Container: testing [find_mapi]
    Container: testing [init]
    Container: testing [mapi]
    Container: testing [filteri]
    Container: testing [filter_mapi]
    Container: testing [concat_mapi] |}]
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
    Container: testing [max_elt] |}]
;;

let%expect_test "String" =
  Base_container_tests.test_indexed_container_s0_with_creators
    (module struct
      include String

      module Elt = struct
        type t = char [@@deriving compare, equal, quickcheck, sexp_of]
      end

      type t = string [@@deriving quickcheck]

      (* eta-expand due to [local_] types *)
      let mem t c = mem t c

      (* leave off the [?sep] argument *)
      let concat list = concat list
      let concat_map list = concat_map list
      let concat_mapi list = concat_mapi list
    end);
  [%expect
    {|
    Container: testing [length]
    Container: testing [is_empty]
    Container: testing [mem]
    Container: testing [iter]
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
    Container: testing [iteri]
    Container: testing [existsi]
    Container: testing [for_alli]
    Container: testing [counti]
    Container: testing [findi]
    Container: testing [find_mapi]
    Container: testing [init]
    Container: testing [mapi]
    Container: testing [filteri]
    Container: testing [filter_mapi]
    Container: testing [concat_mapi] |}]
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
    Container: testing [iteri]
    Container: testing [existsi]
    Container: testing [for_alli]
    Container: testing [counti]
    Container: testing [findi]
    Container: testing [find_mapi] |}]
;;
