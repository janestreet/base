open! Import

module%test _ : module type of Avltree = struct
  open Avltree

  module%template For_quickcheck = struct
    open struct
      module type S = sig
        type t [@@deriving compare, quickcheck, sexp_of]

        include Comparator.S with type t := t

        type unboxed : any

        val box : unboxed -> t
        val unbox : t -> unboxed
        val compare_unboxed : [%compare: unboxed]
        val map : t -> t
        val map_unboxed : unboxed -> unboxed
      end

      module Int :
        S
        with type t = int
         and type comparator_witness = Int.comparator_witness
         and type unboxed := int = struct
        include Int

        [%%rederive type t = int [@@deriving quickcheck]]

        let quickcheck_generator = Base_quickcheck.Generator.small_positive_or_zero_int
        let box = Fn.id
        let unbox = Fn.id
        let compare_unboxed = compare
        let map = neg
        let map_unboxed = neg
      end

      module Int64 :
        S
        with type t = int64
         and type comparator_witness = Int64.comparator_witness
         and type unboxed := int64# = struct
        include Int64

        [%%rederive type t = int64 [@@deriving quickcheck]]

        let quickcheck_generator =
          Base_quickcheck.Generator.small_positive_or_zero_int
          |> Base_quickcheck.Generator.map ~f:Int64.of_int
        ;;

        let box = Int64_u.to_int64
        let unbox = Int64_u.of_int64
        let compare_unboxed = Int64_u.compare
        let map = neg
        let map_unboxed = Int64_u.neg
      end

      module Float :
        S
        with type t = float
         and type comparator_witness = Float.comparator_witness
         and type unboxed := float# = struct
        include Float

        [%%rederive type t = float [@@deriving quickcheck]]

        let box = Float_u.to_float
        let unbox = Float_u.of_float
        let compare_unboxed = Float_u.compare
        let map = neg
        let map_unboxed = Float_u.neg
      end

      module String :
        S
        with type t = string
         and type comparator_witness = String.comparator_witness
         and type unboxed := string = struct
        include String

        [%%rederive type t = string [@@deriving quickcheck]]

        let quickcheck_generator =
          Base_quickcheck.Generator.string_of Base_quickcheck.Generator.char_lowercase
        ;;

        let box = Fn.id
        let unbox = Fn.id
        let compare_unboxed = compare
        let map t = t ^ t
        let map_unboxed t = t ^ t
      end
    end

    module [@kind value_or_null] Key = Int
    module [@kind bits64] Key = Int64
    module [@kind float64] Key = Float
    module [@kind value_or_null] Data = String
    module [@kind bits64] Data = Int64
    module [@kind float64] Data = Float

    [@@@kind.default
      k = (value_or_null, bits64, float64), v = (value_or_null, bits64, float64)]

    open struct
      module Key = Key [@kind k]

      let compare = Key.compare_unboxed

      module Data = Data [@kind v]
    end

    module Constructor = struct
      type t =
        | Add of Key.t * Data.t
        | Replace of Key.t * Data.t
        | Remove of Key.t
      [@@deriving quickcheck, sexp_of]

      let apply_to_tree t tree =
        match t with
        | Add (key, data) ->
          (add [@kind k v])
            tree
            ~key:(Key.unbox key)
            ~data:(Data.unbox data)
            ~compare
            ~added:(ref false)
            ~replace:false
        | Replace (key, data) ->
          (add [@kind k v])
            tree
            ~key:(Key.unbox key)
            ~data:(Data.unbox data)
            ~compare
            ~added:(ref false)
            ~replace:true
        | Remove key ->
          (remove [@kind k v]) tree (Key.unbox key) ~compare ~removed:(ref false)
      ;;

      let apply_to_map t map =
        match t with
        | Add (key, data) -> if Map.mem map key then map else Map.set map ~key ~data
        | Replace (key, data) -> Map.set map ~key ~data
        | Remove key -> Map.remove map key
      ;;
    end

    open struct
      module Constructor = Constructor [@kind k v]
    end

    module Constructors = struct
      type t = Constructor.t list [@@deriving quickcheck, sexp_of]
    end

    let reify constructors =
      List.fold
        constructors
        ~init:((empty [@kind k v]), Map.empty (module Key))
        ~f:(fun (t, map) constructor ->
          ( Constructor.apply_to_tree constructor t
          , Constructor.apply_to_map constructor map ))
    ;;

    let merge map1 map2 =
      Map.merge map1 map2 ~f:(fun ~key variant ->
        match variant with
        | `Left data | `Right data -> Some data
        | `Both (data1, data2) ->
          Error.raise_s
            [%message
              "duplicate data for key" (key : Key.t) (data1 : Data.t) (data2 : Data.t)])
    ;;

    let rec to_map : (_ t[@kind k v]) -> _ = function
      | Empty -> Map.empty (module Key)
      | Leaf { key; value = data } ->
        Map.singleton (module Key) (Key.box key) (Data.box data)
      | Node { left; key; value = data; height = _; right } ->
        (merge [@kind k v])
          (Map.singleton (module Key) (Key.box key) (Data.box data))
          ((merge [@kind k v]) ((to_map [@kind k v]) left) ((to_map [@kind k v]) right))
    ;;
  end

  open For_quickcheck

  [%%template
  [@@@kind.default
    k = (value_or_null, bits64, float64), v = (value_or_null, bits64, float64)]

  type ('k : k, 'v : v) t = (('k, 'v) Avltree.t[@kind k v]) = private
    | Empty
    | Node of
        { mutable left : (('k, 'v) t[@kind k v])
        ; key : 'k
        ; mutable value : 'v
        ; mutable height : int
        ; mutable right : (('k, 'v) t[@kind k v])
        }
    | Leaf of
        { key : 'k
        ; mutable value : 'v
        }

  open struct
    module Key = Key [@kind k]

    let compare = Key.compare_unboxed

    module Data = Data [@kind v]
    module Constructor = Constructor [@kind k v]
    module Constructors = Constructors [@kind k v]
  end

  let empty = (empty [@kind k v])
  let get_empty = (get_empty [@kind k v])

  let%test_unit _ =
    match (empty [@kind k v]), (get_empty [@kind k v]) () with
    | Empty, Empty -> ()
    | _ -> assert false
  ;;

  let is_empty = (is_empty [@kind k v])
  let%test _ = (is_empty [@kind k v]) (empty [@kind k v])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn (module Constructors) ~f:(fun constructors ->
      let t, map = (reify [@kind k v]) constructors in
      [%test_result: bool] ((is_empty [@kind k v]) t) ~expect:(Map.is_empty map))
  ;;

  let invariant = (invariant [@kind k v])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn (module Constructors) ~f:(fun constructors ->
      let t, map = (reify [@kind k v]) constructors in
      (invariant [@kind k v]) t ~compare;
      [%test_result: Data.t Map.M(Key).t] ((to_map [@kind k v]) t) ~expect:map)
  ;;

  let add = (add [@kind k v])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructor.t list * Key.t * Data.t * bool
        [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key, data, replace) ->
        let t, map = (reify [@kind k v]) constructors in
        (* test [added], other aspects of [add] are tested via [reify] in the [invariant]
           test above *)
        let added = ref false in
        let (_ : (_ t[@kind k v])) =
          (add [@kind k v])
            t
            ~key:(Key.unbox key)
            ~data:(Data.unbox data)
            ~compare
            ~added
            ~replace
        in
        [%test_result: bool] !added ~expect:(not (Map.mem map key)))
  ;;

  let remove = (remove [@kind k v])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key) ->
        let t, map = (reify [@kind k v]) constructors in
        (* test [removed], other aspects of [remove] are tested via [reify] in the
           [invariant] test above *)
        let removed = ref false in
        let (_ : (_ t[@kind k v])) =
          (remove [@kind k v]) t (Key.unbox key) ~compare ~removed
        in
        [%test_result: bool] !removed ~expect:(Map.mem map key))
  ;;

  let find = (find [@kind k v] [@mode c]) [@@mode c = (uncontended, shared)]

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key) ->
        let t, map = (reify [@kind k v]) constructors in
        [%test_result: Data.t option]
          ((Option.map [@kind v value_or_null])
             ((find [@kind k v]) t (Key.unbox key) ~compare)
             ~f:Data.box)
          ~expect:(Map.find map key))
  ;;

  let mem = (mem [@kind k v] [@mode c]) [@@mode c = (uncontended, shared)]

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key) ->
        let t, map = (reify [@kind k v]) constructors in
        [%test_result: bool]
          ((mem [@kind k v]) t (Key.unbox key) ~compare)
          ~expect:(Map.mem map key))
  ;;

  let find_and_call = (find_and_call [@kind k v r] [@mode c])
  [@@kind
    k = (value_or_null, bits64, float64)
    , v = (value_or_null, bits64, float64)
    , r = (value_or_null, bits64, float64)]
  [@@mode c = (uncontended, shared)]
  ;;

  let find_and_call = (find_and_call [@kind k v value_or_null])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key) ->
        let t, map = (reify [@kind k v]) constructors in
        [%test_result: [ `Found of Data.t | `Not_found of Key.t ]]
          ((find_and_call [@kind k v])
             t
             (Key.unbox key)
             ~compare
             ~if_found:(fun data -> `Found (Data.box data))
             ~if_not_found:(fun key -> `Not_found (Key.box key)))
          ~expect:
            (match Map.find map key with
             | None -> `Not_found key
             | Some data -> `Found data))
  ;;

  let findi_and_call = (findi_and_call [@kind k v r] [@mode c])
  [@@kind
    k = (value_or_null, bits64, float64)
    , v = (value_or_null, bits64, float64)
    , r = (value_or_null, bits64, float64)]
  [@@mode c = (uncontended, shared)]
  ;;

  let findi_and_call = (findi_and_call [@kind k v value_or_null])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key) ->
        let t, map = (reify [@kind k v]) constructors in
        [%test_result: [ `Found of Key.t * Data.t | `Not_found of Key.t ]]
          ((findi_and_call [@kind k v])
             t
             (Key.unbox key)
             ~compare
             ~if_found:(fun ~key ~data -> `Found (Key.box key, Data.box data))
             ~if_not_found:(fun key -> `Not_found (Key.box key)))
          ~expect:
            (match Map.find map key with
             | None -> `Not_found key
             | Some data -> `Found (key, data)))
  ;;

  let find_and_call1 = (find_and_call1 [@kind k v a r] [@mode c])
  [@@kind
    k = (value_or_null, bits64, float64)
    , v = (value_or_null, bits64, float64)
    , a = (value_or_null, bits64, float64)
    , r = (value_or_null, bits64, float64)]
  [@@mode c = (uncontended, shared)]
  ;;

  let find_and_call1 = (find_and_call1 [@kind k v value_or_null value_or_null])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t * int [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key, a) ->
        let t, map = (reify [@kind k v]) constructors in
        [%test_result: [ `Found of Data.t * int | `Not_found of Key.t * int ]]
          ((find_and_call1 [@kind k v])
             t
             (Key.unbox key)
             ~compare
             ~a
             ~if_found:(fun data a -> `Found (Data.box data, a))
             ~if_not_found:(fun key a -> `Not_found (Key.box key, a)))
          ~expect:
            (match Map.find map key with
             | None -> `Not_found (key, a)
             | Some data -> `Found (data, a)))
  ;;

  let findi_and_call1 = (findi_and_call1 [@kind k v a r] [@mode c])
  [@@kind
    k = (value_or_null, bits64, float64)
    , v = (value_or_null, bits64, float64)
    , a = (value_or_null, bits64, float64)
    , r = (value_or_null, bits64, float64)]
  [@@mode c = (uncontended, shared)]
  ;;

  let findi_and_call1 = (findi_and_call1 [@kind k v value_or_null value_or_null])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t * int [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key, a) ->
        let t, map = (reify [@kind k v]) constructors in
        [%test_result: [ `Found of Key.t * Data.t * int | `Not_found of Key.t * int ]]
          ((findi_and_call1 [@kind k v])
             t
             (Key.unbox key)
             ~compare
             ~a
             ~if_found:(fun ~key ~data a -> `Found (Key.box key, Data.box data, a))
             ~if_not_found:(fun key a -> `Not_found (Key.box key, a)))
          ~expect:
            (match Map.find map key with
             | None -> `Not_found (key, a)
             | Some data -> `Found (key, data, a)))
  ;;

  let find_and_call2 = (find_and_call2 [@kind k v a b r] [@mode c])
  [@@kind
    k = (value_or_null, bits64, float64)
    , v = (value_or_null, bits64, float64)
    , a = (value_or_null, bits64, float64)
    , b = (value_or_null, bits64, float64)
    , r = (value_or_null, bits64, float64)]
  [@@mode c = (uncontended, shared)]
  ;;

  let find_and_call2 =
    (find_and_call2 [@kind k v value_or_null value_or_null value_or_null])
  ;;

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t * int * string [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key, a, b) ->
        let t, map = (reify [@kind k v]) constructors in
        [%test_result:
          [ `Found of Data.t * int * string | `Not_found of Key.t * int * string ]]
          ((find_and_call2 [@kind k v])
             t
             (Key.unbox key)
             ~compare
             ~a
             ~b
             ~if_found:(fun data a b -> `Found (Data.box data, a, b))
             ~if_not_found:(fun key a b -> `Not_found (Key.box key, a, b)))
          ~expect:
            (match Map.find map key with
             | None -> `Not_found (key, a, b)
             | Some data -> `Found (data, a, b)))
  ;;

  let findi_and_call2 = (findi_and_call2 [@kind k v a b r] [@mode c])
  [@@kind
    k = (value_or_null, bits64, float64)
    , v = (value_or_null, bits64, float64)
    , a = (value_or_null, bits64, float64)
    , b = (value_or_null, bits64, float64)
    , r = (value_or_null, bits64, float64)]
  [@@mode c = (uncontended, shared)]
  ;;

  let findi_and_call2 =
    (findi_and_call2 [@kind k v value_or_null value_or_null value_or_null])
  ;;

  let%test_unit _ =
    Base_quickcheck.Test.run_exn
      (module struct
        type t = Constructors.t * Key.t * int * string [@@deriving quickcheck, sexp_of]
      end)
      ~f:(fun (constructors, key, a, b) ->
        let t, map = (reify [@kind k v]) constructors in
        [%test_result:
          [ `Found of Key.t * Data.t * int * string | `Not_found of Key.t * int * string ]]
          ((findi_and_call2 [@kind k v])
             t
             (Key.unbox key)
             ~compare
             ~a
             ~b
             ~if_found:(fun ~key ~data a b -> `Found (Key.box key, Data.box data, a, b))
             ~if_not_found:(fun key a b -> `Not_found (Key.box key, a, b)))
          ~expect:
            (match Map.find map key with
             | None -> `Not_found (key, a, b)
             | Some data -> `Found (key, data, a, b)))
  ;;

  let iter = (iter [@kind k v] [@mode c]) [@@mode c = (uncontended, shared)]

  let%test_unit _ =
    Base_quickcheck.Test.run_exn (module Constructors) ~f:(fun constructors ->
      let t, map = (reify [@kind k v]) constructors in
      [%test_result: (Key.t * Data.t) list]
        (let q = Queue.create () in
         (iter [@kind k v]) t ~f:(fun ~key ~data ->
           Queue.enqueue q (Key.box key, Data.box data));
         Queue.to_list q)
        ~expect:(Map.to_alist map))
  ;;

  let mapi_inplace = (mapi_inplace [@kind k v])

  let%test_unit _ =
    Base_quickcheck.Test.run_exn (module Constructors) ~f:(fun constructors ->
      let t, map = (reify [@kind k v]) constructors in
      [%test_result: (Key.t * Data.t) list]
        ((mapi_inplace [@kind k v]) t ~f:(fun ~key:_ ~data -> Data.map_unboxed data);
         (fold [@kind k v]) t ~init:[] ~f:(fun ~key ~data acc ->
           (Key.box key, Data.box data) :: acc))
        ~expect:(Map.map map ~f:Data.map |> Map.to_alist |> List.rev))
  ;;

  let fold = (fold [@kind k v] [@mode c]) [@@mode c = (uncontended, shared)]

  let%test_unit _ =
    Base_quickcheck.Test.run_exn (module Constructors) ~f:(fun constructors ->
      let t, map = (reify [@kind k v]) constructors in
      [%test_result: (Key.t * Data.t) list]
        ((fold [@kind k v]) t ~init:[] ~f:(fun ~key ~data acc ->
           (Key.box key, Data.box data) :: acc))
        ~expect:(Map.to_alist map |> List.rev))
  ;;

  let choose_exn = (choose_exn [@kind k v] [@mode c]) [@@mode c = (uncontended, shared)]

  let%test_unit _ =
    Base_quickcheck.Test.run_exn (module Constructors) ~f:(fun constructors ->
      let t, map = (reify [@kind k v]) constructors in
      [%test_result: bool]
        (is_some
           (Option.try_with (fun () -> ignore ((choose_exn [@kind k v]) t : #(_ * _)))))
        ~expect:(not (Map.is_empty map)))
  ;;]

  let first = first

  let%test_unit _ =
    Base_quickcheck.Test.run_exn (module Constructors) ~f:(fun constructors ->
      let t, map = reify constructors in
      [%test_result: (Key.t * Data.t) option] (first t) ~expect:(Map.min_elt map))
  ;;

  let last = last

  let%test_unit _ =
    Base_quickcheck.Test.run_exn (module Constructors) ~f:(fun constructors ->
      let t, map = reify constructors in
      [%test_result: (Key.t * Data.t) option] (last t) ~expect:(Map.max_elt map))
  ;;
end
