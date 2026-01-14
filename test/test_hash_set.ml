open! Import

module%test _ = struct
  let () = Dynamic.set_root sexp_style Sexp_style.simple_pretty
end

module Int_hash_set = struct
  type t = Hash_set.M(Int).t [@@deriving equal, sexp_of]
end

module _ : module type of struct
  include Hash_set
end = struct
  module type Accessors = Hash_set.Accessors
  module type Creators = Hash_set.Creators
  module type Creators_generic = Hash_set.Creators_generic
  module type Equal_m = Hash_set.Equal_m
  module type For_deriving = Hash_set.For_deriving
  module type M_of_sexp = Hash_set.M_of_sexp
  module type Sexp_of_m = Hash_set.Sexp_of_m
  module type M_sexp_grammar = Hash_set.M_sexp_grammar

  module%template.portable [@modality p] Creators = Hash_set.Creators [@modality p]
  module Key = Hash_set.Key
  module M = Hash_set.M
  module Ok_or_absent = Hash_set.Ok_or_absent
  module Ok_or_duplicate = Hash_set.Ok_or_duplicate
  module Poly = Hash_set.Poly
  module Private = Hash_set.Private

  type ('a, 'b) create_options = ('a, 'b) Hash_set.create_options

  type ('a, 'b) create_options_without_first_class_module =
    ('a, 'b) Hash_set.create_options_without_first_class_module

  type 'a t = 'a Hash_set.t

  (* Not testing these for now. *)

  let capacity = Hash_set.capacity
  let hashable_s = Hash_set.hashable_s
  let sexp_of_m__t = Hash_set.sexp_of_m__t
  let m__t_of_sexp = Hash_set.m__t_of_sexp
  let m__t_sexp_grammar = Hash_set.m__t_sexp_grammar
  let equal_m__t = Hash_set.equal_m__t
  let equal__local_m__t = Hash_set.equal__local_m__t

  (* Testing generic container functionality. *)

  include (
    Hash_set :
    sig
    @@ portable
      include
        Container.Generic_for_s0 with type ('a, _, _) t := 'a t and type 'a elt := 'a
    end)

  let%expect_test "Container" =
    Base_container_tests.test_container_generic
      ~duplicates:Drop
      ~order:Unpredictable
      (module struct
        include Hash_set

        type (_, _, _) t = int Hash_set.t

        let mem t x ~equal:_ = mem t x

        module Elt = struct
          type _ t = int
          [@@deriving
            compare ~localize, equal ~localize, quickcheck, sexp_of ~stackify, globalize]
        end

        module Simple = struct
          type phantom1
          type phantom2
          type _ t = Hash_set.M(Int).t [@@deriving equal, sexp_of]

          open Base_quickcheck

          let quickcheck_generator _ =
            Generator.list Generator.small_strictly_positive_int
            |> Generator.map ~f:(Hash_set.of_list (module Int))
          ;;

          let quickcheck_observer _ =
            Observer.list Observer.int |> Observer.unmap ~f:Hash_set.to_list
          ;;

          let quickcheck_shrinker _ =
            Shrinker.list Shrinker.int
            |> Shrinker.map ~f:(Hash_set.of_list (module Int)) ~f_inverse:Hash_set.to_list
          ;;
        end
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

  (* Testing remaining individual operations. *)

  let of_list = Hash_set.of_list
  let create = Hash_set.create
  let add = Hash_set.add

  let%expect_test "of_list vs create + add" =
    quickcheck_m
      (module struct
        type t = int list [@@deriving quickcheck ~generator ~shrinker, sexp_of]
      end)
      ~f:(fun elts ->
        require_equal
          (module struct
            type t = Hash_set.M(Int).t [@@deriving equal, sexp_of]
          end)
          (Hash_set.of_list (module Int) elts)
          (let set = Hash_set.create (module Int) in
           List.iter elts ~f:(Hash_set.add set);
           set))
  ;;

  let copy = Hash_set.copy

  let%expect_test "copy" =
    quickcheck_m
      (module struct
        type t = int list [@@deriving quickcheck ~generator ~shrinker, sexp_of]
      end)
      ~f:(fun elts ->
        let source = Hash_set.of_list (module Int) elts in
        let copied = Hash_set.copy source in
        require_equal (module Int_hash_set) source copied;
        let absent =
          Sequence.find_exn
            (Sequence.range 0 (List.length elts + 1))
            ~f:(fun elt -> not (Hash_set.mem source elt))
        in
        Hash_set.strict_add_exn source absent;
        require_not_equal (module Int_hash_set) source copied;
        Hash_set.strict_add_exn copied absent;
        require_equal (module Int_hash_set) source copied)
  ;;

  let get_or_add = Hash_set.get_or_add

  module Test = struct
    type t = { inner : int [@hash.ignore] [@compare.ignore] }
    [@@deriving compare, hash, sexp, fields ~getters]

    let create inner = { inner }
  end

  let%expect_test "get_or_add" =
    let set = Hash_set.create (module Int) in
    require_equal (module Int) (get_or_add set 0) 0;
    require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0 ]);
    require_equal (module Int) (get_or_add set 0) 0;
    require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0 ]);
    require_equal (module Int) (get_or_add set 1) 1;
    require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0; 1 ]);
    require_equal (module Int) (get_or_add set 1) 1;
    require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0; 1 ]);
    (* Check that we get back the original object *)
    let set = Hash_set.create (module Test) in
    let element = Test.create 1 in
    let new_element = Test.create 2 in
    require (not (phys_equal element new_element));
    require (phys_equal (get_or_add set element) element);
    require (phys_equal (get_or_add set new_element) element);
    [%expect {| |}]
  ;;

  let strict_add = Hash_set.strict_add
  let strict_add_exn = Hash_set.strict_add_exn
  let strict_add_or_error = Hash_set.strict_add_or_error

  let%expect_test "strict_add" =
    List.iter
      [ ( "strict_add"
        , fun set elt ->
            match Hash_set.strict_add set elt with
            | Ok -> Ok ()
            | Duplicate -> Or_error.error_string "duplicate" )
      ; ( "strict_add_exn"
        , fun set elt -> Or_error.try_with (fun () -> Hash_set.strict_add_exn set elt) )
      ; "strict_add_or_error", Hash_set.strict_add_or_error
      ]
      ~f:(fun (name, f) ->
        print_endline name;
        let set = Hash_set.create (module Int) in
        require_ok (f set 0);
        require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0 ]);
        require_error [%sexp_of: unit] (f set 0);
        require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0 ]);
        require_ok (f set 1);
        require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0; 1 ]);
        require_error [%sexp_of: unit] (f set 1);
        require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0; 1 ]));
    [%expect
      {|
      strict_add
      strict_add_exn
      strict_add_or_error
      |}]
  ;;

  let remove = Hash_set.remove

  let%expect_test "remove" =
    let set = Hash_set.of_list (module Int) [ 0; 1 ] in
    Hash_set.remove set 1;
    require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0 ]);
    Hash_set.remove set 1;
    require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0 ]);
    Hash_set.remove set 0;
    require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) []);
    Hash_set.remove set 0;
    require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) []);
    [%expect {| |}]
  ;;

  let strict_remove = Hash_set.strict_remove
  let strict_remove_exn = Hash_set.strict_remove_exn
  let strict_remove_or_error = Hash_set.strict_remove_or_error

  let%expect_test "strict_remove" =
    List.iter
      [ ( "strict_remove"
        , fun set elt ->
            match Hash_set.strict_remove set elt with
            | Ok -> Ok ()
            | Absent -> Or_error.error_string "absent" )
      ; ( "strict_remove_exn"
        , fun set elt -> Or_error.try_with (fun () -> Hash_set.strict_remove_exn set elt)
        )
      ; "strict_remove_or_error", Hash_set.strict_remove_or_error
      ]
      ~f:(fun (name, f) ->
        print_endline name;
        let set = Hash_set.of_list (module Int) [ 0; 1 ] in
        require_ok (f set 1);
        require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0 ]);
        require_error [%sexp_of: unit] (f set 1);
        require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) [ 0 ]);
        require_ok (f set 0);
        require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) []);
        require_error [%sexp_of: unit] (f set 0);
        require_equal (module Int_hash_set) set (Hash_set.of_list (module Int) []));
    [%expect
      {|
      strict_remove
      strict_remove_exn
      strict_remove_or_error
      |}]
  ;;

  let clear = Hash_set.clear
  let filter = Hash_set.filter
  let filter_inplace = Hash_set.filter_inplace

  let%expect_test "clear, filter, filter_inplace" =
    let check ~(here : [%call_pos]) set elts =
      require_equal ~here (module Int_hash_set) set (Hash_set.of_list (module Int) elts)
    in
    let original = Hash_set.of_list (module Int) [ 1; 2; 3; 4; 5; 6; 7; 8 ] in
    let modified = Hash_set.filter original ~f:(fun elt -> elt > 4) in
    check original [ 1; 2; 3; 4; 5; 6; 7; 8 ];
    check modified [ 5; 6; 7; 8 ];
    Hash_set.filter_inplace original ~f:(fun elt -> elt <= 4);
    check original [ 1; 2; 3; 4 ];
    check modified [ 5; 6; 7; 8 ];
    Hash_set.clear modified;
    check original [ 1; 2; 3; 4 ];
    check modified [];
    [%expect {| |}]
  ;;

  let diff = Hash_set.diff

  let%expect_test "diff" =
    let test fst snd =
      let fst = Hash_set.of_list (module Int) fst in
      let snd = Hash_set.of_list (module Int) snd in
      let diff = Hash_set.diff fst snd in
      require_equal
        (module Int_hash_set)
        diff
        (Hash_set.filter fst ~f:(Fn.non (Hash_set.mem snd)))
        ~if_false_then_print_s:[%lazy_sexp { fst : Int_hash_set.t; snd : Int_hash_set.t }]
    in
    let lists = List.init 4 ~f:(fun len -> List.init len ~f:Fn.id) in
    List.iter (List.cartesian_product lists lists) ~f:(fun (a, b) -> test a b);
    [%expect {| |}]
  ;;

  let of_hashtbl_keys = Hash_set.of_hashtbl_keys
  let to_hashtbl = Hash_set.to_hashtbl

  let%expect_test "of_hashtbl_keys + to_hashtbl" =
    let test alist =
      let table = Hashtbl.of_alist_exn (module Int) alist in
      let keys = Hash_set.of_list (module Int) (List.map ~f:fst alist) in
      require_equal (module Int_hash_set) keys (Hash_set.of_hashtbl_keys table);
      require_equal
        (module struct
          type t = string Hashtbl.M(Int).t [@@deriving equal, sexp_of]
        end)
        table
        (Hash_set.to_hashtbl keys ~f:(Hashtbl.find_exn table))
    in
    test [];
    [%expect {| |}];
    test [ 1, "one" ];
    [%expect {| |}];
    test [ 1, "one"; 2, "two" ];
    [%expect {| |}];
    test [ 1, "one"; 2, "two"; 3, "three" ];
    [%expect {| |}]
  ;;

  let inter = Hash_set.inter

  module%test [@name "Set Intersection"] _ = struct
    let run_test first_contents second_contents ~expect =
      let of_list lst =
        let s = Hash_set.create (module String) in
        List.iter lst ~f:(Hash_set.add s);
        s
      in
      let s1 = of_list first_contents in
      let s2 = of_list second_contents in
      let expect = of_list expect in
      let result = Hash_set.inter s1 s2 in
      Hash_set.iter result ~f:(fun x -> assert (Hash_set.mem expect x));
      Hash_set.iter expect ~f:(fun x -> assert (Hash_set.mem result x));
      let equal x y = 0 = String.compare x y in
      assert (List.equal equal (Hash_set.to_list result) (Hash_set.to_list expect));
      assert (Hash_set.length result = Hash_set.length expect);
      (* Make sure the sets are unmodified by the inter *)
      assert (List.length first_contents = Hash_set.length s1);
      assert (List.length second_contents = Hash_set.length s2)
    ;;

    let%test_unit "First smaller" =
      run_test [ "0"; "3"; "99" ] [ "0"; "1"; "2"; "3" ] ~expect:[ "0"; "3" ]
    ;;

    let%test_unit "Second smaller" =
      run_test [ "a"; "b"; "c"; "d" ] [ "b"; "d" ] ~expect:[ "b"; "d" ]
    ;;

    let%test_unit "No intersection" =
      run_test ~expect:[] [ "a"; "b"; "c"; "d" ] [ "1"; "2"; "3"; "4" ]
    ;;
  end

  let sexp_of_t = Hash_set.sexp_of_t

  let%expect_test "sexp" =
    let ints = List.init 20 ~f:(fun x -> x * x) in
    let int_hash_set = Hash_set.of_list (module Int) ints in
    print_s [%sexp (int_hash_set : int Hash_set.t)];
    [%expect {| (0 1 4 9 16 25 36 49 64 81 100 121 144 169 196 225 256 289 324 361) |}];
    let strs = List.init 20 ~f:(fun x -> Int.to_string x) in
    let str_hash_set = Hash_set.of_list (module String) strs in
    print_s [%sexp (str_hash_set : string Hash_set.t)];
    [%expect {| (0 1 10 11 12 13 14 15 16 17 18 19 2 3 4 5 6 7 8 9) |}]
  ;;

  let to_array = Hash_set.to_array

  let%expect_test "to_array" =
    let empty_array = Hash_set.to_array (Hash_set.of_list (module Int) []) in
    print_s [%sexp (empty_array : int Array.t)];
    [%expect {| () |}];
    let array_from_to_array =
      Hash_set.to_array (Hash_set.of_list (module Int) [ 1; 2; 3; 4; 5 ])
    in
    print_s [%sexp (array_from_to_array : int Array.t)];
    [%expect {| (1 3 2 4 5) |}];
    let array_via_to_list =
      Hash_set.to_list (Hash_set.of_list (module Int) [ 1; 2; 3; 4; 5 ]) |> Array.of_list
    in
    print_s [%sexp (array_via_to_list : int Array.t)];
    [%expect {| (1 3 2 4 5) |}]
  ;;

  let union = Hash_set.union

  let%expect_test "union" =
    let print_union s1 s2 =
      let s1 = Hash_set.of_list (module Int) s1 in
      let s2 = Hash_set.of_list (module Int) s2 in
      print_s [%sexp (Hash_set.union s1 s2 : int Hash_set.t)]
    in
    print_union [ 0; 1; 2 ] [ 3; 4; 5 ];
    [%expect {| (0 1 2 3 4 5) |}];
    print_union [ 0; 1; 2 ] [ 1; 2; 3 ];
    [%expect {| (0 1 2 3) |}]
  ;;

  let union_in_place = Hash_set.union_in_place

  let%expect_test "union_in_place" =
    let print_union dst src =
      let dst = Hash_set.of_list (module Int) dst in
      let src = Hash_set.of_list (module Int) src in
      Hash_set.union_in_place ~dst ~src;
      print_s [%message (dst : int Hash_set.t) (src : int Hash_set.t)]
    in
    print_union [ 0; 1; 2 ] [ 3; 4; 5 ];
    [%expect {| ((dst (0 1 2 3 4 5)) (src (3 4 5))) |}];
    print_union [ 0; 1; 2 ] [ 1; 2; 3 ];
    [%expect {| ((dst (0 1 2 3)) (src (1 2 3))) |}]
  ;;

  let%template equal = (Hash_set.equal [@mode m]) [@@mode m = (local, global)]

  let%expect_test "deriving equal" =
    let module Hs = struct
      type t = { hs : Hash_set.M(Int).t } [@@deriving equal]

      let of_list lst = { hs = Hash_set.of_list (module Int) lst }
    end
    in
    require (Hs.equal (Hs.of_list []) (Hs.of_list []));
    require (not (Hs.equal (Hs.of_list [ 1 ]) (Hs.of_list [])));
    require (not (Hs.equal (Hs.of_list [ 1 ]) (Hs.of_list [ 2 ])));
    require (Hs.equal (Hs.of_list [ 1 ]) (Hs.of_list [ 1 ]))
  ;;

  let of_array = Hash_set.of_array

  let%expect_test "of_array" =
    let test array =
      print_s [%sexp (Hash_set.of_array (module Int) array : int Hash_set.t)]
    in
    test [||];
    [%expect {| () |}];
    test [| 1; 2; 3; 4; 5 |];
    [%expect {| (1 2 3 4 5) |}];
    test [| 5; 4; 3; 2; 1 |];
    [%expect {| (1 2 3 4 5) |}];
    test [| 3; 1; 4; 1; 5 |];
    [%expect {| (1 3 4 5) |}]
  ;;
end

(* This module exists to check, at compile-time, that [Creators] is a subset of
   [Creators_generic]. *)
module _ (M : Hash_set.Creators) :
  Hash_set.Creators_generic
  with type 'a t := 'a M.t
  with type 'a elt := 'a
  with type ('a, 'z) create_options := ('a, 'z) Hash_set.create_options = struct
  include M

  let create ?growth_allowed ?size m () = create ?growth_allowed ?size m
end
