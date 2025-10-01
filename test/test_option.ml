open! Import
open! Option

let f = ( + )
let%test _ = [%compare.equal: int t] (merge None None ~f) None
let%test _ = [%compare.equal: int t] (merge (Some 3) None ~f) (Some 3)
let%test _ = [%compare.equal: int t] (merge None (Some 3) ~f) (Some 3)
let%test _ = [%compare.equal: int t] (merge (Some 1) (Some 3) ~f) (Some 4)

let%expect_test "[value_exn]" =
  (* Location information should be excluded when [here] is [Lexing.dummy_pos], which is
     the default value of [here] in the external version of Base *)
  Expect_test_helpers_base.require_does_raise ~hide_positions:true (fun () ->
    value_exn None);
  [%expect {| ("Option.value_exn None" lib/base/test/test_option.ml:LINE:COL) |}];
  Expect_test_helpers_base.require_does_raise (fun () ->
    value_exn None ~here:Lexing.dummy_pos);
  [%expect {| "Option.value_exn None" |}]
;;

let%expect_test "[value_or_thunk]" =
  let default () =
    print_endline "THUNK!";
    0
  in
  let value_or_thunk = value_or_thunk ~default in
  let test t = print_s [%sexp (value_or_thunk t : int)] in
  (* trigger the thunk *)
  test None;
  [%expect
    {|
    THUNK!
    0
    |}];
  (* same value, no trigger *)
  test (Some 0);
  [%expect {| 0 |}];
  (* different value *)
  test (Some 1);
  [%expect {| 1 |}];
  (* trigger the thunk again: no memoization *)
  test None;
  [%expect
    {|
    THUNK!
    0
    |}]
;;

let%expect_test "map2" =
  let m t1 t2 =
    let result = Option.map2 ~f:(fun x y -> x + y) t1 t2 in
    print_s [%sexp (result : int Option.t)]
  in
  m None None;
  [%expect {| () |}];
  m (Some 1) (Some 2);
  [%expect {| (3) |}];
  m None (Some 1);
  [%expect {| () |}];
  m (Some 1) None;
  [%expect {| () |}]
;;

[%%template
let%expect_test "some_if_thunk{,_local}" =
  let print_opt = (Option.iter [@mode local]) ~f:(fun x -> Stdlib.print_int x) in
  let run_test some_if_thunk =
    (* In the [false] case, don't run the thunk *)
    print_opt (some_if_thunk false (fun () -> assert false));
    [%expect {| |}];
    (* In the [true] case, do run the thunk *)
    some_if_thunk true (fun () ->
      print_endline "running";
      1)
    |> print_opt;
    [%expect
      {|
      running
      1
      |}]
  in
  run_test (fun b f -> exclave_ [%template some_if_thunk [@mode local]] b f);
  run_test some_if_thunk
;;

let%expect_test "first_some_thunk" =
  let print_opt = (Option.iter [@mode local]) ~f:(fun x -> Stdlib.print_int x) in
  print_opt (first_some_thunk None (Fn.const None));
  [%expect {| |}];
  print_opt (first_some_thunk (Some 1) (fun () -> failwith "should not be run"));
  [%expect {| 1 |}];
  print_opt (first_some_thunk None (Fn.const (Some 2)));
  [%expect {| 2 |}]
;;]

let%expect_test "local Let_syntax" =
  let print_opt = (Option.iter [@mode local]) ~f:(fun x -> Stdlib.print_int x) in
  let open Option.Local.Let_syntax in
  let one = Some 1 in
  let a =
    let%mapl one in
    one + 1
  in
  print_opt a;
  [%expect {| 2 |}];
  let b =
    let%bindl one in
    Some (one + 2)
  in
  print_opt b;
  [%expect {| 3 |}];
  let c =
    let%mapl one
    and two = Some 2
    and three = Some 3 in
    one + two + three
  in
  print_opt c;
  [%expect {| 6 |}];
  let d =
    let%bindl one in
    return one
  in
  print_opt d;
  [%expect {| 1 |}]
;;

module%test Test_sexp = struct
  [%%template
  module
    [@kind k = (float64, bits32, bits64, word, value)] Test_one (T : sig
      type t : k [@@deriving sexp ~stackify]

      val of_int : int -> t
    end) : sig end = struct
    open T

    type nonrec t = (t Option.t[@kind k]) [@@deriving sexp ~stackify]

    let%expect_test "serialize" =
      let five : t = Some (of_int 5) in
      five |> sexp_of_t |> print_s;
      five |> (sexp_of_t [@alloc stack]) |> Sexp.globalize |> print_s;
      [%expect
        {|
        (5)
        (5)
        |}];
      let none : t = None in
      none |> sexp_of_t |> print_s;
      none |> sexp_of_t |> Sexp.globalize |> print_s;
      [%expect
        {|
        ()
        ()
        |}]
    ;;

    let%expect_test "serialize (explicit longhand)" =
      Dynamic.with_temporarily
        Sexplib0.Sexp_conv.write_old_option_format
        false
        ~f:(fun () ->
          let five : t = Some (of_int 5) in
          five |> sexp_of_t |> print_s;
          five |> (sexp_of_t [@alloc stack]) |> Sexp.globalize |> print_s;
          [%expect
            {|
            (some 5)
            (some 5)
            |}];
          let none : t = None in
          none |> sexp_of_t |> print_s;
          none |> sexp_of_t |> Sexp.globalize |> print_s;
          [%expect
            {|
            none
            none
            |}])
    ;;

    let%expect_test "deserialize longhand format" =
      let five = Sexp.List [ Sexp.Atom "Some"; Sexp.Atom "5" ] in
      five |> t_of_sexp |> sexp_of_t |> print_s;
      [%expect {| (5) |}];
      let none = Sexp.Atom "None" in
      none |> t_of_sexp |> sexp_of_t |> print_s;
      [%expect {| () |}]
    ;;

    let%expect_test "deserialize shorthand format" =
      let five = [%sexp_of: int option] (Some 5) in
      five |> t_of_sexp |> sexp_of_t |> print_s;
      [%expect {| (5) |}];
      let none = [%sexp_of: int option] None in
      none |> t_of_sexp |> sexp_of_t |> print_s;
      [%expect {| () |}];
      Dynamic.with_temporarily
        Sexplib0.Sexp_conv.read_old_option_format
        false
        ~f:(fun () ->
          Expect_test_helpers_base.require_does_raise
            ~cr:CR_soon
            ~hide_positions:true
            (fun () ->
               let five = [%sexp_of: int option] (Some 5) in
               five |> t_of_sexp |> sexp_of_t |> print_s);
          [%expect
            {| (Of_sexp_error "option_of_sexp: list must be (some el)" (invalid_sexp (5))) |}];
          Expect_test_helpers_base.require_does_raise
            ~cr:CR_soon
            ~hide_positions:true
            (fun () ->
               let none = [%sexp_of: int option] None in
               none |> t_of_sexp |> sexp_of_t |> print_s);
          [%expect
            {| (Of_sexp_error "option_of_sexp: list must be (some el)" (invalid_sexp ())) |}])
    ;;
  end

  module _ = Test_one [@kind float64] (Float_u)
  module _ = Test_one [@kind word] (Nativeint_u)
  module _ = Test_one [@kind bits64] (Int64_u)

  module _ = Test_one [@kind bits32] (struct
      include Int32_u

      let of_int = of_int_exn
    end)

  module _ = Test_one [@kind value] (struct
      include String

      let of_int = Int.to_string
    end)]
end
