open! Import
open! Int_conversions

let%test_module "pretty" =
  (module struct
    let check input output =
      List.for_all [ ""; "+"; "-" ] ~f:(fun prefix ->
        let input = prefix ^ input in
        let output = prefix ^ output in
        [%compare.equal: string] output (insert_underscores input))
    ;;

    let%test _ = check "1" "1"
    let%test _ = check "12" "12"
    let%test _ = check "123" "123"
    let%test _ = check "1234" "1_234"
    let%test _ = check "12345" "12_345"
    let%test _ = check "123456" "123_456"
    let%test _ = check "1234567" "1_234_567"
    let%test _ = check "12345678" "12_345_678"
    let%test _ = check "123456789" "123_456_789"
    let%test _ = check "1234567890" "1_234_567_890"
  end)
;;

let%test_module "conversions" =
  (module struct
    module type S = sig
      include Int.S

      val module_name : string
    end

    let test_conversion (type a b) loc ma mb a_to_b_or_error a_to_b_trunc b_to_a_trunc =
      let (module A : S with type t = a) = ma in
      let (module B : S with type t = b) = mb in
      let examples =
        [ A.min_value
        ; A.minus_one
        ; A.zero
        ; A.one
        ; A.max_value
        ; B.min_value |> b_to_a_trunc
        ; B.max_value |> b_to_a_trunc
        ]
        |> List.concat_map ~f:(fun a -> [ A.pred a; a; A.succ a ])
        |> List.dedup_and_sort ~compare:A.compare
        |> List.sort ~compare:A.compare
      in
      List.iter examples ~f:(fun a ->
        let b' = a_to_b_trunc a in
        let a' = b_to_a_trunc b' in
        match a_to_b_or_error a with
        | Ok b ->
          require
            loc
            (B.equal b b')
            ~if_false_then_print_s:
              (lazy
                [%message
                  "conversion produced wrong value"
                    ~from:(A.module_name : string)
                    ~to_:(B.module_name : string)
                    ~input:(a : A.t)
                    ~output:(b : B.t)
                    ~expected:(b' : B.t)]);
          require
            loc
            (A.equal a a')
            ~if_false_then_print_s:
              (lazy
                [%message
                  "conversion does not round-trip"
                    ~from:(A.module_name : string)
                    ~to_:(B.module_name : string)
                    ~input:(a : A.t)
                    ~output:(b : B.t)
                    ~round_trip:(a' : A.t)])
        | Error error ->
          require
            loc
            (not (A.equal a a'))
            ~if_false_then_print_s:
              (lazy
                [%message
                  "conversion failed"
                    ~from:(A.module_name : string)
                    ~to_:(B.module_name : string)
                    ~input:(a : A.t)
                    ~expected_output:(b' : B.t)
                    ~error:(error : Error.t)]))
    ;;

    let test loc ma mb (a_to_b_trunc, a_to_b_or_error) (b_to_a_trunc, b_to_a_or_error) =
      test_conversion loc ma mb a_to_b_or_error a_to_b_trunc b_to_a_trunc;
      test_conversion loc mb ma b_to_a_or_error b_to_a_trunc a_to_b_trunc
    ;;

    module Int = struct
      include Int

      let module_name = "Int"
    end

    module Int32 = struct
      include Int32

      let module_name = "Int32"
    end

    module Int64 = struct
      include Int64

      let module_name = "Int64"
    end

    module Nativeint = struct
      include Nativeint

      let module_name = "Nativeint"
    end

    let with_exn f x = Or_error.try_with (fun () -> f x)
    let optional f x = Or_error.try_with (fun () -> Option.value_exn (f x))
    let alwaysok f x = Ok (f x)

    let%expect_test "int <-> int32" =
      test
        [%here]
        (module Int)
        (module Int32)
        (Stdlib.Int32.of_int, with_exn int_to_int32_exn)
        (Stdlib.Int32.to_int, with_exn int32_to_int_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int)
        (module Int32)
        (Stdlib.Int32.of_int, optional int_to_int32)
        (Stdlib.Int32.to_int, optional int32_to_int);
      [%expect {| |}]
    ;;

    let%expect_test "int <-> int64" =
      test
        [%here]
        (module Int)
        (module Int64)
        (Stdlib.Int64.of_int, alwaysok int_to_int64)
        (Stdlib.Int64.to_int, with_exn int64_to_int_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int)
        (module Int64)
        (Stdlib.Int64.of_int, alwaysok int_to_int64)
        (Stdlib.Int64.to_int, optional int64_to_int);
      [%expect {| |}]
    ;;

    let%expect_test "int <-> nativeint" =
      test
        [%here]
        (module Int)
        (module Nativeint)
        (Stdlib.Nativeint.of_int, alwaysok int_to_nativeint)
        (Stdlib.Nativeint.to_int, with_exn nativeint_to_int_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int)
        (module Nativeint)
        (Stdlib.Nativeint.of_int, alwaysok int_to_nativeint)
        (Stdlib.Nativeint.to_int, optional nativeint_to_int);
      [%expect {| |}]
    ;;

    let%expect_test "int32 <-> int64" =
      test
        [%here]
        (module Int32)
        (module Int64)
        (Stdlib.Int64.of_int32, alwaysok int32_to_int64)
        (Stdlib.Int64.to_int32, with_exn int64_to_int32_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int32)
        (module Int64)
        (Stdlib.Int64.of_int32, alwaysok int32_to_int64)
        (Stdlib.Int64.to_int32, optional int64_to_int32);
      [%expect {| |}]
    ;;

    let%expect_test "int32 <-> nativeint" =
      test
        [%here]
        (module Int32)
        (module Nativeint)
        (Stdlib.Nativeint.of_int32, alwaysok int32_to_nativeint)
        (Stdlib.Nativeint.to_int32, with_exn nativeint_to_int32_exn);
      [%expect {| |}];
      test
        [%here]
        (module Int32)
        (module Nativeint)
        (Stdlib.Nativeint.of_int32, alwaysok int32_to_nativeint)
        (Stdlib.Nativeint.to_int32, optional nativeint_to_int32);
      [%expect {| |}]
    ;;

    let%expect_test "int64 <-> nativeint" =
      test
        [%here]
        (module Int64)
        (module Nativeint)
        (Stdlib.Int64.to_nativeint, with_exn int64_to_nativeint_exn)
        (Stdlib.Int64.of_nativeint, alwaysok nativeint_to_int64);
      [%expect {| |}];
      test
        [%here]
        (module Int64)
        (module Nativeint)
        (Stdlib.Int64.to_nativeint, optional int64_to_nativeint)
        (Stdlib.Int64.of_nativeint, alwaysok nativeint_to_int64);
      [%expect {| |}]
    ;;
  end)
;;

let%test_module "Make_hex" =
  (module struct
    module Hex_int = struct
      type t = int [@@deriving quickcheck]

      module M = Make_hex (struct
        type nonrec t = int [@@deriving sexp, compare ~localize, hash, quickcheck]

        let to_string = Int.Hex.to_string
        let of_string = Int.Hex.of_string
        let zero = 0
        let ( < ) = ( < )
        let neg = Int.neg
        let module_name = "Hex_int"
      end)

      include (M.Hex : module type of M.Hex with type t := t)
    end

    let%expect_test "validate sexp grammar" =
      require_ok [%here] (Sexp_grammar_validation.validate_grammar (module Hex_int));
      [%expect {| String |}]
    ;;
  end)
;;
