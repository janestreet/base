open! Base
open Base_quickcheck.Export
open Expect_test_helpers_core

module%test [@name "local mode vs global mode"] _ = struct
  module type Input = sig
    type t [@@deriving quickcheck, sexp_of]
  end

  module type Output = sig
    type t [@@deriving equal, globalize, sexp_of]
  end

  let test
    (type input output)
    (module Input : Input with type t = input)
    (module Output : Output with type t = output)
    ~local:(fn_local : local_ Input.t -> local_ Output.t)
    ~global:(fn_global : Input.t -> Output.t)
    =
    quickcheck_m (module Input) ~f:(fun input ->
      require_equal
        (module struct
          type t = Output.t Or_error.t [@@deriving equal, sexp_of]
        end)
        (Or_error.try_with (fun () ->
           [%globalize: Output.t]
             (require_no_allocation_local (fun () -> exclave_ fn_local input)) [@nontail]))
        (Or_error.try_with (fun () -> fn_global input)))
  ;;

  [%%template
  let%expect_test "value_local" =
    test
      ~local:((Option.value [@mode local]) ~default:0)
      ~global:(Option.value ~default:0)
      (module struct
        type t = int option [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int [@@deriving equal, globalize, sexp_of]
      end)
  ;;

  let%expect_test "value_local_exn" =
    let here = [%here] in
    test
      ~local:((Option.value_exn [@mode local]) ~here)
      ~global:(Option.value_exn ~here)
      (module struct
        type t = int option [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int [@@deriving equal, globalize, sexp_of]
      end)
  ;;

  let%expect_test "value_map_local" =
    test
      ~local:((Option.value_map [@mode local]) ~f:(fun x -> x + 1) ~default:0)
      ~global:(Option.value_map ~f:(fun x -> x + 1) ~default:0)
      (module struct
        type t = int option [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int [@@deriving equal, globalize, sexp_of]
      end)
  ;;

  let%expect_test "value_or_thunk_local" =
    test
      ~local:((Option.value_or_thunk [@mode local]) ~default:(fun () -> 0))
      ~global:(Option.value_or_thunk ~default:(fun () -> 0))
      (module struct
        type t = int option [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int [@@deriving equal, globalize, sexp_of]
      end)
  ;;

  let%expect_test "map_local" =
    test
      ~local:((Option.map [@mode local]) ~f:(fun x -> x + 1))
      ~global:(Option.map ~f:(fun x -> x + 1))
      (module struct
        type t = int option [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int option [@@deriving equal, globalize, sexp_of]
      end)
  ;;

  let%expect_test "to_list_local" =
    test
      ~local:(Option.to_list [@mode local])
      ~global:Option.to_list
      (module struct
        type t = int option [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int list [@@deriving equal, globalize, sexp_of]
      end)
  ;;

  let%expect_test "some_local" =
    test
      ~local:(Option.some [@mode local])
      ~global:Option.some
      (module struct
        type t = int [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int option [@@deriving equal, globalize, sexp_of]
      end)
  ;;

  let%expect_test "first_some_local" =
    test
      ~local:(fun (x, y) -> exclave_ (Option.first_some [@mode local]) x y)
      ~global:(fun (x, y) -> Option.first_some x y)
      (module struct
        type t = int option * int option [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int option [@@deriving equal, globalize, sexp_of]
      end)
  ;;

  let%expect_test "some_if_local" =
    test
      ~local:(fun (b, i) -> exclave_ (Option.some_if [@mode local]) b i)
      ~global:(fun (b, i) -> Option.some_if b i)
      (module struct
        type t = bool * int [@@deriving quickcheck, sexp_of]
      end)
      (module struct
        type t = int option [@@deriving equal, globalize, sexp_of]
      end)
  ;;]
end
