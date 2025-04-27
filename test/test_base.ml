open! Import

let%expect_test _ =
  let f x = x * 2 in
  let g x = x + 3 in
  print_s [%sexp (f @@ 5 : int)];
  [%expect {| 10 |}];
  print_s [%sexp (g @@ f @@ 5 : int)];
  [%expect {| 13 |}];
  print_s [%sexp (f @@ g @@ 5 : int)];
  [%expect {| 16 |}]
;;

let%expect_test "exp is present at the toplevel" =
  print_s [%sexp (2 ** 8 : int)];
  [%expect {| 256 |}]
;;

(* The goal of these tests is to check the behavior of layout-polymorphic primitives like
   [ignore]. We use ppx_template as a convenient way to bind functions of different
   layouts to mangled identifiers that we can access with [@kind] syntax. This prevents
   some code duplication, and makes it obvious which layout(s) we're testing. *)
module%test [@name "layout polymorphism"] _ =
  [%template
  include (
  struct
    type t = int [@@kind immediate]
    and t = nativeint# [@@kind word]
    and t = int32# [@@kind bits32]
    and t = int64# [@@kind bits64]
    and t = float# [@@kind float64]

    let[@kind immediate] zero () = 0
    and[@kind word] zero () = #0n
    and[@kind bits32] zero () = #0l
    and[@kind bits64] zero () = #0L
    and[@kind float64] zero () = #0.

    let[@kind immediate] is_zero = function
      | 0 -> true
      | _ -> false

    and[@kind word] is_zero = function
      | #0n -> true
      | _ -> false

    and[@kind bits32] is_zero = function
      | #0l -> true
      | _ -> false

    and[@kind bits64] is_zero = function
      | #0L -> true
      | _ -> false

    and[@kind float64] is_zero = function
      | #0. -> true
      | _ -> false
    ;;
  end :
  sig
    [@@@kind.default k = (immediate, word, bits32, bits64, float64)]

    type t : k

    val zero : unit -> (t[@kind k])
    val is_zero : (t[@kind k]) -> bool
  end)

  [@@@kind k = (immediate, word, bits32, bits64, float64)]

  (* Ignore a value with each of the specified layouts. *)
  let%test_unit "ignore" = ignore ((zero [@kind k]) () : (t[@kind k]))

  (* Apply [Fn.id >> is_zero] to a value with each of the specified layouts. *)
  let%test "apply/revapply identity" = Fn.id @@ (zero [@kind k]) () |> (is_zero [@kind k])

  (* As above but testing %opaque rather than %identity. *)
  let%test "apply/revapply opaque" =
    Sys.opaque_identity @@ (zero [@kind k]) () |> (is_zero [@kind k])
  ;;]
