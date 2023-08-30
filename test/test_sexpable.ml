open! Import
open Sexpable

let%test_module "Of_stringable" =
  (module struct
    module Doubled_string = struct
      (* Example module with a partial [of_string] function *)

      type t = Double of string [@@deriving quickcheck]

      include Of_stringable (struct
        type nonrec t = t

        let to_string (Double x) = x ^ x

        let of_string x =
          let length = String.length x in
          let first_half = String.drop_suffix x (length / 2) in
          let second_half = String.drop_suffix x (length / 2) in
          if length % 2 = 0 && String.(first_half = second_half)
          then Double first_half
          else failwith [%string "Invalid doubled string %{x}"]
        ;;
      end)
    end

    let%expect_test "validate sexp grammar" =
      require_ok
        [%here]
        (Sexp_grammar_validation.validate_grammar (module Doubled_string));
      [%expect {| String |}]
    ;;
  end)
;;
