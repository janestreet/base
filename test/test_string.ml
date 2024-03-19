open! Import
open! String

let%expect_test ("hash coherence" [@tags "64-bits-only"]) =
  check_hash_coherence [%here] (module String) [ ""; "a"; "foo" ];
  [%expect {| |}]
;;

let%expect_test "edit distance" =
  let strings = [ "catch"; "patch"; "pitch"; "pith"; "pits"; "spits"; "spots" ] in
  List.iteri strings ~f:(fun i a ->
    List.iteri strings ~f:(fun j b ->
      if Int.( >= ) i j
      then (
        let d = edit_distance a b in
        print_s [%sexp (d : int), (a : string), (b : string)];
        if a = b && Int.( <> ) d 0 then print_cr [%here] [%message "non-zero"];
        let d' = edit_distance b a in
        if Int.( <> ) d d'
        then print_cr [%here] [%message "non-symmetric" ~_:(d : int) ~_:(d' : int)])));
  [%expect
    {|
    (0 catch catch)
    (1 patch catch)
    (0 patch patch)
    (2 pitch catch)
    (1 pitch patch)
    (0 pitch pitch)
    (3 pith catch)
    (2 pith patch)
    (1 pith pitch)
    (0 pith pith)
    (4 pits catch)
    (3 pits patch)
    (2 pits pitch)
    (1 pits pith)
    (0 pits pits)
    (5 spits catch)
    (4 spits patch)
    (3 spits pitch)
    (2 spits pith)
    (1 spits pits)
    (0 spits spits)
    (5 spots catch)
    (4 spots patch)
    (4 spots pitch)
    (3 spots pith)
    (2 spots pits)
    (1 spots spits)
    (0 spots spots)
    |}]
;;

let%test_module "concat" =
  (module struct
    let test ?sep list =
      let from_list = concat ?sep list in
      let from_array = concat_array ?sep (Array.of_list list) in
      require_equal [%here] (module String) from_list from_array;
      print_s [%sexp (from_list : string)]
    ;;

    let%expect_test "empty" =
      test [] ~sep:":";
      [%expect {| "" |}]
    ;;

    let%expect_test "singleton" =
      test [ "a" ];
      [%expect {| a |}]
    ;;

    let%expect_test "empty separator" =
      test [ "a"; "b" ];
      [%expect {| ab |}]
    ;;

    let%expect_test "non-empty separator" =
      test [ "a"; "b" ] ~sep:":";
      [%expect {| a:b |}]
    ;;
  end)
;;

let%expect_test "to_list and to_list_rev" =
  let test s =
    let list = to_list s in
    require_equal
      [%here]
      (module struct
        type t = char list [@@deriving equal, sexp_of]
      end)
      (to_list_rev s)
      (List.rev list);
    print_s [%sexp (list : char list)]
  in
  test "";
  [%expect {| () |}];
  test "bladderwrack";
  [%expect {| (b l a d d e r w r a c k) |}]
;;

let%expect_test "[of_sequence] and [to_sequence]" =
  let test t =
    let seq = to_sequence t in
    print_s [%sexp (seq : char Sequence.t)];
    require_equal [%here] (module String) (of_sequence seq) t;
    require_equal
      [%here]
      (module struct
        type t = char Sequence.t [@@deriving equal, sexp_of]
      end)
      seq
      (Sequence.of_list (to_list t))
  in
  test "";
  [%expect {| () |}];
  test "a";
  [%expect {| (a) |}];
  test "ab";
  [%expect {| (a b) |}];
  test "abc";
  [%expect {| (a b c) |}];
  test "lorem ipsum dolor sit amet";
  [%expect {| (l o r e m " " i p s u m " " d o l o r " " s i t " " a m e t) |}]
;;

let%expect_test "sub/unsafe_sub" =
  let test ~pos ~len =
    let string = "0123456789" in
    match Or_error.try_with (fun () -> sub string ~pos ~len) with
    | Ok safe_substring ->
      let unsafe_substring = unsafe_sub string ~pos ~len in
      require_equal [%here] (module String) safe_substring unsafe_substring;
      print_s [%sexp (safe_substring : t)]
    | Error e -> print_s [%sexp (e : Error.t)]
  in
  test ~pos:0 ~len:0;
  [%expect {| "" |}];
  test ~pos:0 ~len:5;
  [%expect {| 01234 |}];
  test ~pos:0 ~len:10;
  [%expect {| 0123456789 |}];
  test ~pos:0 ~len:11;
  [%expect {| (Invalid_argument "pos + len past end: 0 + 11 > 10") |}];
  test ~pos:1 ~len:5;
  [%expect {| 12345 |}];
  test ~pos:1 ~len:10;
  [%expect {| (Invalid_argument "pos + len past end: 1 + 10 > 10") |}];
  test ~pos:9 ~len:1;
  [%expect {| 9 |}];
  test ~pos:9 ~len:2;
  [%expect {| (Invalid_argument "pos + len past end: 9 + 2 > 10") |}];
  test ~pos:10 ~len:0;
  [%expect {| "" |}];
  test ~pos:10 ~len:1;
  [%expect {| (Invalid_argument "pos + len past end: 10 + 1 > 10") |}]
;;

let%test_module "Unicode" =
  (module struct
    let encodings : (module Utf) list =
      [ (module Utf8)
      ; (module Utf16le)
      ; (module Utf16be)
      ; (module Utf32le)
      ; (module Utf32be)
      ]
    ;;

    let test_validity (module Utf : Utf) string ~expect =
      let actual = (Utf.is_valid string : bool) in
      match actual, expect with
      | true, true | false, false -> ()
      | true, false -> print_cr [%here] [%message "expected valid result, got invalid"]
      | false, true -> print_cr [%here] [%message "expected invalid result, got valid"]
    ;;

    let%expect_test "valid" =
      let test_validity = test_validity ~expect:true in
      (* Valid UTF-8 encoding for ASCII 'a' *)
      test_validity (module Utf8) "\x61";
      [%expect {| |}];
      (* Valid UTF-16LE encoding for ASCII 'a' *)
      test_validity (module Utf16le) "\x61\x00";
      [%expect {| |}];
      (* Valid UTF-16BE encoding for ASCII 'a' *)
      test_validity (module Utf16be) "\x00\x61";
      [%expect {| |}];
      (* Valid UTF-32LE encoding for ASCII 'a' *)
      test_validity (module Utf32le) "\x61\x00\x00\x00";
      [%expect {| |}];
      (* Valid UTF-32BE encoding for ASCII 'a' *)
      test_validity (module Utf32be) "\x00\x00\x00\x61";
      [%expect {| |}];
      (* Valid UTF-8 encoding for 'aö' *)
      test_validity (module Utf8) "\x61\xc3\xb6";
      [%expect {| |}];
      (* Valid UTF-16LE encoding for 'aö' *)
      test_validity (module Utf16le) "\x61\x00\xf6\x00";
      [%expect {| |}];
      (* Valid UTF-16BE encoding for 'aö' *)
      test_validity (module Utf16be) "\x00\x61\x00\xf6";
      [%expect {| |}];
      (* Valid UTF-32LE encoding for 'aö' *)
      test_validity (module Utf32le) "\x61\x00\x00\x00\xf6\x00\x00\x00";
      [%expect {| |}];
      (* Valid UTF-32BE encoding for 'aö' *)
      test_validity (module Utf32be) "\x00\x00\x00\x61\x00\x00\x00\xf6";
      [%expect {| |}];
      (* Valid UTF-16LE encoding for '𝕬' *)
      test_validity (module Utf16le) "\x35\xd8\x6c\xdd";
      [%expect {| |}];
      (* Valid UTF-16LE encoding for '𝕬' *)
      test_validity (module Utf16be) "\xd8\x35\xdd\x6c";
      [%expect {| |}]
    ;;

    let%expect_test "invalid" =
      let test_validity = test_validity ~expect:false in
      (* Invalid UTF-8: Premature end-of-string after start of 2-byte sequence *)
      test_validity (module Utf8) "\x61\xc3";
      [%expect {| |}];
      (* Invalid UTF-8: second byte does not start with bits '10' *)
      test_validity (module Utf8) "\xc3\x28";
      [%expect {| |}];
      (* Invalid UTF-8: surrogate pair encoded in UTF-8 *)
      test_validity (module Utf8) "\xed\xa0\x80";
      [%expect {| |}];
      (* Invalid UTF-8: ASCII character "/" (U+002F) is encoded in an overlong form *)
      test_validity (module Utf8) "\xc0\xaf";
      [%expect {| |}];
      (* Invalid UTF-8: The encoded value is outside the Unicode code point range *)
      test_validity (module Utf8) "\xc0\x28";
      [%expect {| |}];
      (* Invalid UTF-16LE: Single byte is not a complete UTF-16 character *)
      test_validity (module Utf16le) "\x61";
      [%expect {| |}];
      (* Invalid UTF-16BE: Single byte is not a complete UTF-16 character *)
      test_validity (module Utf16be) "\x61";
      [%expect {| |}];
      (* Invalid UTF-32LE: Only 3 bytes is not a complete UTF-32 character *)
      test_validity (module Utf32le) "\x61\x00\x00";
      [%expect {| |}];
      (* Invalid UTF-32BE: Only 3 bytes is not a complete UTF-32 character *)
      test_validity (module Utf32be) "\x61\x00\x00";
      [%expect {| |}];
      (* Invalid UTF-16LE: high surrogate not followed by low surrogate *)
      test_validity (module Utf16le) "\x00\xd8\x00\x00";
      [%expect {| |}];
      (* Invalid UTF-16BE: high surrogate not followed by low surrogate *)
      test_validity (module Utf16be) "\xd8\x00\x00\x00";
      [%expect {| |}];
      (* Invalid UTF-32LE: surrogate pair encoded in UTF-32 *)
      test_validity (module Utf32le) "\x00\xD8\x00\x00";
      [%expect {| |}];
      (* Invalid UTF-32BE: surrogate pair encoded in UTF-32 *)
      test_validity (module Utf32be) "\x00\x00\xD8\x00";
      [%expect {| |}]
    ;;

    let test_conversions utf8 =
      let utf8 =
        match Utf8.of_string utf8 with
        | utf8 -> utf8
        | exception exn ->
          print_s [%sexp (exn : exn)];
          Utf8.sanitize utf8
      in
      let uchars = Utf8.to_list utf8 in
      printf "Appearance: %s\n" (Utf8.of_list uchars :> string);
      print_s [%sexp (uchars : Uchar.t list)];
      List.iter encodings ~f:(fun (module Utf : Utf) ->
        let codec_name = Utf.codec_name in
        let t = Utf.of_list uchars in
        print_s [%sexp (codec_name : string), (t : Utf.t)];
        let round_trip = Utf.to_list t in
        if not ([%equal: Uchar.t list] uchars round_trip)
        then
          print_cr
            [%here]
            [%message
              "encoding does not round trip"
                (codec_name : string)
                (uchars : Uchar.t list)
                ~string:(t : Utf.t)
                (round_trip : Uchar.t list)])
    ;;

    let%expect_test "conversions" =
      test_conversions "abc";
      [%expect
        {|
        Appearance: abc
        (U+0061 U+0062 U+0063)
        (UTF-8 abc)
        (UTF-16LE "a\000b\000c\000")
        (UTF-16BE "\000a\000b\000c")
        (UTF-32LE "a\000\000\000b\000\000\000c\000\000\000")
        (UTF-32BE "\000\000\000a\000\000\000b\000\000\000c")
        |}];
      test_conversions "\u{0065}\u{0301}";
      [%expect
        {|
        Appearance: é
        (U+0065 U+0301)
        (UTF-8 "e\204\129")
        (UTF-16LE "e\000\001\003")
        (UTF-16BE "\000e\003\001")
        (UTF-32LE "e\000\000\000\001\003\000\000")
        (UTF-32BE "\000\000\000e\000\000\003\001")
        |}];
      test_conversions "\u{0063}\u{030C}";
      [%expect
        {|
        Appearance: č
        (U+0063 U+030C)
        (UTF-8 "c\204\140")
        (UTF-16LE "c\000\012\003")
        (UTF-16BE "\000c\003\012")
        (UTF-32LE "c\000\000\000\012\003\000\000")
        (UTF-32BE "\000\000\000c\000\000\003\012")
        |}];
      test_conversions "\u{0E28}\u{0E34}";
      [%expect
        {|
        Appearance: ศิ
        (U+0E28 U+0E34)
        (UTF-8 "\224\184\168\224\184\180")
        (UTF-16LE "(\0144\014")
        (UTF-16BE "\014(\0144")
        (UTF-32LE "(\014\000\0004\014\000\000")
        (UTF-32BE "\000\000\014(\000\000\0144")
        |}];
      test_conversions "\u{1D11E}";
      [%expect
        {|
        Appearance: 𝄞
        (U+1D11E)
        (UTF-8 "\240\157\132\158")
        (UTF-16LE "4\216\030\221")
        (UTF-16BE "\2164\221\030")
        (UTF-32LE "\030\209\001\000")
        (UTF-32BE "\000\001\209\030")
        |}];
      test_conversions "\u{1D56C}";
      [%expect
        {|
        Appearance: 𝕬
        (U+1D56C)
        (UTF-8 "\240\157\149\172")
        (UTF-16LE "5\216l\221")
        (UTF-16BE "\2165\221l")
        (UTF-32LE "l\213\001\000")
        (UTF-32BE "\000\001\213l")
        |}];
      test_conversions "\xFF\xFF";
      [%expect
        {|
        ("Base.String.Utf8.of_string: invalid UTF-8" "\255\255")
        Appearance: ��
        (U+FFFD U+FFFD)
        (UTF-8 "\239\191\189\239\191\189")
        (UTF-16LE "\253\255\253\255")
        (UTF-16BE "\255\253\255\253")
        (UTF-32LE "\253\255\000\000\253\255\000\000")
        (UTF-32BE "\000\000\255\253\000\000\255\253")
        |}]
    ;;

    let%expect_test "Test [get] used at an invalid offset" =
      let utf8 = Utf8.of_string "αβ" in
      printf "%s\n" (utf8 :> string);
      [%expect {| αβ |}];
      print_s [%message "" ~_:(utf8 : Utf8.t) ~_:(Utf8.to_list utf8 : Uchar.t list)];
      [%expect {| ("\206\177\206\178" (U+03B1 U+03B2)) |}];
      require_does_raise [%here] (fun () -> Utf8.get utf8 ~byte_pos:1);
      [%expect
        {|
        ("Base.String.Utf8.get: invalid UTF-8 encoding at given position"
         "\206\177\206\178"
         (pos 1))
        |}]
    ;;
  end)
;;

let%test_module "Caseless Suffix/Prefix" =
  (module struct
    let%test _ = Caseless.is_suffix "OCaml" ~suffix:"AmL"
    let%test _ = Caseless.is_suffix "OCaml" ~suffix:"ocAmL"
    let%test _ = Caseless.is_suffix "a@!$b" ~suffix:"a@!$B"
    let%test _ = not (Caseless.is_suffix "a@!$b" ~suffix:"C@!$B")
    let%test _ = not (Caseless.is_suffix "aa" ~suffix:"aaa")
    let%test _ = Caseless.is_prefix "OCaml" ~prefix:"oc"
    let%test _ = Caseless.is_prefix "OCaml" ~prefix:"ocAmL"
    let%test _ = Caseless.is_prefix "a@!$b" ~prefix:"a@!$B"
    let%test _ = not (Caseless.is_prefix "a@!$b" ~prefix:"a@!$C")
    let%test _ = not (Caseless.is_prefix "aa" ~prefix:"aaa")
  end)
;;

let%test_module "Caseless Substring" =
  (module struct
    let%test _ = Caseless.is_substring "OCaml" ~substring:"AmL"
    let%test _ = Caseless.is_substring "OCaml" ~substring:"oc"
    let%test _ = Caseless.is_substring "OCaml" ~substring:"ocAmL"
    let%test _ = Caseless.is_substring "a@!$b" ~substring:"a@!$B"
    let%test _ = not (Caseless.is_substring "a@!$b" ~substring:"C@!$B")
    let%test _ = not (Caseless.is_substring "a@!$b" ~substring:"a@!$C")
    let%test _ = not (Caseless.is_substring "aa" ~substring:"aaa")
    let%test _ = not (Caseless.is_substring "aa" ~substring:"AAA")

    let%test_unit _ =
      Base_quickcheck.Test.run_exn
        (module struct
          type t = string * string [@@deriving quickcheck, sexp_of]
        end)
        ~f:(fun (t, substring) ->
          let actual = Caseless.is_substring t ~substring in
          let expect = is_substring (lowercase t) ~substring:(lowercase substring) in
          [%test_result: bool] actual ~expect)
    ;;
  end)
;;

let%test_module "Caseless Comparable" =
  (module struct
    (* examples from docs *)
    let%test _ = Caseless.equal "OCaml" "ocaml"
    let%test _ = Caseless.("apple" < "Banana")
    let%test _ = Caseless.("aa" < "aaa")
    let%test _ = Int.( <> ) (Caseless.compare "apple" "Banana") (compare "apple" "Banana")
    let%test _ = Caseless.equal "XxX" "xXx"
    let%test _ = Caseless.("XxX" < "xXxX")
    let%test _ = Caseless.("XxXx" > "xXx")

    let%test _ =
      List.is_sorted ~compare:Caseless.compare [ "Apples"; "bananas"; "Carrots" ]
    ;;
  end)
;;

let%test_module "Caseless Hashable" =
  (module struct
    let%test _ =
      Int.( <> ) (hash "x") (hash "X")
      && Int.( = ) (Caseless.hash "x") (Caseless.hash "X")
    ;;

    let%test _ = Int.( = ) (Caseless.hash "OCaml") (Caseless.hash "ocaml")
    let%test _ = Int.( <> ) (Caseless.hash "aaa") (Caseless.hash "aaaa")
    let%test _ = Int.( <> ) (Caseless.hash "aaa") (Caseless.hash "aab")

    let%test _ =
      let tbl = Hashtbl.create (module Caseless) in
      Hashtbl.add_exn tbl ~key:"x" ~data:7;
      [%compare.equal: int option] (Hashtbl.find tbl "X") (Some 7)
    ;;
  end)
;;

let%test _ = not (contains "" 'a')
let%test _ = contains "a" 'a'
let%test _ = not (contains "a" 'b')
let%test _ = contains "ab" 'a'
let%test _ = contains "ab" 'b'
let%test _ = not (contains "ab" 'c')
let%test _ = not (contains "abcd" 'b' ~pos:1 ~len:0)
let%test _ = contains "abcd" 'b' ~pos:1 ~len:1
let%test _ = contains "abcd" 'c' ~pos:1 ~len:2
let%test _ = not (contains "abcd" 'd' ~pos:1 ~len:2)
let%test _ = contains "abcd" 'd' ~pos:1
let%test _ = not (contains "abcd" 'a' ~pos:1)

let%test_module "Search_pattern" =
  (module struct
    open Search_pattern

    let%test_module "Search_pattern.create" =
      (module struct
        let prefix s n = sub s ~pos:0 ~len:n
        let suffix s n = sub s ~pos:(length s - n) ~len:n

        let slow_create pattern ~case_sensitive =
          let string_equal =
            if case_sensitive then String.equal else String.Caseless.equal
          in
          (* Compute the longest prefix-suffix array from definition, O(n^3) *)
          let n = length pattern in
          let kmp_array = Array.create ~len:n (-1) in
          for i = 0 to n - 1 do
            let x = prefix pattern (i + 1) in
            for j = 0 to i do
              if string_equal (prefix x j) (suffix x j) then kmp_array.(i) <- j
            done
          done;
          ({ pattern; kmp_array; case_sensitive } : Private.t)
        ;;

        let test_both ({ pattern; case_sensitive; kmp_array = _ } as expected : Private.t)
          =
          let create_repr = Private.representation (create pattern ~case_sensitive) in
          let slow_create_repr = slow_create pattern ~case_sensitive in
          require_equal [%here] (module Private) create_repr expected;
          require_equal [%here] (module Private) slow_create_repr expected
        ;;

        let cmp_both pattern ~case_sensitive =
          let create_repr = Private.representation (create pattern ~case_sensitive) in
          let slow_create_repr = slow_create pattern ~case_sensitive in
          require_equal [%here] (module Private) create_repr slow_create_repr
        ;;

        let%expect_test _ =
          List.iter [%all: bool] ~f:(fun case_sensitive ->
            test_both { pattern = ""; case_sensitive; kmp_array = [||] })
        ;;

        let%expect_test _ =
          List.iter [%all: bool] ~f:(fun case_sensitive ->
            test_both
              { pattern = "ababab"; case_sensitive; kmp_array = [| 0; 0; 1; 2; 3; 4 |] })
        ;;

        let%expect_test _ =
          List.iter [%all: bool] ~f:(fun case_sensitive ->
            test_both
              { pattern = "abaCabaD"
              ; case_sensitive
              ; kmp_array = [| 0; 0; 1; 0; 1; 2; 3; 0 |]
              })
        ;;

        let%expect_test _ =
          List.iter [%all: bool] ~f:(fun case_sensitive ->
            test_both
              { pattern = "abaCabaDabaCabaCabaDabaCabaEabab"
              ; case_sensitive
              ; kmp_array =
                  [| 0
                   ; 0
                   ; 1
                   ; 0
                   ; 1
                   ; 2
                   ; 3
                   ; 0
                   ; 1
                   ; 2
                   ; 3
                   ; 4
                   ; 5
                   ; 6
                   ; 7
                   ; 4
                   ; 5
                   ; 6
                   ; 7
                   ; 8
                   ; 9
                   ; 10
                   ; 11
                   ; 12
                   ; 13
                   ; 14
                   ; 15
                   ; 0
                   ; 1
                   ; 2
                   ; 3
                   ; 2
                  |]
              })
        ;;

        let%expect_test _ =
          test_both { pattern = "aaA"; case_sensitive = true; kmp_array = [| 0; 1; 0 |] }
        ;;

        let%expect_test _ =
          test_both { pattern = "aaA"; case_sensitive = false; kmp_array = [| 0; 1; 2 |] }
        ;;

        let%expect_test _ =
          test_both
            { pattern = "aAaAaA"
            ; case_sensitive = true
            ; kmp_array = [| 0; 0; 1; 2; 3; 4 |]
            }
        ;;

        let%expect_test _ =
          test_both
            { pattern = "aAaAaA"
            ; case_sensitive = false
            ; kmp_array = [| 0; 1; 2; 3; 4; 5 |]
            }
        ;;

        let rec x k =
          if Int.( < ) k 0
          then ""
          else (
            let b = x (k - 1) in
            b ^ make 1 (Stdlib.Char.unsafe_chr (65 + k)) ^ b)
        ;;

        let%expect_test _ =
          List.iter [%all: bool] ~f:(fun case_sensitive ->
            cmp_both ~case_sensitive (x 10))
        ;;

        let%expect_test _ =
          List.iter [%all: bool] ~f:(fun case_sensitive ->
            cmp_both ~case_sensitive (x 5 ^ "E" ^ x 4 ^ "D" ^ x 3 ^ "B" ^ x 2 ^ "C" ^ x 3))
        ;;

        let%test_unit _ =
          Base_quickcheck.Test.run_exn
            (module struct
              type t = string [@@deriving quickcheck, sexp_of]
            end)
            ~f:(fun pattern ->
              let case_insensitive =
                Private.representation (create pattern ~case_sensitive:false)
              in
              let case_sensitive_but_lowercase =
                Private.representation (create (lowercase pattern) ~case_sensitive:true)
              in
              [%test_result: String.Caseless.t]
                case_insensitive.pattern
                ~expect:case_sensitive_but_lowercase.pattern;
              [%test_result: int array]
                case_insensitive.kmp_array
                ~expect:case_sensitive_but_lowercase.kmp_array)
        ;;
      end)
    ;;

    let ( = ) = [%compare.equal: int option]
    let%test _ = index (create "") ~in_:"abababac" = Some 0
    let%test _ = index ~pos:(-1) (create "") ~in_:"abababac" = None
    let%test _ = index ~pos:1 (create "") ~in_:"abababac" = Some 1
    let%test _ = index ~pos:7 (create "") ~in_:"abababac" = Some 7
    let%test _ = index ~pos:8 (create "") ~in_:"abababac" = Some 8
    let%test _ = index ~pos:9 (create "") ~in_:"abababac" = None
    let%test _ = index (create "abababaca") ~in_:"abababac" = None
    let%test _ = index (create "abababac") ~in_:"abababac" = Some 0
    let%test _ = index ~pos:0 (create "abababac") ~in_:"abababac" = Some 0
    let%test _ = index (create "abac") ~in_:"abababac" = Some 4
    let%test _ = index ~pos:4 (create "abac") ~in_:"abababac" = Some 4
    let%test _ = index ~pos:5 (create "abac") ~in_:"abababac" = None
    let%test _ = index ~pos:5 (create "abac") ~in_:"abababaca" = None
    let%test _ = index ~pos:5 (create "baca") ~in_:"abababaca" = Some 5
    let%test _ = index ~pos:(-1) (create "a") ~in_:"abc" = None
    let%test _ = index ~pos:2 (create "a") ~in_:"abc" = None
    let%test _ = index ~pos:2 (create "c") ~in_:"abc" = Some 2
    let%test _ = index ~pos:3 (create "c") ~in_:"abc" = None
    let ( = ) = [%compare.equal: bool]
    let%test _ = matches (create "") "abababac" = true
    let%test _ = matches (create "abababaca") "abababac" = false
    let%test _ = matches (create "abababac") "abababac" = true
    let%test _ = matches (create "abac") "abababac" = true
    let%test _ = matches (create "abac") "abababaca" = true
    let%test _ = matches (create "baca") "abababaca" = true
    let%test _ = matches (create "a") "abc" = true
    let%test _ = matches (create "c") "abc" = true
    let ( = ) = [%compare.equal: int list]
    let%test _ = index_all (create "") ~may_overlap:false ~in_:"abcd" = [ 0; 1; 2; 3; 4 ]
    let%test _ = index_all (create "") ~may_overlap:true ~in_:"abcd" = [ 0; 1; 2; 3; 4 ]
    let%test _ = index_all (create "abab") ~may_overlap:false ~in_:"abababab" = [ 0; 4 ]
    let%test _ = index_all (create "abab") ~may_overlap:true ~in_:"abababab" = [ 0; 2; 4 ]
    let%test _ = index_all (create "abab") ~may_overlap:false ~in_:"ababababab" = [ 0; 4 ]

    let%test _ =
      index_all (create "abab") ~may_overlap:true ~in_:"ababababab" = [ 0; 2; 4; 6 ]
    ;;

    let%test _ =
      index_all (create "aaa") ~may_overlap:false ~in_:"aaaaBaaaaaa" = [ 0; 5; 8 ]
    ;;

    let%test _ =
      index_all (create "aaa") ~may_overlap:true ~in_:"aaaaBaaaaaa" = [ 0; 1; 5; 6; 7; 8 ]
    ;;

    let ( = ) = [%compare.equal: string]
    let%test _ = replace_first (create "abab") ~in_:"abababab" ~with_:"" = "abab"
    let%test _ = replace_first (create "abab") ~in_:"abacabab" ~with_:"" = "abac"
    let%test _ = replace_first (create "abab") ~in_:"ababacab" ~with_:"A" = "Aacab"
    let%test _ = replace_first (create "abab") ~in_:"acabababab" ~with_:"A" = "acAabab"
    let%test _ = replace_first (create "ababab") ~in_:"acabababab" ~with_:"A" = "acAab"

    let%test _ =
      replace_first (create "abab") ~in_:"abababab" ~with_:"abababab" = "abababababab"
    ;;

    let%test _ = replace_all (create "abab") ~in_:"abababab" ~with_:"" = ""
    let%test _ = replace_all (create "abab") ~in_:"abacabab" ~with_:"" = "abac"
    let%test _ = replace_all (create "abab") ~in_:"acabababab" ~with_:"A" = "acAA"
    let%test _ = replace_all (create "ababab") ~in_:"acabababab" ~with_:"A" = "acAab"

    let%test _ =
      replace_all (create "abaC") ~in_:"abaCabaDCababaCabaCaba" ~with_:"x"
      = "xabaDCabxxaba"
    ;;

    let%test _ = replace_all (create "a") ~in_:"aa" ~with_:"aaa" = "aaaaaa"

    let%test _ =
      replace_all (create "") ~in_:"abcdeefff" ~with_:"X1"
      = "X1aX1bX1cX1dX1eX1eX1fX1fX1fX1"
    ;;

    (* a doc comment in core_string.mli gives this as an example *)
    let%test _ = replace_all (create "bc") ~in_:"aabbcc" ~with_:"cb" = "aabcbc"

    let%test _ =
      [%compare.equal: string list]
        (split_on (create "====") "aa====bbb====c=====d======e========fff")
        [ "aa"; "bbb"; "c"; "=d"; "==e"; ""; "fff" ]
    ;;

    let%test _ =
      [%compare.equal: string list]
        (split_on (create "XYXYX") "XYXYXaaXYXYXYXbbXYXYXYXYXYX")
        [ ""; "aa"; "YXbb"; "Y"; "" ]
    ;;

    let%test _ =
      [%compare.equal: string list]
        (split_on (create "") "abcd")
        (* [index_all (create "")] includes the occurrences at index 0 and at the end of
           the string, and the result of [split_on (create "")] is a consequence of this
        *)
        [ ""; "a"; "b"; "c"; "d"; "" ]
    ;;

    let%test _ =
      [%compare.equal: string list]
        (split_on (create "not present") "here is a string with no matches")
        [ "here is a string with no matches" ]
    ;;
  end)
;;

let%test _ = rev "" = ""
let%test _ = rev "a" = "a"
let%test _ = rev "ab" = "ba"
let%test _ = rev "abc" = "cba"

let%test_unit _ =
  List.iter
    ~f:(fun (t, expect) ->
      let actual = split_lines t in
      if not ([%compare.equal: string list] actual expect)
      then
        raise_s [%message "split_lines bug" (t : t) (actual : t list) (expect : t list)])
    [ "", []
    ; "\n", [ "" ]
    ; "a", [ "a" ]
    ; "a\n", [ "a" ]
    ; "a\nb", [ "a"; "b" ]
    ; "a\nb\n", [ "a"; "b" ]
    ; "a\n\n", [ "a"; "" ]
    ; "a\n\nb", [ "a"; ""; "b" ]
    ]
;;

let%test_unit _ =
  let lines = [ ""; "a"; "bc" ] in
  let newlines = [ "\n"; "\r\n" ] in
  let rec loop n expect to_concat =
    if Int.( = ) n 0
    then (
      let input = concat to_concat in
      let actual = Or_error.try_with (fun () -> split_lines input) in
      if not ([%compare.equal: t list Or_error.t] actual (Ok expect))
      then
        raise_s
          [%message
            "split_lines bug" (input : t) (actual : t list Or_error.t) (expect : t list)])
    else (
      loop (n - 1) expect to_concat;
      List.iter lines ~f:(fun t ->
        let loop to_concat = loop (n - 1) (t :: expect) (t :: to_concat) in
        if (not (is_empty t)) && List.is_empty to_concat then loop [];
        List.iter newlines ~f:(fun newline -> loop (newline :: to_concat))))
  in
  loop 3 [] []
;;

let%test_unit _ =
  let s = init 10 ~f:Char.of_int_exn in
  assert (phys_equal s (sub s ~pos:0 ~len:(String.length s)));
  assert (phys_equal s (prefix s (String.length s)));
  assert (phys_equal s (suffix s (String.length s)));
  assert (phys_equal s (concat [ s ]));
  assert (phys_equal s (tr s ~target:'\255' ~replacement:'\000'))
;;

let%test_module "tr_multi" =
  (module struct
    let gold_standard ~target ~replacement string =
      map string ~f:(fun char ->
        match rindex target char with
        | None -> char
        | Some i -> get replacement (Int.min i (length replacement - 1)))
    ;;

    module Test = struct
      type nonrec t =
        { target : t
        ; replacement : t
        ; string : t
        ; expected : t option [@sexp.option]
        }
      [@@deriving sexp_of]

      let quickcheck_generator =
        let open Base_quickcheck.Generator in
        let open Base_quickcheck.Generator.Let_syntax in
        let%bind size = size in
        let%bind target_len = int_log_uniform_inclusive 1 255 in
        let%bind target = string_with_length ~length:target_len in
        let%bind replacement_len = int_inclusive 1 target_len in
        let%bind replacement = string_with_length ~length:replacement_len in
        let%bind string_length = int_inclusive 0 size in
        let%map string = string_with_length ~length:string_length in
        { target; replacement; string; expected = None }
      ;;

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end

    let examples =
      [ "", "", "abcdefg", "abcdefg"
      ; "", "a", "abcdefg", "abcdefg"
      ; "aaaa", "abcd", "abcdefg", "dbcdefg"
      ; "abcd", "bcde", "abcdefg", "bcdeefg"
      ; "abcd", "bcde", "", ""
      ; "abcd", "_", "abcdefg", "____efg"
      ; "abcd", "b_", "abcdefg", "b___efg"
      ; "a", "dcba", "abcdefg", "dbcdefg"
      ; "ab", "dcba", "abcdefg", "dccdefg"
      ]
      |> List.map ~f:(fun (target, replacement, string, expected) ->
           { Test.target; replacement; string; expected = Some expected })
    ;;

    let%test_unit _ =
      Base_quickcheck.Test.run_exn
        (module Test)
        ~examples
        ~f:(fun ({ target; replacement; string; expected } : Test.t) ->
          (* test implementation behavior against gold standard *)
          let impl_result = unstage (tr_multi ~target ~replacement) string in
          let gold_result = gold_standard ~target ~replacement string in
          [%test_result: t] ~expect:gold_result impl_result;
          (* test against expected result, if one is provided (non-random examples) *)
          Option.iter expected ~f:(fun expected ->
            [%test_result: t] ~expect:expected impl_result);
          (* test for returning input if the string is unchanged *)
          if equal string impl_result then assert (phys_equal string impl_result))
    ;;
  end)
;;

let%test_unit _ = [%test_result: int option] (index "bob" 'b') ~expect:(Some 0)
let%test_unit _ = [%test_result: int option] (rindex "bob" 'b') ~expect:(Some 2)
let%test_unit _ = [%test_result: int option] (index "bob" 'c') ~expect:None
let%test_unit _ = [%test_result: int option] (rindex "bob" 'c') ~expect:None
let%test_unit _ = [%test_result: int option] (index_from "bobob" 1 'b') ~expect:(Some 2)
let%test_unit _ = [%test_result: int option] (rindex_from "bobob" 3 'b') ~expect:(Some 2)

let%test_unit _ =
  [%test_result: int option] (lfindi "bob" ~f:(fun _ -> Char.( = ) 'b')) ~expect:(Some 0)
;;

let%test_unit _ =
  [%test_result: int option]
    (lfindi ~pos:0 "bob" ~f:(fun _ -> Char.( = ) 'b'))
    ~expect:(Some 0)
;;

let%test_unit _ =
  [%test_result: int option]
    (lfindi ~pos:1 "bob" ~f:(fun _ -> Char.( = ) 'b'))
    ~expect:(Some 2)
;;

let%test_unit _ =
  [%test_result: int option] (lfindi "bob" ~f:(fun _ -> Char.( = ) 'x')) ~expect:None
;;

let%test_unit _ =
  [%test_result: char option]
    (find_map "fop" ~f:(fun c -> if Char.(c >= 'o') then Some c else None))
    ~expect:(Some 'o')
;;

let%test_unit _ =
  [%test_result: _ option] (find_map "bar" ~f:(fun _ -> None)) ~expect:None
;;

let%test_unit _ =
  [%test_result: _ option] (find_map "" ~f:(fun _ -> assert false)) ~expect:None
;;

let%test_unit _ =
  [%test_result: int option] (rfindi "bob" ~f:(fun _ -> Char.( = ) 'b')) ~expect:(Some 2)
;;

let%test_unit _ =
  [%test_result: int option]
    (rfindi ~pos:2 "bob" ~f:(fun _ -> Char.( = ) 'b'))
    ~expect:(Some 2)
;;

let%test_unit _ =
  [%test_result: int option]
    (rfindi ~pos:1 "bob" ~f:(fun _ -> Char.( = ) 'b'))
    ~expect:(Some 0)
;;

let%test_unit _ =
  [%test_result: int option] (rfindi "bob" ~f:(fun _ -> Char.( = ) 'x')) ~expect:None
;;

let%test_module "strip" =
  (module struct
    let test ?drop s = print_s [%sexp (strip ?drop s : string)]

    let%expect_test "whitespace on both ends" =
      test " foo bar \n";
      [%expect {| "foo bar" |}]
    ;;

    let%expect_test "custom drop, present on end" =
      test ~drop:(Char.( = ) '"') "\" foo bar ";
      [%expect {| " foo bar " |}]
    ;;

    let%expect_test "custom drop, absent from end" =
      test ~drop:(Char.( = ) '"') " \" foo bar ";
      [%expect {| " \" foo bar " |}]
    ;;

    let%expect_test "all whitespace" =
      test "\n\t \n";
      [%expect {| "" |}]
    ;;

    let%expect_test "no whitespace on ends" =
      test "as \t\ndf";
      [%expect {| "as \t\ndf" |}]
    ;;

    let%expect_test "just one side" =
      test " a";
      [%expect {| a |}];
      test "a ";
      [%expect {| a |}]
    ;;
  end)
;;

let%test_module "lstrip" =
  (module struct
    let test ?drop s = print_s [%sexp (lstrip ?drop s : string)]

    let%expect_test "whitespace on left" =
      test " \t\r\n123  \t\n";
      [%expect {| "123  \t\n" |}]
    ;;

    let%expect_test "all whitespace" =
      test " \t \n\n\r ";
      [%expect {| "" |}]
    ;;

    let%expect_test "no whitespace on left" =
      test "foo Bar \n ";
      [%expect {| "foo Bar \n " |}]
    ;;
  end)
;;

let%test_module "rstrip" =
  (module struct
    let test ?drop s = print_s [%sexp (rstrip ?drop s : string)]

    let%expect_test "whitespace on right" =
      test " \t\r\n123  \t\n\r";
      [%expect {| " \t\r\n123" |}]
    ;;

    let%expect_test "all whitespace" =
      test " \t \n\n\r ";
      [%expect {| "" |}]
    ;;

    let%expect_test "no whitespace on right" =
      test " \n foo Bar";
      [%expect {| " \n foo Bar" |}]
    ;;
  end)
;;

let%test_module "map" =
  (module struct
    let%expect_test "empty" = require_equal [%here] (module String) (map "" ~f:Fn.id) ""

    let%expect_test "non-empty" =
      let s = "faboo" in
      require_equal
        [%here]
        (module String)
        (map s ~f:(function
          | 'a' -> 'b'
          | 'b' -> 'a'
          | x -> x))
        "fbaoo"
    ;;
  end)
;;

let%expect_test "split" =
  let test s = print_s [%sexp (split s ~on:'c' : string list)] in
  test "";
  [%expect {| ("") |}];
  test "c";
  [%expect {| ("" "") |}];
  test "fooc";
  [%expect {| (foo "") |}];
  test "cfoo";
  [%expect {| ("" foo) |}];
  test "cfooc";
  [%expect {| ("" foo "") |}];
  test "bocci ball";
  [%expect {| (bo "" "i ball") |}]
;;

let%expect_test "split_on_chars" =
  let test s ~on = print_s [%sexp (split_on_chars s ~on : string list)] in
  test "" ~on:[ 'c' ];
  [%expect {| ("") |}];
  test "c" ~on:[ 'c' ];
  [%expect {| ("" "") |}];
  test "chr" ~on:[ 'h'; 'c'; 'r' ];
  [%expect {| ("" "" "" "") |}];
  test "fooc" ~on:[ 'c' ];
  [%expect {| (foo "") |}];
  test "fooc" ~on:[ 'c'; 'o' ];
  [%expect {| (f "" "" "") |}];
  test "cfoo" ~on:[ 'c' ];
  [%expect {| ("" foo) |}];
  test "cfoo" ~on:[ 'c'; 'f' ];
  [%expect {| ("" "" oo) |}];
  test "bocci ball" ~on:[ 'c' ];
  [%expect {| (bo "" "i ball") |}];
  test "bocci ball" ~on:[ 'c'; ' '; 'i' ];
  [%expect {| (bo "" "" "" ball) |}]
;;

let%test_unit _ =
  [%test_result: bool] ~expect:false (exists "" ~f:(fun _ -> assert false))
;;

let%test_unit _ = [%test_result: bool] ~expect:false (exists "abc" ~f:(Fn.const false))
let%test_unit _ = [%test_result: bool] ~expect:true (exists "abc" ~f:(Fn.const true))

let%test_unit _ =
  [%test_result: bool]
    ~expect:true
    (exists "abc" ~f:(function
      | 'a' -> false
      | 'b' -> true
      | _ -> assert false))
;;

let%test_unit _ =
  [%test_result: bool] ~expect:true (for_all "" ~f:(fun _ -> assert false))
;;

let%test_unit _ = [%test_result: bool] ~expect:true (for_all "abc" ~f:(Fn.const true))
let%test_unit _ = [%test_result: bool] ~expect:false (for_all "abc" ~f:(Fn.const false))

let%test_unit _ =
  [%test_result: bool]
    ~expect:false
    (for_all "abc" ~f:(function
      | 'a' -> true
      | 'b' -> false
      | _ -> assert false))
;;

let%test_unit _ =
  [%test_result: char list]
    (fold "hello" ~init:[] ~f:(fun acc ch -> ch :: acc))
    ~expect:(List.rev [ 'h'; 'e'; 'l'; 'l'; 'o' ]);
  [%test_result: (int * char) list]
    (foldi "hello" ~init:[] ~f:(fun i acc ch -> (i, ch) :: acc))
    ~expect:(List.rev [ 0, 'h'; 1, 'e'; 2, 'l'; 3, 'l'; 4, 'o' ])
;;

let%expect_test "iteri" =
  iteri "hello" ~f:(fun i ch -> printf "%d%c " i ch);
  [%expect {| 0h 1e 2l 3l 4o |}]
;;

let%test_unit _ = [%test_result: t] (filter "hello" ~f:(Char.( <> ) 'h')) ~expect:"ello"
let%test_unit _ = [%test_result: t] (filter "hello" ~f:(Char.( <> ) 'l')) ~expect:"heo"
let%test_unit _ = [%test_result: t] (filter "hello" ~f:(fun _ -> false)) ~expect:""
let%test_unit _ = [%test_result: t] (filter "hello" ~f:(fun _ -> true)) ~expect:"hello"

let%test_unit _ =
  [%test_result: t] (filteri "hello" ~f:(fun i _ -> Int.(i % 2 = 0))) ~expect:"hlo"
;;

let%test_unit _ =
  let s = "hello" in
  [%test_result: bool] ~expect:true (phys_equal (filter s ~f:(fun _ -> true)) s)
;;

let%test_unit _ =
  let s = "abc" in
  let r = ref 0 in
  assert (
    phys_equal
      s
      (filter s ~f:(fun _ ->
         Int.incr r;
         true)));
  assert (Int.( = ) !r (String.length s))
;;

let%test_module "Hash" =
  (module struct
    external hash : string -> int = "Base_hash_string" [@@noalloc]

    let%test_unit _ =
      List.iter
        ~f:(fun string ->
          assert (Int.( = ) (hash string) (Stdlib.Hashtbl.hash string));
          (* with 31-bit integers, the hash computed by ppx_hash overflows so it doesn't match
             polymorphic hash exactly. *)
          if Int.( > ) Int.num_bits 31
          then assert (Int.( = ) (hash string) ([%hash: string] string)))
        [ "Oh Gloria inmarcesible! Oh jubilo inmortal!"
        ; "Oh say can you see, by the dawn's early light"
        ; "Hahahaha\200"
        ]
    ;;
  end)
;;

let%test _ = of_char_list [ 'a'; 'b'; 'c' ] = "abc"
let%test _ = of_char_list [] = ""

let%expect_test "is_substring_at" =
  let string = "lorem ipsum dolor sit amet" in
  let test pos substring =
    match is_substring_at string ~pos ~substring with
    | bool -> print_s [%sexp (bool : bool)]
    | exception exn -> print_s [%message "raised" ~_:(exn : exn)]
  in
  test 0 "lorem";
  [%expect {| true |}];
  test 1 "lorem";
  [%expect {| false |}];
  test 6 "ipsum";
  [%expect {| true |}];
  test 5 "ipsum";
  [%expect {| false |}];
  test 22 "amet";
  [%expect {| true |}];
  test 23 "amet";
  [%expect {| false |}];
  test 22 "amet and some other stuff";
  [%expect {| false |}];
  test 0 "";
  [%expect {| true |}];
  test 10 "";
  [%expect {| true |}];
  test 26 "";
  [%expect {| true |}];
  test 100 "";
  [%expect
    {|
    (raised (
      Invalid_argument
      "String.is_substring_at: invalid index 100 for string of length 26"))
    |}];
  test (-1) "";
  [%expect
    {|
    (raised (
      Invalid_argument
      "String.is_substring_at: invalid index -1 for string of length 26"))
    |}]
;;

let%expect_test "prefixes and suffixes" =
  let s = "0123456789" in
  require_equal [%here] (module String) (String.prefix s 0) "";
  require_equal [%here] (module String) (String.prefix s 1) "0";
  require_equal [%here] (module String) (String.prefix s 2) "01";
  require_equal [%here] (module String) (String.prefix s 10) s;
  require_equal [%here] (module String) (String.prefix s 20) s;
  require_does_raise [%here] (fun () -> String.prefix s (-1));
  [%expect {| (Invalid_argument "prefix expecting nonnegative argument") |}];
  require_equal [%here] (module String) (String.suffix s 0) "";
  require_equal [%here] (module String) (String.suffix s 1) "9";
  require_equal [%here] (module String) (String.suffix s 2) "89";
  require_equal [%here] (module String) (String.suffix s 10) s;
  require_equal [%here] (module String) (String.suffix s 20) s;
  require_does_raise [%here] (fun () -> String.suffix s (-1));
  [%expect {| (Invalid_argument "suffix expecting nonnegative argument") |}]
;;

let%expect_test "drop prefixes and suffixes" =
  let s = "0123456789" in
  require_equal [%here] (module String) (String.drop_prefix s 0) s;
  require_equal [%here] (module String) (String.drop_prefix s 1) "123456789";
  require_equal [%here] (module String) (String.drop_prefix s 2) "23456789";
  require_equal [%here] (module String) (String.drop_prefix s 10) "";
  require_equal [%here] (module String) (String.drop_prefix s 20) "";
  require_does_raise [%here] (fun () -> String.drop_prefix s (-1));
  [%expect {| (Invalid_argument "drop_prefix expecting nonnegative argument") |}];
  require_equal [%here] (module String) (String.drop_suffix s 0) s;
  require_equal [%here] (module String) (String.drop_suffix s 1) "012345678";
  require_equal [%here] (module String) (String.drop_suffix s 2) "01234567";
  require_equal [%here] (module String) (String.drop_suffix s 10) "";
  require_equal [%here] (module String) (String.drop_suffix s 20) "";
  require_does_raise [%here] (fun () -> String.drop_suffix s (-1));
  [%expect {| (Invalid_argument "drop_suffix expecting nonnegative argument") |}]
;;

let%expect_test "testing prefixes and suffixes" =
  let test outer inner =
    print_s
      [%message
        ""
          ~is_prefix:(is_prefix outer ~prefix:inner : bool)
          ~is_suffix:(is_suffix outer ~suffix:inner : bool)]
  in
  test "" "a";
  [%expect {|
    ((is_prefix false)
     (is_suffix false))
    |}];
  test "" "";
  [%expect {|
    ((is_prefix true)
     (is_suffix true))
    |}];
  test "Foo" "";
  [%expect {|
    ((is_prefix true)
     (is_suffix true))
    |}];
  test "H" "H";
  [%expect {|
    ((is_prefix true)
     (is_suffix true))
    |}];
  test "Hello" "He";
  [%expect {|
    ((is_prefix true)
     (is_suffix false))
    |}];
  test "Hello" "lo";
  [%expect {|
    ((is_prefix false)
     (is_suffix true))
    |}];
  test "HelloFoo" "lo";
  [%expect {|
    ((is_prefix false)
     (is_suffix false))
    |}]
;;

let%expect_test "chop_prefix" =
  let s = "__x__" in
  let test_prefix ~prefix =
    let result = Or_error.try_with (fun () -> chop_prefix_exn s ~prefix) in
    require_equal
      [%here]
      (module struct
        type t = string option [@@deriving equal, sexp_of]
      end)
      (chop_prefix s ~prefix)
      (Or_error.ok result);
    require_equal
      [%here]
      (module String)
      (chop_prefix_if_exists s ~prefix)
      (result |> Or_error.ok |> Option.value ~default:s);
    print_s [%sexp (result : string Or_error.t)]
  in
  test_prefix ~prefix:"";
  [%expect {| (Ok __x__) |}];
  test_prefix ~prefix:"__";
  [%expect {| (Ok x__) |}];
  test_prefix ~prefix:"==";
  [%expect {| (Error (Invalid_argument "String.chop_prefix_exn \"__x__\" \"==\"")) |}]
;;

let%expect_test "chop_suffix" =
  let s = "__x__" in
  let test_suffix ~suffix =
    let result = Or_error.try_with (fun () -> chop_suffix_exn s ~suffix) in
    require_equal
      [%here]
      (module struct
        type t = string option [@@deriving equal, sexp_of]
      end)
      (chop_suffix s ~suffix)
      (Or_error.ok result);
    require_equal
      [%here]
      (module String)
      (chop_suffix_if_exists s ~suffix)
      (result |> Or_error.ok |> Option.value ~default:s);
    print_s [%sexp (result : string Or_error.t)]
  in
  test_suffix ~suffix:"";
  [%expect {| (Ok __x__) |}];
  test_suffix ~suffix:"__";
  [%expect {| (Ok __x) |}];
  test_suffix ~suffix:"==";
  [%expect {| (Error (Invalid_argument "String.chop_suffix_exn \"__x__\" \"==\"")) |}]
;;

let%expect_test "String.concat_lines" =
  let concat_lines_reference lines ~crlf =
    String.concat (List.map lines ~f:(fun line -> line ^ if crlf then "\r\n" else "\n"))
  in
  quickcheck_m
    [%here]
    (module struct
      type t = string list * bool [@@deriving quickcheck, sexp_of]
    end)
    ~f:(fun (lines, crlf) ->
      require_equal
        [%here]
        (module String)
        (String.concat_lines lines ~crlf)
        (concat_lines_reference lines ~crlf));
  [%expect {| |}]
;;

let%expect_test "String.concat_lines examples" =
  let test ?crlf lines = print_s [%sexp (String.concat_lines ?crlf lines : string)] in
  test [ "foo"; "bar" ];
  [%expect {| "foo\nbar\n" |}];
  test [ "foo\n"; "b\nar\n"; "x" ];
  [%expect {| "foo\n\nb\nar\n\nx\n" |}];
  test [];
  [%expect {| "" |}];
  test ~crlf:true [ "foo"; "bar" ];
  [%expect {| "foo\r\nbar\r\n" |}]
;;

let%expect_test "pad_left and pad_right" =
  let test ?char t ~len =
    let padded_left = pad_left ?char t ~len in
    require [%here] (Int.( >= ) (length padded_left) len);
    require [%here] (String.is_suffix padded_left ~suffix:t);
    let padded_right = pad_right ?char t ~len in
    require [%here] (Int.( >= ) (length padded_right) len);
    require [%here] (String.is_prefix padded_right ~prefix:t);
    if Int.( >= ) (length t) len
    then (
      require [%here] (phys_equal t padded_left);
      require [%here] (phys_equal t padded_right));
    print_s [%message (t : string) (padded_left : string) (padded_right : string)]
  in
  test "" ~len:2;
  [%expect
    {|
    ((t            "")
     (padded_left  "  ")
     (padded_right "  "))
    |}];
  test "foo" ~len:2;
  [%expect
    {|
    ((t            foo)
     (padded_left  foo)
     (padded_right foo))
    |}];
  test "foo" ~len:3;
  [%expect
    {|
    ((t            foo)
     (padded_left  foo)
     (padded_right foo))
    |}];
  test "foo" ~len:10 ~char:'_';
  [%expect
    {|
    ((t            foo)
     (padded_left  _______foo)
     (padded_right foo_______))
    |}]
;;

let%test_module "functions that raise Not_found_s" =
  (module struct
    let show f sexp_of_ok = print_s [%sexp (Result.try_with f : (ok, exn) Result.t)]

    let%expect_test "index_exn" =
      let test s = show (fun () -> index_exn s ':') [%sexp_of: int] in
      test "";
      [%expect {| (Error (Not_found_s "String.index_exn: not found")) |}];
      test "abc";
      [%expect {| (Error (Not_found_s "String.index_exn: not found")) |}];
      test ":abc";
      [%expect {| (Ok 0) |}];
      test "abc:";
      [%expect {| (Ok 3) |}];
      test "ab:cd:ef";
      [%expect {| (Ok 2) |}]
    ;;

    let%expect_test "index_from_exn" =
      let test_at s i = show (fun () -> index_from_exn s i ':') [%sexp_of: int] in
      let test s =
        for i = 0 to length s do
          test_at s i
        done
      in
      test "";
      [%expect {| (Error (Not_found_s "String.index_from_exn: not found")) |}];
      test "abc";
      [%expect
        {|
        (Error (Not_found_s "String.index_from_exn: not found"))
        (Error (Not_found_s "String.index_from_exn: not found"))
        (Error (Not_found_s "String.index_from_exn: not found"))
        (Error (Not_found_s "String.index_from_exn: not found"))
        |}];
      test "a:b:c";
      [%expect
        {|
        (Ok 1)
        (Ok 1)
        (Ok 3)
        (Ok 3)
        (Error (Not_found_s "String.index_from_exn: not found"))
        (Error (Not_found_s "String.index_from_exn: not found"))
        |}];
      let test_bounds s =
        test_at s (-1);
        test_at s (length s + 1)
      in
      test_bounds "abc";
      [%expect
        {|
        (Error (Invalid_argument String.index_from_exn))
        (Error (Invalid_argument String.index_from_exn))
        |}]
    ;;

    let%expect_test "rindex_exn" =
      let test s = show (fun () -> rindex_exn s ':') [%sexp_of: int] in
      test "";
      [%expect {| (Error (Not_found_s "String.rindex_exn: not found")) |}];
      test "abc";
      [%expect {| (Error (Not_found_s "String.rindex_exn: not found")) |}];
      test ":abc";
      [%expect {| (Ok 0) |}];
      test "abc:";
      [%expect {| (Ok 3) |}];
      test "ab:cd:ef";
      [%expect {| (Ok 5) |}]
    ;;

    let%expect_test "rindex_from_exn" =
      let test_at s i = show (fun () -> rindex_from_exn s i ':') [%sexp_of: int] in
      let test s =
        for i = length s - 1 downto -1 do
          test_at s i
        done
      in
      test "";
      [%expect {| (Error (Not_found_s "String.rindex_from_exn: not found")) |}];
      test "abc";
      [%expect
        {|
        (Error (Not_found_s "String.rindex_from_exn: not found"))
        (Error (Not_found_s "String.rindex_from_exn: not found"))
        (Error (Not_found_s "String.rindex_from_exn: not found"))
        (Error (Not_found_s "String.rindex_from_exn: not found"))
        |}];
      test "a:b:c";
      [%expect
        {|
        (Ok 3)
        (Ok 3)
        (Ok 1)
        (Ok 1)
        (Error (Not_found_s "String.rindex_from_exn: not found"))
        (Error (Not_found_s "String.rindex_from_exn: not found"))
        |}];
      let test_bounds s =
        test_at s (-2);
        test_at s (length s)
      in
      test_bounds "abc";
      [%expect
        {|
        (Error (Invalid_argument String.rindex_from_exn))
        (Error (Invalid_argument String.rindex_from_exn))
        |}]
    ;;

    let%expect_test "lsplit2_exn" =
      let test s =
        let option_result = lsplit2 s ~on:':' in
        let exn_result = Or_error.try_with (fun () -> lsplit2_exn s ~on:':') in
        require_equal
          [%here]
          (module struct
            type t = (string * string) option [@@deriving equal, sexp_of]
          end)
          option_result
          (Or_error.ok exn_result);
        print_s [%sexp (exn_result : (string * string) Or_error.t)]
      in
      test "";
      [%expect {| (Error (Not_found_s "String.lsplit2_exn: not found")) |}];
      test "abc";
      [%expect {| (Error (Not_found_s "String.lsplit2_exn: not found")) |}];
      test ":abc";
      [%expect {| (Ok ("" abc)) |}];
      test "abc:";
      [%expect {| (Ok (abc "")) |}];
      test "ab:cd:ef";
      [%expect {| (Ok (ab cd:ef)) |}]
    ;;

    let%expect_test "rsplit2_exn" =
      let test s =
        let option_result = rsplit2 s ~on:':' in
        let exn_result = Or_error.try_with (fun () -> rsplit2_exn s ~on:':') in
        require_equal
          [%here]
          (module struct
            type t = (string * string) option [@@deriving equal, sexp_of]
          end)
          option_result
          (Or_error.ok exn_result);
        print_s [%sexp (exn_result : (string * string) Or_error.t)]
      in
      test "";
      [%expect {| (Error (Not_found_s "String.rsplit2_exn: not found")) |}];
      test "abc";
      [%expect {| (Error (Not_found_s "String.rsplit2_exn: not found")) |}];
      test ":abc";
      [%expect {| (Ok ("" abc)) |}];
      test "abc:";
      [%expect {| (Ok (abc "")) |}];
      test "ab:cd:ef";
      [%expect {| (Ok (ab:cd ef)) |}]
    ;;
  end)
;;

let%test_module "Escaping" =
  (module struct
    open Escaping

    let%test_module "escape_gen" =
      (module struct
        let escape =
          unstage
            (escape_gen_exn ~escapeworthy_map:[ '%', 'p'; '^', 'c' ] ~escape_char:'_')
        ;;

        let%test _ = escape "" = ""
        let%test _ = escape "foo" = "foo"
        let%test _ = escape "_" = "__"
        let%test _ = escape "foo%bar" = "foo_pbar"
        let%test _ = escape "^foo%" = "_cfoo_p"

        let escape2 =
          unstage
            (escape_gen_exn
               ~escapeworthy_map:[ '_', '.'; '%', 'p'; '^', 'c' ]
               ~escape_char:'_')
        ;;

        let%test _ = escape2 "_." = "_.."
        let%test _ = escape2 "_" = "_."
        let%test _ = escape2 "foo%_bar" = "foo_p_.bar"
        let%test _ = escape2 "_foo%" = "_.foo_p"

        let checks_for_one_to_one escapeworthy_map =
          Exn.does_raise (fun () -> escape_gen_exn ~escapeworthy_map ~escape_char:'_')
        ;;

        let%test _ = checks_for_one_to_one [ '%', 'p'; '^', 'c'; '$', 'c' ]
        let%test _ = checks_for_one_to_one [ '%', 'p'; '^', 'c'; '%', 'd' ]
      end)
    ;;

    let%test_module "unescape_gen" =
      (module struct
        let unescape =
          unstage
            (unescape_gen_exn ~escapeworthy_map:[ '%', 'p'; '^', 'c' ] ~escape_char:'_')
        ;;

        let%test _ = unescape "__" = "_"
        let%test _ = unescape "foo" = "foo"
        let%test _ = unescape "__" = "_"
        let%test _ = unescape "foo_pbar" = "foo%bar"
        let%test _ = unescape "_cfoo_p" = "^foo%"

        let unescape2 =
          unstage
            (unescape_gen_exn
               ~escapeworthy_map:[ '_', '.'; '%', 'p'; '^', 'c' ]
               ~escape_char:'_')
        ;;

        (* this one is ill-formed, just ignore the escape_char without escaped char *)
        let%test _ = unescape2 "_" = ""
        let%test _ = unescape2 "a_" = "a"
        let%test _ = unescape2 "__" = "_"
        let%test _ = unescape2 "_.." = "_."
        let%test _ = unescape2 "_." = "_"
        let%test _ = unescape2 "foo_p_.bar" = "foo%_bar"
        let%test _ = unescape2 "_.foo_p" = "_foo%"

        (* generate [n] random string and check if escaping and unescaping are consistent *)
        let random_test ~escapeworthy_map ~escape_char n =
          let escape = unstage (escape_gen_exn ~escapeworthy_map ~escape_char) in
          let unescape = unstage (unescape_gen_exn ~escapeworthy_map ~escape_char) in
          let test str =
            let escaped = escape str in
            let unescaped = unescape escaped in
            if str <> unescaped
            then
              failwith
                (Printf.sprintf
                   "string: %s\nescaped string: %s\nunescaped string: %s"
                   str
                   escaped
                   unescaped)
          in
          let random_char =
            let print_chars =
              List.range (Char.to_int Char.min_value) (Char.to_int Char.max_value + 1)
              |> List.filter_map ~f:Char.of_int
              |> List.filter ~f:Char.is_print
              |> Array.of_list
            in
            fun () -> Array.random_element_exn print_chars
          in
          let escapeworthy_chars = List.map escapeworthy_map ~f:fst |> Array.of_list in
          try
            for _ = 0 to n - 1 do
              let str =
                List.init (Random.int 50) ~f:(fun _ ->
                  let p = Random.int 100 in
                  if Int.(p < 10)
                  then escape_char
                  else if Int.(p < 25)
                  then Array.random_element_exn escapeworthy_chars
                  else random_char ())
                |> of_char_list
              in
              test str
            done;
            true
          with
          | e -> raise e
        ;;

        let%test _ =
          random_test 1000 ~escapeworthy_map:[ '%', 'p'; '^', 'c' ] ~escape_char:'_'
        ;;

        let%test _ =
          random_test
            1000
            ~escapeworthy_map:[ '_', '.'; '%', 'p'; '^', 'c' ]
            ~escape_char:'_'
        ;;
      end)
    ;;

    let%test_module "escape" =
      (module struct
        let escape = unstage (escape ~escape_char:'_' ~escapeworthy:[ '_'; '%'; '^' ])
        let%test _ = escape "foo" = "foo"
        let%test _ = escape "_" = "__"
        let%test _ = escape "foo%bar" = "foo_%bar"
        let%test _ = escape "^foo%" = "_^foo_%"
      end)
    ;;

    let%test_module "unescape" =
      (module struct
        let unescape = unstage (unescape ~escape_char:'_')
        let%test _ = unescape "foo" = "foo"
        let%test _ = unescape "__" = "_"
        let%test _ = unescape "foo_%bar" = "foo%bar"
        let%test _ = unescape "_^foo_%" = "^foo%"
      end)
    ;;

    let%test_module "is_char_escaping" =
      (module struct
        let is = is_char_escaping ~escape_char:'_'
        let%test_unit _ = [%test_result: bool] (is "___" 0) ~expect:true
        let%test_unit _ = [%test_result: bool] (is "___" 1) ~expect:false
        let%test_unit _ = [%test_result: bool] (is "___" 2) ~expect:true

        (* considered escaping, though there's nothing to escape *)
        let%test_unit _ = [%test_result: bool] (is "a_b__c" 0) ~expect:false
        let%test_unit _ = [%test_result: bool] (is "a_b__c" 1) ~expect:true
        let%test_unit _ = [%test_result: bool] (is "a_b__c" 2) ~expect:false
        let%test_unit _ = [%test_result: bool] (is "a_b__c" 3) ~expect:true
        let%test_unit _ = [%test_result: bool] (is "a_b__c" 4) ~expect:false
        let%test_unit _ = [%test_result: bool] (is "a_b__c" 5) ~expect:false
      end)
    ;;

    let%test_module "is_char_escaped" =
      (module struct
        let is = is_char_escaped ~escape_char:'_'
        let%test_unit _ = [%test_result: bool] (is "___" 2) ~expect:false
        let%test_unit _ = [%test_result: bool] (is "x" 0) ~expect:false
        let%test_unit _ = [%test_result: bool] (is "_x" 1) ~expect:true
        let%test_unit _ = [%test_result: bool] (is "sadflkas____sfff" 12) ~expect:false
        let%test_unit _ = [%test_result: bool] (is "s_____s" 6) ~expect:true
      end)
    ;;

    let%test_module "is_char_literal" =
      (module struct
        let is_char_literal = is_char_literal ~escape_char:'_'
        let%test_unit _ = [%test_result: bool] (is_char_literal "123456" 4) ~expect:true
        let%test_unit _ = [%test_result: bool] (is_char_literal "12345_6" 6) ~expect:false
        let%test_unit _ = [%test_result: bool] (is_char_literal "12345_6" 5) ~expect:false

        let%test_unit _ =
          [%test_result: bool] (is_char_literal "123__456" 4) ~expect:false
        ;;

        let%test_unit _ =
          [%test_result: bool] (is_char_literal "123456__" 7) ~expect:false
        ;;

        let%test_unit _ =
          [%test_result: bool] (is_char_literal "__123456" 1) ~expect:false
        ;;

        let%test_unit _ =
          [%test_result: bool] (is_char_literal "__123456" 0) ~expect:false
        ;;

        let%test_unit _ = [%test_result: bool] (is_char_literal "__123456" 2) ~expect:true
      end)
    ;;

    let%test_module "index_from" =
      (module struct
        let f = index_from ~escape_char:'_'
        let%test_unit _ = [%test_result: int option] (f "__" 0 '_') ~expect:None
        let%test_unit _ = [%test_result: int option] (f "_.." 0 '.') ~expect:(Some 2)

        let%test_unit _ =
          [%test_result: int option] (f "1273456_7789" 3 '7') ~expect:(Some 9)
        ;;

        let%test_unit _ =
          [%test_result: int option] (f "1273_7456_7789" 3 '7') ~expect:(Some 11)
        ;;

        let%test_unit _ =
          [%test_result: int option] (f "1273_7456_7789" 3 'z') ~expect:None
        ;;
      end)
    ;;

    let%test_module "rindex" =
      (module struct
        let f = rindex_from ~escape_char:'_'
        let%test_unit _ = [%test_result: int option] (f "__" 0 '_') ~expect:None

        let%test_unit _ =
          [%test_result: int option] (f "123456_37839" 9 '3') ~expect:(Some 2)
        ;;

        let%test_unit _ = [%test_result: int option] (f "123_2321" 6 '2') ~expect:(Some 6)
        let%test_unit _ = [%test_result: int option] (f "123_2321" 5 '2') ~expect:(Some 1)

        let%test_unit _ =
          [%test_result: int option] (rindex "" ~escape_char:'_' 'x') ~expect:None
        ;;

        let%test_unit _ =
          [%test_result: int option] (rindex "a_a" ~escape_char:'_' 'a') ~expect:(Some 0)
        ;;
      end)
    ;;

    let%test_module "split" =
      (module struct
        let split = split ~escape_char:'_' ~on:','

        let%test_unit _ =
          [%test_result: string list]
            (split "foo,bar,baz")
            ~expect:[ "foo"; "bar"; "baz" ]
        ;;

        let%test_unit _ =
          [%test_result: string list] (split "foo_,bar,baz") ~expect:[ "foo_,bar"; "baz" ]
        ;;

        let%test_unit _ =
          [%test_result: string list] (split "foo_,bar_,baz") ~expect:[ "foo_,bar_,baz" ]
        ;;

        let%test_unit _ =
          [%test_result: string list]
            (split "foo__,bar,baz")
            ~expect:[ "foo__"; "bar"; "baz" ]
        ;;

        let%test_unit _ =
          [%test_result: string list]
            (split "foo,bar,baz_,")
            ~expect:[ "foo"; "bar"; "baz_," ]
        ;;

        let%test_unit _ =
          [%test_result: string list]
            (split "foo,bar_,baz_,,")
            ~expect:[ "foo"; "bar_,baz_,"; "" ]
        ;;
      end)
    ;;

    let%test_module "split_on_chars" =
      (module struct
        let split = split_on_chars ~escape_char:'_' ~on:[ ','; ':' ]

        let%test_unit _ =
          [%test_result: string list]
            (split "foo,bar:baz")
            ~expect:[ "foo"; "bar"; "baz" ]
        ;;

        let%test_unit _ =
          [%test_result: string list] (split "foo_,bar,baz") ~expect:[ "foo_,bar"; "baz" ]
        ;;

        let%test_unit _ =
          [%test_result: string list] (split "foo_:bar_,baz") ~expect:[ "foo_:bar_,baz" ]
        ;;

        let%test_unit _ =
          [%test_result: string list]
            (split "foo,bar,baz_,")
            ~expect:[ "foo"; "bar"; "baz_," ]
        ;;

        let%test_unit _ =
          [%test_result: string list]
            (split "foo:bar_,baz_,,")
            ~expect:[ "foo"; "bar_,baz_,"; "" ]
        ;;
      end)
    ;;

    let%test_module "split2" =
      (module struct
        let escape_char = '_'
        let on = ','

        let%test_unit _ =
          [%test_result: (string * string) option]
            (lsplit2 ~escape_char ~on "foo_,bar,baz_,0")
            ~expect:(Some ("foo_,bar", "baz_,0"))
        ;;

        let%test_unit _ =
          [%test_result: (string * string) option]
            (rsplit2 ~escape_char ~on "foo_,bar,baz_,0")
            ~expect:(Some ("foo_,bar", "baz_,0"))
        ;;

        let%test_unit _ =
          [%test_result: string * string]
            (lsplit2_exn ~escape_char ~on "foo_,bar,baz_,0")
            ~expect:("foo_,bar", "baz_,0")
        ;;

        let%test_unit _ =
          [%test_result: string * string]
            (rsplit2_exn ~escape_char ~on "foo_,bar,baz_,0")
            ~expect:("foo_,bar", "baz_,0")
        ;;

        let%test_unit _ =
          [%test_result: (string * string) option]
            (lsplit2 ~escape_char ~on "foo_,bar")
            ~expect:None
        ;;

        let%test_unit _ =
          [%test_result: (string * string) option]
            (rsplit2 ~escape_char ~on "foo_,bar")
            ~expect:None
        ;;

        let%test _ = Exn.does_raise (fun () -> lsplit2_exn ~escape_char ~on "foo_,bar")
        let%test _ = Exn.does_raise (fun () -> rsplit2_exn ~escape_char ~on "foo_,bar")
      end)
    ;;

    let%test _ = strip_literal ~escape_char:' ' " foo bar \n" = " foo bar \n"
    let%test _ = strip_literal ~escape_char:' ' " foo bar \n\n" = " foo bar \n"
    let%test _ = strip_literal ~escape_char:'\n' " foo bar \n" = "foo bar \n"
    let%test _ = lstrip_literal ~escape_char:' ' " foo bar \n\n" = " foo bar \n\n"
    let%test _ = rstrip_literal ~escape_char:' ' " foo bar \n\n" = " foo bar \n"
    let%test _ = lstrip_literal ~escape_char:'\n' " foo bar \n" = "foo bar \n"
    let%test _ = rstrip_literal ~escape_char:'\n' " foo bar \n" = " foo bar \n"
    let%test _ = strip_literal ~drop:Char.is_alpha ~escape_char:'\\' "foo boar" = " "
    let%test _ = strip_literal ~drop:Char.is_alpha ~escape_char:'\\' "fooboar" = ""
    let%test _ = strip_literal ~drop:Char.is_alpha ~escape_char:'o' "foo boar" = "oo boa"
    let%test _ = strip_literal ~drop:Char.is_alpha ~escape_char:'a' "foo boar" = " boar"
    let%test _ = strip_literal ~drop:Char.is_alpha ~escape_char:'b' "foo boar" = " bo"

    let%test _ =
      lstrip_literal ~drop:Char.is_alpha ~escape_char:'o' "foo boar" = "oo boar"
    ;;

    let%test _ =
      rstrip_literal ~drop:Char.is_alpha ~escape_char:'o' "foo boar" = "foo boa"
    ;;

    let%test _ = lstrip_literal ~drop:Char.is_alpha ~escape_char:'b' "foo boar" = " boar"
    let%test _ = rstrip_literal ~drop:Char.is_alpha ~escape_char:'b' "foo boar" = "foo bo"
  end)
;;
