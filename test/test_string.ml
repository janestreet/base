open! Import
open! String

let%expect_test "hash coherence" [@tags "64-bits-only"] =
  check_hash_coherence [%here] (module String) [ ""; "a"; "foo" ];
  [%expect {| |}];
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

let%test_module "Caseless Comparable" =
  (module struct
    (* examples from docs *)
    let%test _ = Caseless.equal "OCaml" "ocaml"
    let%test _ = Caseless.("apple" < "Banana")

    let%test _ = Caseless.("aa" < "aaa")
    let%test _ = Int.(<>) (Caseless.compare "apple" "Banana") (compare "apple" "Banana")
    let%test _ = Caseless.equal "XxX" "xXx"
    let%test _ = Caseless.("XxX" < "xXxX")
    let%test _ = Caseless.("XxXx" > "xXx")

    let%test _ = List.is_sorted ~compare:Caseless.compare ["Apples"; "bananas"; "Carrots"]

    let%expect_test _ =
      let x = Sys.opaque_identity "one string" in
      let y = Sys.opaque_identity "another"    in
      require_no_allocation [%here] (fun () ->
        ignore (Sys.opaque_identity (Caseless.equal x y) : bool));
      [%expect {||}];
    ;;
  end)

let%test_module "Caseless Hashable" =
  (module struct
    let%test _ =
      Int.(<>) (hash "x") (hash "X")
      && Int.(=) (Caseless.hash "x") (Caseless.hash "X")
    let%test _ = Int.(=) (Caseless.hash "OCaml") (Caseless.hash "ocaml")
    let%test _ = Int.(<>) (Caseless.hash "aaa") (Caseless.hash "aaaa")
    let%test _ = Int.(<>) (Caseless.hash "aaa") (Caseless.hash "aab")
    let%test _ =
      let tbl = Hashtbl.create (module Caseless) in
      Hashtbl.add_exn tbl ~key:"x" ~data:7;
      [%compare.equal: int option] (Hashtbl.find tbl "X") (Some 7)
  end)

let%test _ = not (contains ""     'a')
let%test _ =      contains "a"    'a'
let%test _ = not (contains "a"    'b')
let%test _ =      contains "ab"   'a'
let%test _ =      contains "ab"   'b'
let%test _ = not (contains "ab"   'c')
let%test _ = not (contains "abcd" 'b' ~pos:1 ~len:0)
let%test _ =      contains "abcd" 'b' ~pos:1 ~len:1
let%test _ =      contains "abcd" 'c' ~pos:1 ~len:2
let%test _ = not (contains "abcd" 'd' ~pos:1 ~len:2)
let%test _ =      contains "abcd" 'd' ~pos:1
let%test _ = not (contains "abcd" 'a' ~pos:1)

let%test_module "Search_pattern" =
  (module struct
    open Search_pattern

    let%test_module "Search_pattern.create" =
      (module struct
        let prefix s n = sub s ~pos:0 ~len:n
        let suffix s n = sub s ~pos:(length s - n) ~len:n

        let slow_create pattern =
          (* Compute the longest prefix-suffix array from definition, O(n^3) *)
          let n = length pattern in
          let kmp_arr = Array.create ~len:n (-1) in
          for i = 0 to n - 1 do
            let x = prefix pattern (i + 1) in
            for j = 0 to i do
              if String.equal (prefix x j) (suffix x j) then
                kmp_arr.(i) <- j
            done
          done;
          (pattern, kmp_arr)
        ;;

        let sexp_of_int = Base.Not_exposed_properly.Sexp_conv.sexp_of_int

        let test_both (s, a) =
          let create_s      = create s      |> [%sexp_of: t                 ] in
          let slow_create_s = slow_create s |> [%sexp_of: string * int array] in
          let expected = [%sexp ((s, a) : string * int array)] in
          require [%here] (Sexp.equal create_s expected && Sexp.equal slow_create_s expected)
            ~if_false_then_print_s:(lazy (
              [%message "not equal"
                          (create_s      : Sexp.t)
                          (slow_create_s : Sexp.t)
                          (expected      : Sexp.t)]))
        ;;

        let cmp_both s =
          let create_s      = create s      |> [%sexp_of: t                 ] in
          let slow_create_s = slow_create s |> [%sexp_of: string * int array] in
          require [%here] (Sexp.equal create_s slow_create_s)
            ~if_false_then_print_s:(lazy (
              [%message "not equal"
                          (create_s      : Sexp.t)
                          (slow_create_s : Sexp.t)]))
        ;;

        let%expect_test _ =
          test_both ("", [| |])
        let%expect_test _ =
          test_both ("ababab", [|0; 0; 1; 2; 3; 4|])
        let%expect_test _ =
          test_both ("abaCabaD", [|0; 0; 1; 0; 1; 2; 3; 0|])
        let%expect_test _ =
          test_both ("abaCabaDabaCabaCabaDabaCabaEabab",
                     [|0; 0; 1; 0; 1; 2; 3; 0; 1; 2; 3; 4; 5; 6; 7; 4; 5; 6; 7; 8;
                       9; 10; 11; 12; 13; 14; 15; 0; 1; 2; 3; 2|])

        let rec x k =
          if Int.(<) k 0 then "" else
            let b = x (k - 1) in
            b ^ (make 1 (Caml.Char.unsafe_chr (65 + k))) ^ b
        ;;

        let%expect_test _ =
          cmp_both (x 10)
        let%expect_test _ =
          cmp_both ((x 5) ^ "E" ^ (x 4) ^ "D" ^ (x 3) ^ "B" ^ (x 2) ^ "C" ^ (x 3))
      end)

    let (=) = [%compare.equal: int option]
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

    let (=) = [%compare.equal: int list]
    let%test _ = index_all (create "") ~may_overlap:false ~in_:"abcd" = [0; 1; 2; 3; 4]
    let%test _ = index_all (create "") ~may_overlap:true ~in_:"abcd" = [0; 1; 2; 3; 4]
    let%test _ = index_all (create "abab") ~may_overlap:false ~in_:"abababab" = [0; 4]
    let%test _ = index_all (create "abab") ~may_overlap:true  ~in_:"abababab" = [0; 2; 4]
    let%test _ = index_all (create "abab") ~may_overlap:false ~in_:"ababababab" = [0; 4]
    let%test _ = index_all (create "abab") ~may_overlap:true  ~in_:"ababababab" = [0; 2; 4; 6]
    let%test _ = index_all (create "aaa") ~may_overlap:false ~in_:"aaaaBaaaaaa" = [0; 5; 8]
    let%test _ = index_all (create "aaa") ~may_overlap:true  ~in_:"aaaaBaaaaaa" = [0; 1; 5; 6; 7; 8]

    let (=) = [%compare.equal: string]
    let%test _ = replace_first (create "abab") ~in_:"abababab" ~with_:"" = "abab"
    let%test _ = replace_first (create "abab") ~in_:"abacabab" ~with_:"" = "abac"
    let%test _ = replace_first (create "abab") ~in_:"ababacab" ~with_:"A" = "Aacab"
    let%test _ = replace_first (create "abab") ~in_:"acabababab" ~with_:"A" = "acAabab"
    let%test _ = replace_first (create "ababab") ~in_:"acabababab" ~with_:"A" = "acAab"
    let%test _ = replace_first (create "abab") ~in_:"abababab" ~with_:"abababab" = "abababababab"

    let%test _ = replace_all (create "abab") ~in_:"abababab" ~with_:"" = ""
    let%test _ = replace_all (create "abab") ~in_:"abacabab" ~with_:"" = "abac"
    let%test _ = replace_all (create "abab") ~in_:"acabababab" ~with_:"A" = "acAA"
    let%test _ = replace_all (create "ababab") ~in_:"acabababab" ~with_:"A" = "acAab"
    let%test _ = replace_all (create "abaC") ~in_:"abaCabaDCababaCabaCaba" ~with_:"x" = "xabaDCabxxaba"
    let%test _ = replace_all (create "a") ~in_:"aa" ~with_:"aaa" = "aaaaaa"
    let%test _ = replace_all (create "") ~in_:"abcdeefff" ~with_:"X1" = "X1aX1bX1cX1dX1eX1eX1fX1fX1fX1"

    (* a doc comment in core_string.mli gives this as an example *)
    let%test _ = replace_all (create "bc") ~in_:"aabbcc" ~with_:"cb" = "aabcbc"
  end)

let%test _ = rev "" = "";;
let%test _ = rev "a" = "a";;
let%test _ = rev "ab" = "ba";;
let%test _ = rev "abc" = "cba";;

let%test_unit _ =
  List.iter ~f:(fun (t, expect) ->
    let actual = split_lines t in
    if not ([%compare.equal: string list] actual expect)
    then raise_s [%message "split_lines bug"
                             (t : t) (actual : t list) (expect : t list)])
    [ ""             , [];
      "\n"           , [""];
      "a"            , ["a"];
      "a\n"          , ["a"];
      "a\nb"         , ["a"; "b"];
      "a\nb\n"       , ["a"; "b"];
      "a\n\n"        , ["a"; "" ];
      "a\n\nb"       , ["a"; "" ; "b"];
    ]
;;

let%test_unit _ =
  let lines = [ ""; "a"; "bc" ] in
  let newlines = [ "\n"; "\r\n" ] in
  let rec loop n expect to_concat =
    if Int.(=) n 0 then begin
      let input = concat to_concat in
      let actual = Or_error.try_with (fun () -> split_lines input) in
      if not ([%compare.equal: t list Or_error.t] actual (Ok expect))
      then raise_s [%message "split_lines bug"
                               (input : t)
                               (actual : t list Or_error.t)
                               (expect : t list)]
    end else begin
      loop (n - 1) expect to_concat;
      List.iter lines ~f:(fun t ->
        let loop to_concat = loop (n - 1) (t :: expect) (t :: to_concat) in
        if not (is_empty t) && List.is_empty to_concat then loop [];
        List.iter newlines ~f:(fun newline -> loop (newline :: to_concat)));
    end
  in
  loop 3 [] []
;;

open Polymorphic_compare

let%test _ = lfindi "bob" ~f:(fun _ c -> 'b' = c) = Some 0
let%test _ = lfindi ~pos:0 "bob" ~f:(fun _ c -> 'b' = c) = Some 0
let%test _ = lfindi ~pos:1 "bob" ~f:(fun _ c -> 'b' = c) = Some 2
let%test _ = lfindi "bob" ~f:(fun _ c -> 'x' = c) = None

let%test _ = find_map "fop" ~f:(fun c -> if c >= 'o' then Some c else None) = Some 'o'
let%test _ = find_map "bar" ~f:(fun _ -> None) = None
let%test _ = find_map "" ~f:(fun _ -> assert false) = None

let%test _ = rfindi "bob" ~f:(fun _ c -> 'b' = c) = Some 2
let%test _ = rfindi ~pos:2 "bob" ~f:(fun _ c -> 'b' = c) = Some 2
let%test _ = rfindi ~pos:1 "bob" ~f:(fun _ c -> 'b' = c) = Some 0
let%test _ = rfindi "bob" ~f:(fun _ c -> 'x' = c) = None

let%test _ = strip " foo bar \n" = "foo bar"
let%test _ = strip ~drop:(fun c -> c = '"') "\" foo bar " = " foo bar "
let%test _ = strip ~drop:(fun c -> c = '"') " \" foo bar " = " \" foo bar "

let%test _ = false = exists ""    ~f:(fun _ -> assert false)
let%test _ = false = exists "abc" ~f:(Fn.const false)
let%test _ = true  = exists "abc" ~f:(Fn.const true)
let%test _ = true  = exists "abc" ~f:(function
  'a' -> false | 'b' -> true | _ -> assert false)

let%test _ = true  = for_all ""    ~f:(fun _ -> assert false)
let%test _ = true  = for_all "abc" ~f:(Fn.const true)
let%test _ = false = for_all "abc" ~f:(Fn.const false)
let%test _ = false = for_all "abc" ~f:(function
  'a' -> true | 'b' -> false | _ -> assert false)

let%test _ = (foldi "hello" ~init:[] ~f:(fun i acc ch -> (i,ch)::acc)
              = List.rev [0,'h';1,'e';2,'l';3,'l';4,'o'])

let%test _ = filter "hello" ~f:(fun c -> c <> 'h') = "ello"
let%test _ = filter "hello" ~f:(fun c -> c <> 'l') = "heo"
let%test _ = filter "hello" ~f:(fun _ -> false) = ""
let%test _ = filter "hello" ~f:(fun _ -> true) = "hello"
let%test _ = let s = "hello" in phys_equal (filter s ~f:(fun _ -> true)) s
let%test_unit _ =
  let s = "abc" in
  let r = ref 0 in
  assert (phys_equal s (filter s ~f:(fun _ -> Int.incr r; true)));
  assert (!r = String.length s)
;;

let%test_module "Hash" =
  (module struct
    external hash : string -> int = "Base_hash_string" [@@noalloc]

    let%test_unit _ =
      List.iter ~f:(fun string ->
        assert (hash string = Caml.Hashtbl.hash string);
        (* with 31-bit integers, the hash computed by ppx_hash overflows so it doesn't match
           polymorphic hash exactly. *)
        if Int.num_bits > 31 then
          assert (hash string = [%hash: string] string)
      )
        [ "Oh Gloria inmarcesible! Oh jubilo inmortal!"
        ; "Oh say can you see, by the dawn's early light"
        ; "Hahahaha\200"
        ]
    ;;
  end)

let%test _ = of_char_list ['a';'b';'c'] = "abc"
let%test _ = of_char_list [] = ""

let%expect_test "mem does not allocate" =
  let string = Sys.opaque_identity "abracadabra" in
  let char   = Sys.opaque_identity 'd'           in
  require_no_allocation [%here] (fun () ->
    ignore (String.mem string char : bool));
  [%expect {||}];
;;

let%expect_test "is_substring_at" =
  let string = "lorem ipsum dolor sit amet" in
  let test pos substring =
    match is_substring_at string ~pos ~substring with
    | bool          -> print_s [%sexp (bool : bool)]
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
  [%expect {|
    (raised (
      Invalid_argument
      "String.is_substring_at: invalid index 100 for string of length 26")) |}];
  test (-1) "";
  [%expect {|
    (raised (
      Invalid_argument
      "String.is_substring_at: invalid index -1 for string of length 26")) |}];
;;

let%test_module "Escaping" =
  (module struct
    open Escaping

    let%test_module "escape_gen" =
      (module struct
        let escape = unstage
                       (escape_gen_exn
                          ~escapeworthy_map:[('%','p');('^','c')] ~escape_char:'_')

        let%test _ = escape "" = ""
        let%test _ = escape "foo" = "foo"
        let%test _ = escape "_" = "__"
        let%test _ = escape "foo%bar" = "foo_pbar"
        let%test _ = escape "^foo%" = "_cfoo_p"

        let escape2 = unstage
                        (escape_gen_exn
                           ~escapeworthy_map:[('_','.');('%','p');('^','c')] ~escape_char:'_')

        let%test _ = escape2 "_." = "_.."
        let%test _ = escape2 "_" = "_."
        let%test _ = escape2 "foo%_bar" = "foo_p_.bar"
        let%test _ = escape2 "_foo%" = "_.foo_p"

        let checks_for_one_to_one escapeworthy_map =
          try
            let _escape = escape_gen_exn ~escapeworthy_map ~escape_char:'_' in
            false
          with _ -> true

        let%test _ = checks_for_one_to_one [('%','p');('^','c');('$','c')]
        let%test _ = checks_for_one_to_one [('%','p');('^','c');('%','d')]
      end)

    let%test_module "unescape_gen" =
      (module struct
        let unescape =
          unstage
            (unescape_gen_exn ~escapeworthy_map:['%','p';'^','c'] ~escape_char:'_')

        let%test _ = unescape "__" = "_"
        let%test _ = unescape "foo" = "foo"
        let%test _ = unescape "__" = "_"
        let%test _ = unescape "foo_pbar" = "foo%bar"
        let%test _ = unescape "_cfoo_p" = "^foo%"

        let unescape2 =
          unstage
            (unescape_gen_exn ~escapeworthy_map:['_','.';'%','p';'^','c'] ~escape_char:'_')

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
          let escape =
            unstage (escape_gen_exn ~escapeworthy_map ~escape_char)
          in
          let unescape =
            unstage (unescape_gen_exn ~escapeworthy_map ~escape_char)
          in
          let test str =
            let escaped = escape str in
            let unescaped = unescape escaped in
            if str <> unescaped then
              failwith (
                Printf.sprintf
                  "string: %s\nescaped string: %s\nunescaped string: %s"
                  str escaped unescaped)
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
          let escapeworthy_chars =
            List.map escapeworthy_map ~f:fst |> Array.of_list
          in
          try
            for _ = 0 to n - 1 do
              let str =
                List.init (Random.int 50) ~f:(fun _ ->
                  let p = Random.int 100 in
                  if p < 10 then
                    escape_char
                  else if p < 25 then
                    Array.random_element_exn escapeworthy_chars
                  else
                    random_char ()
                )
                |> of_char_list
              in
              test str
            done;
            true
          with e ->
            raise e

        let%test _ = random_test 1000 ~escapeworthy_map:['%','p';'^','c'] ~escape_char:'_'
        let%test _ = random_test 1000 ~escapeworthy_map:['_','.';'%','p';'^','c'] ~escape_char:'_'
      end)

    let%test_module "unescape" =
      (module struct
        let unescape = unstage (unescape ~escape_char:'_')
        let%test _ = unescape "foo" = "foo"
        let%test _ = unescape "__" = "_"
        let%test _ = unescape "foo_%bar" = "foo%bar"
        let%test _ = unescape "_^foo_%" = "^foo%"
      end)

    let%test_module "is_char_escaping" =
      (module struct
        let is = is_char_escaping ~escape_char:'_'
        let%test _ = is "___" 0 = true
        let%test _ = is "___" 1 = false
        let%test _ = is "___" 2 = true (* considered escaping, though there's nothing to escape *)

        let%test _ = is "a_b__c" 0 = false
        let%test _ = is "a_b__c" 1 = true
        let%test _ = is "a_b__c" 2 = false
        let%test _ = is "a_b__c" 3 = true
        let%test _ = is "a_b__c" 4 = false
        let%test _ = is "a_b__c" 5 = false
      end)

    let%test_module "is_char_escaped" =
      (module struct
        let is = is_char_escaped ~escape_char:'_'
        let%test _ = is "___" 2 = false
        let%test _ = is "x" 0 = false
        let%test _ = is "_x" 1 = true
        let%test _ = is "sadflkas____sfff" 12 = false
        let%test _ = is "s_____s" 6 = true
      end)

    let%test_module "is_char_literal" =
      (module struct
        let is_char_literal = is_char_literal ~escape_char:'_'
        let%test _ = is_char_literal "123456" 4 = true
        let%test _ = is_char_literal "12345_6" 6 = false
        let%test _ = is_char_literal "12345_6" 5 = false
        let%test _ = is_char_literal "123__456" 4 = false
        let%test _ = is_char_literal "123456__" 7 = false
        let%test _ = is_char_literal "__123456" 1 = false
        let%test _ = is_char_literal "__123456" 0 = false
        let%test _ = is_char_literal "__123456" 2 = true
      end)

    let%test_module "index_from" =
      (module struct
        let f = index_from ~escape_char:'_'
        let%test _ = f "__" 0 '_' = None
        let%test _ = f "_.." 0 '.' = Some 2
        let%test _ = f "1273456_7789" 3 '7' = Some 9
        let%test _ = f "1273_7456_7789" 3 '7' = Some 11
        let%test _ = f "1273_7456_7789" 3 'z' = None
      end)

    let%test_module "rindex" =
      (module struct
        let f = rindex_from ~escape_char:'_'
        let%test _ = f "__" 0 '_' = None
        let%test _ = f "123456_37839" 9 '3' = Some 2
        let%test _ = f "123_2321" 6 '2' = Some 6
        let%test _ = f "123_2321" 5 '2' = Some 1

        let%test _ = rindex "" ~escape_char:'_' 'x' = None
        let%test _ = rindex "a_a" ~escape_char:'_' 'a' = Some 0
      end)

    let%test_module "split" =
      (module struct
        let split = split ~escape_char:'_' ~on:','
        let%test _ = split "foo,bar,baz" = ["foo"; "bar"; "baz"]
        let%test _ = split "foo_,bar,baz" = ["foo_,bar"; "baz"]
        let%test _ = split "foo_,bar_,baz" = ["foo_,bar_,baz"]
        let%test _ = split "foo__,bar,baz" = ["foo__"; "bar"; "baz"]
        let%test _ = split "foo,bar,baz_," = ["foo"; "bar"; "baz_,"]
        let%test _ = split "foo,bar_,baz_,," = ["foo"; "bar_,baz_,"; ""]
      end)

    let%test_module "split_on_chars" =
      (module struct
        let split = split_on_chars ~escape_char:'_' ~on:[',';':']
        let%test _ = split "foo,bar:baz" = ["foo"; "bar"; "baz"]
        let%test _ = split "foo_,bar,baz" = ["foo_,bar"; "baz"]
        let%test _ = split "foo_:bar_,baz" = ["foo_:bar_,baz"]
        let%test _ = split "foo,bar,baz_," = ["foo"; "bar"; "baz_,"]
        let%test _ = split "foo:bar_,baz_,," = ["foo"; "bar_,baz_,"; ""]
      end)

    let%test_module "split2" =
      (module struct
        let escape_char = '_'
        let on = ','
        let%test _ = lsplit2 ~escape_char ~on "foo_,bar,baz_,0" = Some ("foo_,bar", "baz_,0")
        let%test _ = rsplit2 ~escape_char ~on "foo_,bar,baz_,0" = Some ("foo_,bar", "baz_,0")
        let%test _ = lsplit2_exn ~escape_char ~on "foo_,bar,baz_,0" = ("foo_,bar", "baz_,0")
        let%test _ = rsplit2_exn ~escape_char ~on "foo_,bar,baz_,0" = ("foo_,bar", "baz_,0")
        let%test _ = lsplit2 ~escape_char ~on "foo_,bar" = None
        let%test _ = rsplit2 ~escape_char ~on "foo_,bar" = None
        let%test _ = try lsplit2_exn ~escape_char ~on "foo_,bar" |> Fn.const false with _ -> true
        let%test _ = try rsplit2_exn ~escape_char ~on "foo_,bar" |> Fn.const false with _ -> true
      end)

    let%test _ = strip_literal ~escape_char:' ' " foo bar \n" = " foo bar \n"
    let%test _ = strip_literal ~escape_char:' ' " foo bar \n\n" = " foo bar \n"
    let%test _ = strip_literal ~escape_char:'\n' " foo bar \n" = "foo bar \n"

    let%test _ = lstrip_literal ~escape_char:' ' " foo bar \n\n" = " foo bar \n\n"
    let%test _ = rstrip_literal ~escape_char:' ' " foo bar \n\n" = " foo bar \n"
    let%test _ = lstrip_literal ~escape_char:'\n' " foo bar \n" = "foo bar \n"
    let%test _ = rstrip_literal ~escape_char:'\n' " foo bar \n" = " foo bar \n"

    let%test _ = strip_literal ~drop:(Char.is_alpha) ~escape_char:'\\' "foo boar" = " "
    let%test _ = strip_literal ~drop:(Char.is_alpha) ~escape_char:'\\' "fooboar" = ""
    let%test _ = strip_literal ~drop:(Char.is_alpha) ~escape_char:'o' "foo boar" = "oo boa"
    let%test _ = strip_literal ~drop:(Char.is_alpha) ~escape_char:'a' "foo boar" = " boar"
    let%test _ = strip_literal ~drop:(Char.is_alpha) ~escape_char:'b' "foo boar" = " bo"

    let%test _ = lstrip_literal ~drop:(Char.is_alpha) ~escape_char:'o' "foo boar" = "oo boar"
    let%test _ = rstrip_literal ~drop:(Char.is_alpha) ~escape_char:'o' "foo boar" = "foo boa"
    let%test _ = lstrip_literal ~drop:(Char.is_alpha) ~escape_char:'b' "foo boar" = " boar"
    let%test _ = rstrip_literal ~drop:(Char.is_alpha) ~escape_char:'b' "foo boar" = "foo bo"
  end)
