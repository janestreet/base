open! Import

module C = Core_kernel

let%expect_test _ =
  C.Quickcheck.test (C.String.Set.gen C.String.gen)
    ~sizes:(Sequence.of_list [0; 1; 2; 3; 4; 5; 10; 100; 1000])
    ~trials:9
    ~f:(fun strings ->
      let assoc = List.mapi (Set.to_list strings) ~f:(fun i s -> (s, i)) in
      let map  = Map.of_alist_exn (module String) assoc in
      let dict = String_dict.of_alist_exn assoc         in
      let inputs =
        List.concat_map (Set.to_list strings) ~f:(fun str ->
          let len = String.length str in
          [ str
          ; str ^ "x"
          ; if len = 0 then
              "foo"
            else
              String.sub str ~pos:0 ~len:(len - 1)
          ])
      in
      List.iter inputs ~f:(fun str ->
        let from_map  = Map.find  map  str in
        let from_dict = String_dict.find dict str in
        require [%here] ([%compare.equal: int option] from_map from_dict)
          ~if_false_then_print_s:
            (lazy [%sexp { from_map  : int option
                         ; from_dict : int option
                         ; map       : int Map.M(String).t
                         }])))
;;


