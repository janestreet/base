open! Import
open! Int

let%test_module "Hex" =
  (module struct

    let f (i,s_hum) =
      let s = String.filter s_hum ~f:(fun c -> not (Char.equal c '_')) in
      let sexp_hum = Sexp.Atom s_hum in
      let sexp = Sexp.Atom s in
      [%test_result: Sexp.t] ~message:"sexp_of_t" ~expect:sexp (Hex.sexp_of_t i);
      [%test_result: int] ~message:"t_of_sexp" ~expect:i (Hex.t_of_sexp sexp);
      [%test_result: int] ~message:"t_of_sexp[human]" ~expect:i (Hex.t_of_sexp sexp_hum);
      [%test_result: string] ~message:"to_string" ~expect:s (Hex.to_string i);
      [%test_result: string] ~message:"to_string_hum" ~expect:s_hum (Hex.to_string_hum i);
      [%test_result: int] ~message:"of_string" ~expect:i (Hex.of_string s);
      [%test_result: int] ~message:"of_string[human]" ~expect:i (Hex.of_string s_hum);
    ;;

    let%test_unit _ =
      List.iter ~f
        [ 0, "0x0"
        ; 1, "0x1"
        ; 2, "0x2"
        ; 5, "0x5"
        ; 10, "0xa"
        ; 16, "0x10"
        ; 254, "0xfe"
        ; 65_535, "0xffff"
        ; 65_536, "0x1_0000"
        ; 1_000_000, "0xf_4240"
        ; -1, "-0x1"
        ; -2, "-0x2"
        ; -1_000_000, "-0xf_4240"
        ; max_value,
          (match num_bits with
           | 31 -> "0x3fff_ffff"
           | 32 -> "0x7fff_ffff"
           | 63 -> "0x3fff_ffff_ffff_ffff"
           | _  -> assert false)
        ; min_value,
          (match num_bits with
           | 31 -> "-0x4000_0000"
           | 32 -> "-0x8000_0000"
           | 63 -> "-0x4000_0000_0000_0000"
           | _  -> assert false)
        ]

    let%test_unit _ =
      [%test_result: int] (Hex.of_string "0XA") ~expect:10

    let%test_unit _ =
      match Option.try_with (fun () -> Hex.of_string "0") with
      | None -> ()
      | Some _ -> failwith "Hex must always have a 0x prefix."

    let%test_unit _ =
      match Option.try_with (fun () -> Hex.of_string "0x_0") with
      | None -> ()
      | Some _ -> failwith "Hex may not have '_' before the first digit."

  end)
