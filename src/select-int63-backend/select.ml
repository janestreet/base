let () =
  let portable_int63, arch_sixtyfour, output =
    try
      match Sys.argv with
      | [|_; "-portable-int63"; x; "-arch-sixtyfour"; y; "-o"; fn|] ->
        (bool_of_string x, bool_of_string y, fn)
      | _ -> raise Exit
    with _ ->
      failwith "bad command line arguments"
  in
  let backend =
    if portable_int63 then
      "Dynamic"
    else if arch_sixtyfour then
      "Native"
    else
      "Emulated"
  in
  let oc = open_out output in
  Printf.fprintf oc "include Int63_backends.%s" backend;
  close_out oc
