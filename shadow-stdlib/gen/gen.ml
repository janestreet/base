open StdLabels

let () =
  try
    Clflags.native_code := true;
    let dir = Filename.dirname Sys.argv.(1)
    and module_ =
      Filename.chop_extension
        (String.capitalize (Filename.basename Sys.argv.(1))) in
    Clflags.include_dirs := dir :: !Clflags.include_dirs;
    Opttoploop.set_paths ();
    Warnings.parse_options false "-58";
    let buf = Buffer.create 512 in
    let pp = Format.formatter_of_buffer buf in
    Format.pp_set_margin pp max_int; (* so we can parse line by line below *)
    let res =
      Opttoploop.execute_phrase true pp
        (!Opttoploop.parse_toplevel_phrase
           (Lexing.from_string ("include " ^ module_ ^ ";;")))
    in
    assert res;
    let s = Buffer.contents buf in
    let lines = Str.split (Str.regexp "\n") s in
    Buffer.clear buf;
    Buffer.add_string buf "[@@@warning \"-3\"]";
    List.iter lines ~f:(fun line ->
      Printf.bprintf buf "%s\n" (Mapper.line (Lexing.from_string line)));
    let s = Buffer.contents buf in
    Parse.interface (Lexing.from_string s)
    |> Format.printf "%a@." Pprintast.signature
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 2
