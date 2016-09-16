#load "str.cma";;
#load "unix.cma";;

open StdLabels
open MoreLabels
open Printf

let bootstrap_dir = "bootstrap/src"
let caml_dir = "compiler-stdlib/src"

let pr fmt = printf (fmt ^^ "\n")
let ( ^/ ) = Filename.concat
let quote = Filename.quote

let path =
  match Sys.getenv "PATH" with
  | exception Not_found -> []
  | s ->
     let sep =
       if Sys.win32 then
         ";"
       else
         ":"
     in
     Str.split_delim (Str.regexp sep) s
;;

let find_prog_location prog =
  let rec search = function
    | [] ->
       eprintf "Program %s not found in PATH" prog;
       exit 2
    | dir :: rest ->
       let fn = dir ^/ prog in
       if Sys.file_exists fn then
         dir
       else
         search rest
  in
  search path

let ocaml_bin_dir = find_prog_location "ocamlc"
let ocaml_prog =
  let suffix =
    if Sys.file_exists (ocaml_bin_dir ^/ "ocamlc.opt") then
      ".opt"
    else
      ""
  in
  function
  | "ocaml" -> ocaml_bin_dir ^/ "ocaml"
  | "ocamlmklib" -> ocaml_bin_dir ^/ "ocamlmklib"
  | tool -> ocaml_bin_dir ^/ tool ^ suffix

let progs = ["ocaml"; "ocamlc"; "ocamlopt"; "ocamldep"; "ocamllex"; "ocamlmklib"]

let rec walk dir ~init ~f =
  Sys.readdir dir
  |> Array.fold_left ~init ~f:(fun acc fn ->
                       let fn = if dir = "." then fn else dir ^/ fn in
                       if Sys.is_directory fn then
                         walk fn ~init:acc ~f
                       else
                         f acc fn)

let () =
  List.iter progs ~f:(fun prog ->
              pr "%-8s := %s"
                 (String.uppercase prog) (ocaml_prog prog));

  let generated_files =
    "bootstrap/src/base_int63_backend.ml" ::
    walk "." ~init:[] ~f:(fun acc fn ->
        if Filename.check_suffix fn ".mll" then
          (Filename.chop_extension fn ^ ".ml") :: acc
        else
          acc)
  in

  pr "";
  pr "GENERATED_FILES := %s" (String.concat ~sep:" " generated_files);

  let oc = open_out "build/config.ml" in
  fprintf oc "let has_ocamlopt = %B\n\n" (Sys.file_exists (ocaml_prog "ocamlopt"));
  List.iter progs ~f:(fun prog ->
              fprintf oc "let %-8s = %S\n" prog (ocaml_prog prog));
  close_out oc
