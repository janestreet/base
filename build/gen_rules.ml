#load "str.cma";;
#load "unix.cma";;
#use "config.ml";;

open StdLabels
open MoreLabels
open Printf

let bootstrap_dir = "bootstrap/src"
let caml_dir = "compiler-stdlib/src"

let comp_flags = "-w -40 -g -bin-annot"
let boot_comp_flags = sprintf "-I %s -I %s %s" bootstrap_dir caml_dir comp_flags

let pr fmt = printf (fmt ^^ "\n")
let ( ^/ ) = Filename.concat

let stdlib_dir =
  let ic = ksprintf Unix.open_process_in "%s -where" ocamlc in
  let dir = input_line ic in
  ignore (Unix.close_process_in ic);
  dir

let run fmt =
  ksprintf (fun cmd ->
      let n = Sys.command cmd in
      if n <> 0 then
        exit n)
           fmt

let split_words = Str.split (Str.regexp "[ \t]+")

type kind = Intf | Impl

type entry =
  { base : string
  ; kind : kind
  ; deps : string list
  }

let read_deps files =
  let ic =
    ksprintf Unix.open_process_in "%s -I %s -modules %s"
             ocamldep bootstrap_dir (String.concat ~sep:" " files)
  in
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> acc
    | line ->
       Scanf.sscanf line "%[^:]: %[\000-\255]" (fun fn mods ->
                      let deps = split_words mods in
                      let entry =
                        if Filename.check_suffix fn ".mli" then
                          { base = Filename.chop_extension fn
                          ; kind = Intf
                          ; deps
                          }
                        else
                          { base = Filename.chop_extension fn
                          ; kind = Impl
                          ; deps
                          }
                      in
                      loop (entry :: acc))
  in
  let deps = loop [] in
  ignore (Unix.close_process_in ic);
  deps

module String_set = Set.Make(String)

let topsort deps =
  let deps =
    List.filter deps ~f:(fun e -> e.kind = Impl)
    |> List.map ~f:(fun e ->
        (String.capitalize (Filename.basename e.base), e.deps))
  in
  let n = List.length deps in
  let deps_by_module = Hashtbl.create n in
  List.iter deps ~f:(fun (m, deps) ->
      Hashtbl.add deps_by_module m deps);
  let not_seen = ref (List.map deps ~f:fst |> String_set.of_list) in
  let res = ref [] in
  let rec loop m =
    if String_set.mem m !not_seen then begin
      not_seen := String_set.remove m !not_seen;
      List.iter (Hashtbl.find deps_by_module m) ~f:loop;
      res := m :: !res
    end
  in
  while not (String_set.is_empty !not_seen) do
    loop (String_set.choose !not_seen)
  done;
  List.rev !res

type dep_kind = Cmx | Cmi

let rec find_dir_containing path fn =
  match path with
  | [] ->
     None
  | dir :: rest ->
     if Sys.file_exists (dir ^/ fn) then
       Some path
     else
       find_dir_containing rest fn

let () =
  let files =
    Sys.readdir bootstrap_dir
    |> Array.to_list
  in

  let files, c_files =
    ((List.filter files ~f:(fun fn ->
         Filename.check_suffix fn ".ml" || Filename.check_suffix fn ".mli")
     |> List.map ~f:((^/) bootstrap_dir)
     |> String_set.of_list),
     List.filter files ~f:(fun fn -> Filename.check_suffix fn ".c" && fn <> "select-int63-backend.c")
     |> List.map ~f:((^/) bootstrap_dir))
  in

  pr "BASE0_O_FILES := %s" (String.concat ~sep:" "
    (List.map c_files ~f:(fun fn -> Filename.chop_extension fn ^ ".o")));
  List.iter c_files ~f:(fun fn ->
      pr "%s.o: %s" (Filename.chop_extension fn) fn;
      pr "\tcd %s && %s -g -c %s"
        (Filename.dirname fn) ocamlc (Filename.basename fn));

  let deps = read_deps (String_set.elements files) in
  let module_deps kind mods =
    List.map mods ~f:(fun m ->
               let base    = bootstrap_dir ^/ String.uncapitalize m in
               let base_cm = bootstrap_dir ^/ "base0__" ^ m         in
               match kind with
               | Cmi ->
                  if String_set.mem (base ^ ".mli") files then
                    [base_cm ^ ".cmi"]
                  else if String_set.mem (base ^ ".ml") files then
                    [base_cm ^ ".cmo"]
                  else if m = "Caml" then
                    [caml_dir ^/ "caml.cmo"]
                  else
                    let fn = stdlib_dir ^/ String.uncapitalize m ^ ".cmi" in
                    if Sys.file_exists fn then
                      [fn]
                    else []
               | Cmx ->
                  if String_set.mem (base ^ ".mli") files then
                    [sprintf "%s.cmi %s.cmx" base_cm base_cm]
                  else if String_set.mem (base ^ ".ml") files then
                    [base_cm ^ ".cmx"]
                  else if m = "Caml" then
                    [caml_dir ^/ "caml.cmx"]
                  else
                    let base = stdlib_dir ^/ String.uncapitalize m in
                    if Sys.file_exists (base ^ ".cmi") then
                      [sprintf "%s.cmi %s.cmx" base base]
                    else
                      [])
    |> List.concat
    |> String.concat ~sep:" "
  in
  let targets ext =
    [ bootstrap_dir ^/ "base0" ^ ext
    ; "compiler-stdlib/src/caml" ^ ext
    ; "shadow-stdlib/src/shadow_stdlib" ^ ext
    ; "wrapper/base" ^ ext
    ]
  in
  pr "TARGETS := %s"
    (String.concat ~sep:" "
       (targets ".cma" @ if has_ocamlopt then targets ".cmxa" else []));
  let sorted_mods = topsort deps in
  let all_cm ext =
    ((bootstrap_dir ^/ "base0" ^ ext) ::
     List.map sorted_mods ~f:(fun m ->
         bootstrap_dir ^/ "base0__" ^ m ^ ext))
    |> String.concat ~sep:" "
  in
  pr "BASE0_CMO_FILES := %s" (all_cm ".cmo");
  pr "BASE0_CMX_FILES := %s" (all_cm ".cmx");

  (* Generate the base0.ml file *)
  let mods =
    List.map deps ~f:(fun entry ->
               String.capitalize (Filename.basename entry.base))
    |> String_set.of_list
  in
  let buf = Buffer.create 1024 in
  String_set.iter mods ~f:(fun m ->
      bprintf buf "module %s = Base0__%s\n" m m);
  let contents = Buffer.contents buf in
  let base0_mlgen = "bootstrap/src/base0.ml-gen" in
  let current =
    match open_in base0_mlgen with
    | exception _ -> ""
    | ic -> let s = really_input_string ic (in_channel_length ic) in close_in ic; s
  in
  (* Don't override the file if it hasn't changed *)
  if contents <> current then begin
    let oc = open_out base0_mlgen in
    output_string oc contents;
    close_out oc;
  end;

  pr "";
  List.iter deps ~f:(fun entry ->
    pr "";
    let base =
      Filename.dirname entry.base ^/ "base0__" ^ String.capitalize (Filename.basename entry.base)
    in
    match entry.kind with
    | Intf ->
       pr "%s.cmi: %s.mli %s"
          base entry.base (module_deps Cmi entry.deps);
       pr "\t%s %s -c -open Base0 -o %s.cmi %s.mli" ocamlc boot_comp_flags base entry.base
    | Impl ->
       let self_cmi_dep_for_cmo, self_cmi_dep_for_cmx =
         if String_set.mem (entry.base ^ ".mli") files then
           let s = sprintf "%s.cmi " base in
           (s, s)
         else
           (* Depend on the .cmo to avoid having two rules that can generate the .cmi in parallel *)
           ("", sprintf "%s.cmo " base)
       in
       pr "%s.cmo: %s.ml %s/base0.cmo %s%s"
          base entry.base bootstrap_dir self_cmi_dep_for_cmo (module_deps Cmi entry.deps);
       pr "\t%s %s -c -open Base0 -o %s.cmo %s.ml" ocamlc boot_comp_flags base entry.base;
       pr "";
       pr "%s.cmx: %s.ml %s/base0.cmx %s%s"
          base entry.base bootstrap_dir self_cmi_dep_for_cmx (module_deps Cmx entry.deps);
       pr "\t%s %s -c -open Base0 -o %s.cmx %s.ml" ocamlopt boot_comp_flags base entry.base)
