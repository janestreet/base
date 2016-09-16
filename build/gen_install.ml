open StdLabels
open MoreLabels
open Printf

let ( ^/ ) = Filename.concat

let rec walk dir ~init ~f =
  Sys.readdir dir
  |> Array.fold_left ~init ~f:(fun acc fn ->
                       let fn = if dir = "." then fn else dir ^/ fn in
                       if Sys.is_directory fn then begin
                         walk fn ~init:acc ~f
                       end else
                         f acc fn)

let extension fn =
  match String.rindex fn '.' with
  | exception Not_found -> ""
  | i -> String.sub fn ~pos:(i+1) ~len:(String.length fn - i - 1)

module String_set = Set.Make(String)

let pr fmt = printf (fmt ^^ "\n")

let () =
  let dirs =
    String_set.of_list
      [ "bootstrap/src"
      ; "compiler-stdlib/src"
      ; "shadow-stdlib/src"
      ; "wrapper"
      ]
  in
  let files =
    walk "." ~init:String_set.empty ~f:(fun acc fn ->
        if String_set.mem (Filename.dirname fn) dirs then
          String_set.add fn acc
        else
          acc)
  in
  let lib, stublibs =
    String_set.fold files ~init:([], []) ~f:(fun fn (lib, stublibs) ->
        match extension fn with
        | "cmx" | "cmi" | "cma" | "cmxs" | "cmxa" | "cmt" | "cmti" | "a" | "mli" ->
          (fn :: lib, stublibs)
        | "so" ->
          (lib, fn :: stublibs)
        | "ml" when not (String_set.mem (fn ^ "i") files) ->
          (fn :: lib, stublibs)
        | _ ->
          (lib, stublibs))
  in
  pr "lib: [";
  pr "  \"META\"";
  List.iter lib ~f:(fun fn ->
      pr "  %S" fn);
  pr "]";
  pr "stublibs: [";
  List.iter stublibs ~f:(fun fn ->
      pr "  %S" fn);
  pr "]"
