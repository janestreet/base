open Core.Std
open Shexp_process.Std

module P = Process
open P.Let_syntax
let context = P.Context.create ()
let eval proc = P.eval ~context proc

let fe_sexp = sprintf "\
(Build_projections base public-release)
(Scrutiny critical_path)
(Owner %s)
(Reviewed_by (All_of (Users)))
(Fewer_than_min_reviewers true)
(Apply_to All_files)
" Bootstrap_owner.owner

module Rewrite_jbuild = struct
  let assoc_field field_name (fields : Sexp.t list) =
    List.find_map fields ~f:(function
      | List [ Atom f; sexp ] when f = field_name -> Some sexp
      | _ -> None)

  let rewrite_library_field : Sexp.t -> Sexp.t = function
    | List [ Atom "name"; _ ] ->
      [%sexp `name "base0"]
    | List [ Atom "libraries"; List _ ] ->
      [%sexp `libraries ["caml"]]
    | List [ Atom "preprocess"; _ ] ->
      [%sexp `preprocess [`no_preprocessing `All]]
    | sexp -> sexp

  let rewrite : Sexp.t -> Sexp.t option = function
    | List [ Atom "rule"; List fields ]
      when assoc_field "targets" fields = Some [%sexp ["pow_overflow_bounds.ml"]] ->
      None
    | List [ Atom "library"; List fields ] ->
      let fields =
        List.map fields ~f:rewrite_library_field @
        [ [%sexp `public_name "base.0"]
        ; [%sexp `extra_disabled_warnings ["32"]]
        ]
      in
      Some [%sexp `library (fields : Sexp.t list)]
    | sexp -> Some sexp
end


type config =
  { ppx_exe : string
  ; src_dir : string
  ; dst_dir : string
  }

let process_file ~config fn =
  let src_fn = config.src_dir ^/ fn in
  let dst_fn = config.dst_dir ^/ fn in
  match fn with
  | ".fe.sexp" ->
    P.stdout_to dst_fn (P.print fe_sexp)
  | "jbuild" ->
    let s =
      Sexp.load_sexps src_fn
      |> List.filter_map ~f:Rewrite_jbuild.rewrite
      |> List.map ~f:(fun j -> Sexp.to_string_hum j ^ "\n")
      |> String.concat ~sep:"\n"
    in
    P.stdout_to dst_fn (P.print s)
  | _ ->
    match snd (Filename.split_extension fn) with
    | Some ("ml" | "mli") ->
      P.run config.ppx_exe
        [ "-dirname"; "lib/base/src"
        ; "-inside-base"
        ; "-inline-test-lib"; "base"
        ; "-inline-test-drop"
        ; "-bench-drop"
        ; src_fn
        ; "-loc-filename"; Filename.basename fn
        ; "-dump-ast"
        ; "-o"; dst_fn
        ]
    | _ ->
      P.run "cp" [src_fn; dst_fn]

let process ~check ~ppx_exe ~src_dir ~dst_dir =
  let%bind hg_root = P.run "hg" ["root"] |- P.read_all >>| String.strip in
  let%bind ppx_exe =
    let ppx_exe = Option.value ppx_exe ~default:(hg_root ^/ ".ppx/BASE/ppx.exe") in
    let ppx_backup_exe = Filename.chop_extension ppx_exe ^ ".backup.exe" in
    (* Create a backup of the ppx so that we can keep promoting if we screw up
       something *)
    match%bind P.file_exists ppx_exe with
    | true  -> let%map () = P.run "cp" [ppx_exe; ppx_backup_exe] in ppx_exe
    | false ->
      match%map P.file_exists ppx_backup_exe with
      | true  -> ppx_backup_exe
      | false ->
        raise_s
          [%message
            "ppx program is missing, you need to build it to be able to run this script"
              (ppx_exe : string)
          ]
  in
  let src_dir = Option.value src_dir ~default:(hg_root ^/ "lib/base/src")           in
  let dst_dir = Option.value dst_dir ~default:(hg_root ^/ "lib/base/bootstrap/src") in
  let config =
    { ppx_exe; src_dir; dst_dir }
  in

  let%bind () = P.rm_rf dst_dir in
  let%bind () = P.mkdir dst_dir ~p:() in

  let%bind () =
    (P.chdir src_dir (P.run "hg" ["files"; "."])) |- P.iter_lines (process_file ~config)
  in

  if check then
    P.run "hg" ["diff"; dst_dir]
  else
    let%bind () = P.run (hg_root ^/ "bin/make-hgignore") [] in
    let%bind () = P.run "hg" ["addremove"; "-s"; "50"; dst_dir] in
    P.run "fe" ["obligations"; "update-low-review-files"]

let spec =
  let open Command.Let_syntax in
  let%map_open
    check = flag "-check" no_arg
              ~doc:" don't commit, just check that the bootstrap directory is up-to-date"
  and debug =
    flag "-debug" no_arg
      ~doc:" print command as they are executed"
  and ppx_exe =
    flag "-ppx" (optional file)
      ~doc:"PPX ppx driver to use to pre-process files"
  and src_dir =
    flag "-src" (optional file)
      ~doc:"SRC source directory"
  and dst_dir =
    flag "-dst" (optional file)
      ~doc:"DST destination directory"
  in
  fun () ->
    let proc = process ~check ~ppx_exe ~src_dir ~dst_dir in
    if debug then P.Logged.eval proc else P.eval proc

let () =
  Command.run
    (Command.basic' spec ~summary:"Produce the bootstrapped version of base")
