(* OASIS_START *)
(* OASIS_STOP *)
# 4 "myocamlbuild.ml"

module JS = Jane_street_ocamlbuild_goodies

let dev_mode = true

let dispatch = function
  | After_rules ->
    let prod = "bootstrap/src/base_int63_backend.ml" in
    let dep  = "bootstrap/src/select-int63-backend.c" in
    rule "int63 backend"
      ~prod
      ~dep
      (fun _ _ ->
         let args =
           Printf.sprintf
             "-ccopt -E -c %s | \
              grep '^\"OUT:[^\"]*\"$' | \
              sed 's/\"OUT:\\([^\"]*\\)\"/\\1/' > %s"
             dep prod
         in
         Cmd (S [!Options.ocamlc; Sh args]))

  | _ ->
    ()

let () =
  Ocamlbuild_plugin.dispatch (fun hook ->
    JS.alt_cmxs_of_cmxa_rule hook;
    JS.pass_predicates_to_ocamldep hook;
    if dev_mode && not Sys.win32 then JS.track_external_deps hook;
    dispatch hook;
    dispatch_default hook)
