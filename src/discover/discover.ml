open Configurator.V1

let program =
  {|
int main(int argc, char ** argv)
{
  return __builtin_popcount(argc);
}
|}
;;

let () =
  let output = ref "" in
  main
    ~name:"discover"
    ~args:[ "-o", Set_string output, "FILENAME output file" ]
    (fun c ->
       let arch = ocaml_config_var c "architecture" in
       let cc = ocaml_config_var_exn c "native_c_compiler" in
       let is_gcc =
         (Process.run c cc [ "--version" ] ~env:[]).stdout
         |> String.lowercase_ascii
         |> String.starts_with ~prefix:"gcc"
       in
       let try_flag c flag = if c_test c ~c_flags:[ flag ] program then flag else "" in
       let flags =
         match arch with
         | Some s when String.length s >= 3 ->
           (match String.sub s 0 3 with
            | "ppc" | "pow" ->
              let popcntd = try_flag c "-mpopcntd" in
              if popcntd <> ""
              then popcntd
              (* needed to prevent crashing, only gcc has this option *)
              else if is_gcc
              then try_flag c "-mpopcntb"
              else ""
            | "spa" -> try_flag c "-mpopc"
            | "x86" | "amd" -> try_flag c "-mpopcnt"
            | _ -> "")
           (* armv8/arm64/aarch64 support popcnt via vcnt by default and don't require any flags *)
         | _ -> ""
       in
       let flags = if flags <> "" then [ flags ] else [] in
       Flags.write_sexp !output flags)
;;

