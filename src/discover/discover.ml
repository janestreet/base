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
    (fun _c ->
      let f,oc = Filename.open_temp_file "baseconf" ".c" in
      let has_popcnt =
        Fun.protect ~finally:(fun () -> close_out oc; Sys.remove f)
          (fun () -> Out_channel.(output_string oc program; flush oc);
            Sys.command (Printf.sprintf "cc %s -mpopcnt -o /dev/null >/dev/null 2>&1" f) = 0) in
      Flags.write_sexp !output (if has_popcnt then [ "-mpopcnt" ] else []))
;;
