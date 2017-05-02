{
open StdLabels
open Printf

let deprecated_msg what =
  sprintf
    "[@@deprecated \"\\\n\
     [2016-09] this element comes from the stdlib distributed with OCaml.\n\
     Refering to the stdlib directly is discouraged by Base. You should either\n\
     use the equivalent functionality offered by Base, or if you really want to\n\
     refer to the stdlib, use Caml.%s instead\"]"
    what

let deprecated_msg_no_equivalent what =
  sprintf
    "[@@deprecated \"\\\n\
     [2016-09] this element comes from the stdlib distributed with OCaml.\n\
     There is not equivalent functionality in Base or Stdio at the moment,\n\
     so you need to use [Caml.%s] instead\"]"
    what

let deprecated_msg_with_repl repl =
  sprintf
    "[@@deprecated \"\\\n\
     [2016-09] this element comes from the stdlib distributed with OCaml.\n\
     Use [%s] instead.\"]"
    repl

let deprecated_msg_with_approx_repl ~id repl =
  sprintf
    "[@@deprecated \"\\\n\
     [2016-09] this element comes from the stdlib distributed with OCaml.\n\
     There is no equivalent functionality in Base or Stdio but you can use\n\
     [%s] instead.\n\
     Alternatively, if you really want to refer the stdlib function you can\n\
     use [Caml.%s].\"]"
    repl id

type replacement =
  | No_equivalent
  | Repl of string
  | Approx of string

let val_replacement = function
  | "( != )"              -> Repl "not (phys_equal ...)"
  | "( == )"              -> Repl "phys_equal"
  | "( ** )"              -> Repl "**."
  | "close_in"            -> Repl "Stdio.In_channel.close"
  | "close_in_noerr"      -> Repl "Stdio.In_channel.close"
  | "close_out"           -> Repl "Stdio.Out_channel.close"
  | "close_out_noerr"     -> Repl "Stdio.Out_channel.close"
  | "flush"               -> Repl "Stdio.Out_channel.flush"
  | "flush_all"           -> No_equivalent
  | "in_channel_length"   -> Repl "Stdio.In_channel.length"
  | "input"               -> Repl "Stdio.In_channel.input"
  | "input_binary_int"    -> Repl "Stdio.In_channel.input_binary_int"
  | "input_byte"          -> Repl "Stdio.In_channel.input_byte"
  | "input_char"          -> Repl "Stdio.In_channel.input_char"
  | "input_line"          -> Repl "Stdio.In_channel.input_line"
  | "input_value"         -> Repl "Stdio.In_channel.unsafe_input_value"
  | "open_in"             -> Repl "Stdio.In_channel.create"
  | "open_in_bin"         -> Repl "Stdio.In_channel.create"
  | "open_in_gen"         -> No_equivalent
  | "open_out"            -> Repl "Stdio.Out_channel.create"
  | "open_out_bin"        -> Repl "Stdio.Out_channel.create"
  | "open_out_gen"        -> No_equivalent
  | "out_channel_length"  -> Repl "Stdio.Out_channel.length"
  | "output"              -> Repl "Stdio.Out_channel.output"
  | "output_binary_int"   -> Repl "Stdio.Out_channel.output_binary_int"
  | "output_byte"         -> Repl "Stdio.Out_channel.output_byte"
  | "output_bytes"        -> No_equivalent
  | "output_char"         -> Repl "Stdio.Out_channel.output_char"
  | "output_string"       -> Repl "Stdio.Out_channel.output_string"
  | "output_substring"    -> Repl "Stdio.Out_channel.output"
  | "output_value"        -> Repl "Stdio.Out_channel.output_value"
  | "pos_in"              -> Repl "Stdio.In_channel.pos"
  | "pos_out"             -> Repl "Stdio.Out_channel.pos"
  | "prerr_bytes"         -> No_equivalent
  | "prerr_char"          -> Repl "Stdio.Out_channel.output_char Stdio.stderr"
  | "prerr_endline"       -> Repl "Stdio.prerr_endline"
  | "prerr_float"         -> Repl "Stdio.eprintf \"%f\""
  | "prerr_int"           -> Repl "Stdio.eprintf \"%d\""
  | "prerr_newline"       -> Repl "Stdio.eprintf \"\n%!\""
  | "prerr_string"        -> Repl "Stdio.Out_channel.output_string Stdio.stderr"
  | "print_bytes"         -> No_equivalent
  | "print_char"          -> Repl "Stdio.Out_channel.output_char Stdio.stdout"
  | "print_endline"       -> Repl "Stdio.print_endline"
  | "print_float"         -> Repl "Stdio.eprintf \"%f\""
  | "print_int"           -> Repl "Stdio.eprintf \"%d\""
  | "print_newline"       -> Repl "Stdio.eprintf \"\n%!\""
  | "print_string"        -> Repl "Stdio.Out_channel.output_string Stdio.stdout"
  | "read_float"          -> No_equivalent
  | "read_int"            -> No_equivalent
  | "read_line"           -> Repl "Stdio.In_channel.input_line"
  | "really_input"        -> Repl "Stdio.In_channel.really_input"
  | "really_input_string" -> Approx "Stdio.Out_channel"
  | "seek_in"             -> Repl "Stdio.In_channel.seek"
  | "seek_out"            -> Repl "Stdio.Out_channel.seek"
  | "set_binary_mode_in"  -> Repl "Stdio.In_channel.set_binary_mode"
  | "set_binary_mode_out" -> Repl "Stdio.Out_channel.set_binary_mode"
  | "stderr"              -> Repl "Stdio.stderr"
  | "stdin"               -> Repl "Stdio.stdin"
  | "stdout"              -> Repl "Stdio.stdout"
  (* This is documented as DO-NOT-USE in the stdlib *)
  | "unsafe_really_input" -> No_equivalent
  | _                     -> No_equivalent
;;

let replace_val id line =
  let msg =
    match val_replacement id with
    | No_equivalent -> deprecated_msg_no_equivalent id
    | Repl repl     -> deprecated_msg_with_repl repl
    | Approx repl   -> deprecated_msg_with_approx_repl repl ~id
  in
  sprintf "%s\n%s" line msg
;;
}

let id_trail = ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let id = ['a'-'z' 'A'-'Z' '_' '0'-'9'] id_trail
let val_id = id | '(' [^ ')']* ')'
let params = ('(' [^')']* ')' | ['+' '-']? '\'' id) " "

let val_ = "val " | "external "

rule line = parse
  | "module Camlinternal" _*
    { "" (* We can't deprecate these *) }

  | "type " (params? (id as id) _* as def)
    { sprintf "type nonrec %s\n%s" def
        (match id with
         | "in_channel"  -> deprecated_msg_with_repl "Stdio.In_channel.t"
         | "out_channel" -> deprecated_msg_with_repl "Stdio.Out_channel.t"
         | _ -> deprecated_msg id)
    }

  | val_ (val_id as id) _* as line { replace_val id line }

  | "exception " (id as id) _* as line
  | "module " (id as id) _* as line
    { sprintf "%s\n%s" line (deprecated_msg id) }
  | _* as line
    { ksprintf failwith "cannot parse this: %s" line }
