{
open StdLabels
open Printf

let deprecated what =
  sprintf "[@@deprecated \"[2016-09] use Caml.%s instead\"]"
    what
}

let id = ['a'-'z' 'A'-'Z' '_' '0'-'9']+
let val_id = id | '(' [^ ')']* ')'
let params = ('(' [^')']* ')' | ['+' '-']? '\'' id) " "

rule line = parse
  | "type " (params? (id as id) _* as def)
      { sprintf "type nonrec %s %s" def (deprecated id) }
  | "val " (val_id as id) [^'=']* as line _*
      { sprintf "%s %s" line (deprecated id) }
  | "external " (val_id as id) _* as line
      { sprintf "%s %s" line (deprecated id) }
  | "exception " (id as id) _* as line
      { sprintf "%s %s" line (deprecated id) }
  | "module Camlinternal" _*
      { "" (* We can't deprecate these *) }
  | "module " (id as id) _* as line
      { sprintf "%s %s" line (deprecated id) }
  | _* as line
      { ksprintf failwith "cannot parse this: %s" line }
