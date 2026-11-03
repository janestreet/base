open! Import0
module Char = Char0
module Int = Int0
module Sexp = Sexp0
module String = String0
module Sys = Sys0

open struct
  let string_starts_with string char =
    String.length string >= 1 && Char.equal char (String.get string 0)
  ;;

  let string_contains_pair string char1 char2 =
    let len = String.length string in
    let rec string_contains_pair_starting_at pos =
      let next = pos + 1 in
      next < len
      && ((Char.equal char1 (String.get string pos)
           && Char.equal char2 (String.get string next))
          || string_contains_pair_starting_at next)
    in
    string_contains_pair_starting_at 0
  ;;
end

type t = Stdlib.Printexc.raw_backtrace

let elide = Dynamic.make false
let elided_message = "<backtrace elided in test>"

let get ?(at_most_num_frames = Int.max_value) () =
  Stdlib.Printexc.get_callstack at_most_num_frames
;;

let to_string t =
  if Dynamic.get elide then elided_message else Stdlib.Printexc.raw_backtrace_to_string t
;;

let to_string_list t = String.split_lines (to_string t)
let sexp_of_t t = Sexp.List (List.map (to_string_list t) ~f:(fun x -> Sexp.Atom x))

module Exn = struct
  let set_recording = Stdlib.Printexc.record_backtrace
  let am_recording = Stdlib.Printexc.backtrace_status
  let most_recent () = Stdlib.Printexc.get_raw_backtrace ()

  let most_recent_for_exn exn =
    if Exn.is_phys_equal_most_recent exn then Some (most_recent ()) else None
  ;;

  (* We turn on backtraces by default if OCAMLRUNPARAM doesn't explicitly mention them. *)
  let maybe_set_recording () =
    let ocamlrunparam_mentions_backtraces =
      match Sys.getenv "OCAMLRUNPARAM" with
      | None -> false
      | Some string ->
        string_starts_with string 'b' || string_contains_pair string ',' 'b'
    in
    if not ocamlrunparam_mentions_backtraces then set_recording true
  ;;

  (* the caller set something, they are responsible *)

  let%template[@mode m = (global, local)] with_recording b ~f =
    let saved = am_recording () in
    set_recording b;
    (Exn.protect [@mode m]) ~f ~finally:(fun () -> set_recording saved)
    [@nontail] [@exclave_if_local m ~reasons:[ May_return_local ]]
  ;;
end

let initialize_module () = Exn.maybe_set_recording ()
