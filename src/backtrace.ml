open! Import

type t = Caml.Printexc.raw_backtrace

let get ?(at_most_num_frames = Int.max_value) () =
  Caml.Printexc.get_callstack at_most_num_frames
;;

let to_string = Caml.Printexc.raw_backtrace_to_string

let sexp_of_t t =
  Sexp.List
    (List.map (String.split (to_string t) ~on:'\n')
       ~f:(fun x -> Sexp.Atom x))
;;

module Exn = struct
  let set_recording = Caml.Printexc.record_backtrace
  let am_recording  = Caml.Printexc.backtrace_status

  let most_recent () =
    if am_testing
    then "<backtrace elided in test>"
    else if not (am_recording ())
    then ""
    else Caml.Printexc.get_backtrace ()
  ;;

  (* We turn on backtraces by default if OCAMLRUNPARAM isn't set. *)
  let maybe_set_recording () =
    match Caml.Sys.getenv "OCAMLRUNPARAM" with
    | exception _ -> set_recording true
    | _ -> ()  (* the caller set something, they are responsible *)
  ;;

  let with_recording b ~f =
    let saved = am_recording () in
    set_recording b;
    Exn.protect ~f ~finally:(fun () -> set_recording saved)
  ;;
end

let initialize_module () =
  Exn.maybe_set_recording ();
;;
