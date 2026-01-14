open! Import
module Sexp = Sexp0

type t = exn [@@deriving sexp_of]

exception Finally of t * t [@@deriving sexp]
exception Reraised of string * t [@@deriving sexp]
exception Sexp of Sexp.t
exception Sexp_lazy of Sexp.t Portable_lazy.t

(* We install a custom exn-converter rather than use:

   {[
     exception Sexp of Sexp.t [@@deriving sexp]
   ]}

   to eliminate the extra wrapping of [(Sexp ...)]. (Similarly for [Sexp_lazy].) *)
let () =
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Sexp] (function
    | Sexp t -> t
    | _ ->
      (* Reaching this branch indicates a bug in sexplib. *)
      assert false);
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Sexp_lazy] (function
    | Sexp_lazy t -> Portable_lazy.force t
    | _ -> assert false)
;;

let create_s sexp = Sexp sexp
let create_s_lazy lazy_sexp = Sexp_lazy lazy_sexp

let raise_with_original_backtrace t backtrace =
  Stdlib.Printexc.raise_with_backtrace t backtrace
;;

external is_phys_equal_most_recent
  :  t
  -> bool
  @@ portable
  = "Base_caml_exn_is_most_recent_exn"

let reraise exn str =
  let exn' = Reraised (str, exn) in
  if is_phys_equal_most_recent exn
  then (
    let bt = Stdlib.Printexc.get_raw_backtrace () in
    raise_with_original_backtrace exn' bt)
  else raise exn'
;;

let reraisef exc format = Printf.ksprintf (fun str () -> reraise exc str) format
let to_string exc = Sexp.to_string_hum ~indent:2 (sexp_of_exn exc)
let to_string_mach exc = Sexp.to_string_mach (sexp_of_exn exc)
let sexp_of_t = sexp_of_exn

let protectx ~f x ~(finally : _ -> unit) =
  match f x with
  | res ->
    finally x;
    res
  | exception exn ->
    let bt = Stdlib.Printexc.get_raw_backtrace () in
    (match finally x with
     | () -> raise_with_original_backtrace exn bt
     | exception final_exn ->
       (* Unfortunately, the backtrace of the [final_exn] is discarded here. *)
       raise_with_original_backtrace (Finally (exn, final_exn)) bt)
;;

let protect ~f ~finally = protectx ~f () ~finally

let does_raise (type a) (f : unit -> a) =
  try
    ignore (f () : a);
    false
  with
  | _ -> true
;;

module Serialized = struct
  type t : value mod contended portable =
    | Sexp of Sexp.t
    | String of string
  [@@unsafe_allow_any_mode_crossing]

  let of_exn exn =
    match sexp_of_exn_opt exn with
    | Some sexp -> Sexp sexp
    | None -> String (Stdlib.Printexc.to_string exn)
  ;;

  let pp ppf t =
    match t with
    | Sexp sexp -> Sexp.pp_hum ppf sexp
    | String str -> Stdlib.Format.pp_print_string ppf str
  ;;
end

include%template Pretty_printer.Register_pp [@modality portable] (struct
    type t = exn

    let pp ppf t = Serialized.of_exn t |> Serialized.pp ppf
    let module_name = "Base.Exn"
  end)

let get_err_formatter = Obj.magic_portable Stdlib.Format.get_err_formatter

let print_with_backtrace exn raw_backtrace =
  let serialized_exn = Serialized.of_exn exn in
  Stdlib.Format.fprintf
    (get_err_formatter ())
    "@[<2>Uncaught exception:@\n@\n@[%a@]@]@\n@."
    Serialized.pp
    serialized_exn;
  if Stdlib.Printexc.backtrace_status ()
  then Stdlib.Printexc.print_raw_backtrace Stdlib.stderr raw_backtrace;
  Stdlib.flush Stdlib.stderr
;;

let set_uncaught_exception_handler () =
  Stdlib.Printexc.set_uncaught_exception_handler print_with_backtrace
;;

(* skips [at_exit] handlers *)
external exit_immediately : int -> 'a @@ portable = "caml_sys_exit"

let handle_uncaught_aux ~do_at_exit ~exit_or_ignore f =
  try f () with
  | exc ->
    let raw_backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (* One reason to run [do_at_exit] handlers before printing out the error message is
       that it helps curses applications bring the terminal in a good state, otherwise the
       error message might get corrupted. Also, the OCaml top-level uncaught exception
       handler does the same. *)
    (try do_at_exit () with
     | _ -> ());
    (try print_with_backtrace exc raw_backtrace with
     | _ ->
       (try
          Stdlib.Printf.eprintf "Exn.handle_uncaught could not print; exiting anyway\n%!"
        with
        | _ -> ()));
    exit_or_ignore 1
;;

let handle_uncaught_and_exit f =
  handle_uncaught_aux f ~exit_or_ignore:exit_immediately ~do_at_exit:Stdlib.do_at_exit
;;

let handle_uncaught_and_exit_immediately f =
  handle_uncaught_aux f ~exit_or_ignore:exit_immediately ~do_at_exit:ignore
;;

let handle_uncaught ~exit:must_exit f =
  handle_uncaught_aux
    f
    ~exit_or_ignore:(if must_exit then exit_immediately else ignore)
    ~do_at_exit:(if must_exit then Stdlib.do_at_exit else ignore)
;;

let reraise_uncaught str func =
  try func () with
  | exn ->
    let bt = Stdlib.Printexc.get_raw_backtrace () in
    raise_with_original_backtrace (Reraised (str, exn)) bt
;;

external clear_backtrace : unit -> unit @@ portable = "Base_clear_caml_backtrace_pos"
[@@noalloc]

let raise_without_backtrace e =
  (* We clear the backtrace to reduce confusion, so that people don't think whatever is
     stored corresponds to this raise. *)
  clear_backtrace ();
  Stdlib.raise_notrace e
;;

let initialize_module () = set_uncaught_exception_handler ()

module Private = struct
  let clear_backtrace = clear_backtrace
end
