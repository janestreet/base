(* This module is trying to minimize dependencies on modules in Core, so as to allow
   [Info], [Error], and [Or_error] to be used in as many places as possible. Please avoid
   adding new dependencies. *)

open! Import
include Info_intf.Definitions
module Sexp = Sexp0
module String = String0

module Message = struct
  type t =
    | Could_not_construct of Sexp.t
    | String of string
    | Exn of exn Modes.Contended_via_portable.t Modes.Global.t
    | Sexp of Sexp.t
    | Tag_sexp of string * Sexp.t * Source_code_position0.t option
    | Tag_t of string * t
    | Tag_arg of string * Sexp.t * t
    | Of_list of int option * t list
    | With_backtrace of t * string (* backtrace *)
  [@@deriving sexp_of]

  let rec to_sexps_hum t ac =
    match t with
    | Could_not_construct _ as t -> sexp_of_t t :: ac
    | String string -> Atom string :: ac
    | Exn { global = exn } ->
      Exn.sexp_of_t (Modes.Contended_via_portable.unwrap exn) :: ac
    | Sexp sexp -> sexp :: ac
    | Tag_sexp (tag, sexp, here) ->
      List
        (Atom tag
         :: sexp
         ::
         (match here with
          | None -> []
          | Some here -> [ Source_code_position0.sexp_of_t here ]))
      :: ac
    | Tag_t (tag, t) -> List (Atom tag :: to_sexps_hum t []) :: ac
    | Tag_arg (tag, sexp, t) ->
      let body = sexp :: to_sexps_hum t [] in
      if String.length tag = 0 then List body :: ac else List (Atom tag :: body) :: ac
    | With_backtrace (t, backtrace) ->
      Sexp.List
        [ to_sexp_hum t; sexp_of_list sexp_of_string (String.split_lines backtrace) ]
      :: ac
    | Of_list (_, ts) ->
      List.fold (List.rev ts) ~init:ac ~f:(fun ac t -> to_sexps_hum t ac)

  and to_sexp_hum t =
    match to_sexps_hum t [] with
    | [ sexp ] -> sexp
    | sexps -> Sexp.List sexps
  ;;
end

open Message

module Computed = struct
  (* Memoized, lazily-computed representation of messages. Maintains its own state to
     avoid stack overflow from nested [Lazy.t]. *)

  (* We use a [Modes.Global.t] wrapper around mutable state so we can mutate the state,
     but still [globalize] with no cost and without duplicating state. *)
  type info = unstaged_info Modes.Global.t

  and unstaged_info =
    | Constant of Message.t
    | Staged of staged_info

  and staged_info = { state : state ref Modes.Contended_via_portable.t } [@@unboxed]

  (* An [info] starts as a [constructor]. When forced, it is marked [Computing] to avoid
     cycles. When finished, the final [message] is recorded. *)
  and state =
    | Initial of constructor
    | Computing
    | Final of Message.t

  (* Recursive constructors for [Info.t]. Others can be built directly as [Message.t]. *)
  and constructor =
    | Cons_lazy_info of info Lazy.t
    | Cons_list of info list
    | Cons_tag_arg of string * Sexp.t * info
    | Cons_tag_t of string * info

  (* We keep a list of stack_frames while computing, rather than using the call stack. *)
  type stack_frame =
    | In_info of staged_info
    | In_tag_arg of string * Sexp.t
    | In_tag_t of string
    | In_list of
        { fwd_prefix : info list
        ; rev_suffix : Message.t list
        }

  (* The following mutually-recursive functions compute a [Message.t] from an [info].
     All calls below are tail calls: we want to avoid using the call stack in favor
     of our own manual stack. *)
  let rec compute_info info stack =
    match Modes.Global.unwrap info with
    | Constant message -> compute_message message stack
    | Staged info ->
      let state = Modes.Contended_via_portable.unwrap info.state in
      (match !state with
       | Initial cons ->
         state := Computing;
         compute_constructor cons (In_info info :: stack)
       | Computing ->
         compute_message
           (Could_not_construct (Atom "cycle while computing message"))
           stack
       | Final message -> compute_message message stack)

  and compute_info_list ~fwd_prefix ~rev_suffix stack =
    match fwd_prefix with
    | info :: fwd_prefix -> compute_info info (In_list { fwd_prefix; rev_suffix } :: stack)
    | [] ->
      let infos =
        List.fold rev_suffix ~init:[] ~f:(fun tail message ->
          match message with
          | Of_list (_, messages) -> messages @ tail
          | _ -> message :: tail)
      in
      compute_message (Of_list (None, infos)) stack

  and compute_constructor cons stack =
    match cons with
    | Cons_tag_arg (tag, arg, info) -> compute_info info (In_tag_arg (tag, arg) :: stack)
    | Cons_tag_t (tag, info) -> compute_info info (In_tag_t tag :: stack)
    | Cons_list infos -> compute_info_list ~fwd_prefix:infos ~rev_suffix:[] stack
    | Cons_lazy_info lazy_info ->
      (match Lazy.force lazy_info with
       | info -> compute_info info stack
       | exception exn -> compute_message (Could_not_construct (sexp_of_exn exn)) stack)

  and compute_message message stack =
    match stack with
    | [] -> message
    | In_info info :: stack ->
      Modes.Contended_via_portable.unwrap info.state := Final message;
      compute_message message stack
    | In_tag_arg (tag, arg) :: stack ->
      compute_message (Tag_arg (tag, arg, message)) stack
    | In_tag_t tag :: stack -> compute_message (Tag_t (tag, message)) stack
    | In_list { fwd_prefix; rev_suffix } :: stack ->
      compute_info_list ~fwd_prefix ~rev_suffix:(message :: rev_suffix) stack
  ;;

  (* Helper functions for converting and constructing [info]. *)

  let to_message info = compute_info info []

  let%template of_message (message @ p) : info = { global = Constant message }
  [@@mode p = (portable, nonportable)]
  ;;

  let is_computed : info -> bool =
    fun t ->
    match t.global with
    | Constant _ -> true
    | Staged info ->
      let state = Modes.Contended_via_portable.unwrap info.state in
      (match !state with
       | Initial _ | Computing -> false
       | Final _ -> true)
  ;;

  let of_cons cons =
    Staged { state = ref (Initial cons) |> Modes.Contended_via_portable.wrap }
    |> Modes.Global.wrap
  ;;

  let of_lazy_info lazy_info = of_cons (Cons_lazy_info lazy_info)

  let of_lazy_cons lazy_cons =
    of_cons (Cons_lazy_info (lazy (of_cons (Lazy.force lazy_cons))))
  ;;

  let of_lazy_message lazy_message =
    of_cons (Cons_lazy_info (lazy (of_message (Lazy.force lazy_message))))
  ;;
end

open Computed

type t = Computed.info

let globalize x = x
let invariant _ = ()

(* It is OK to use [Message.to_sexp_hum], which is not stable, because [t_of_sexp] below
   can handle any sexp. *)
let sexp_of_t t = Message.to_sexp_hum (to_message t)
let t_of_sexp sexp = of_message (Message.Sexp sexp)
let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = { untyped = Any "Info.t" }
let compare t1 t2 = Sexp.compare (sexp_of_t t1) (sexp_of_t t2)
let compare__local t1 t2 = compare (globalize t1) (globalize t2)
let equal t1 t2 = Sexp.equal (sexp_of_t t1) (sexp_of_t t2)
let equal__local t1 t2 = equal (globalize t1) (globalize t2)
let hash_fold_t state t = Sexp.hash_fold_t state (sexp_of_t t)
let hash t = Hash.run hash_fold_t t

let to_string_hum t =
  match to_message t with
  | String s -> s
  | message -> Sexp.to_string_hum (Message.to_sexp_hum message)
;;

let to_string_mach t = Sexp.to_string_mach (sexp_of_t t)
let of_lazy l = of_lazy_message (lazy (String (Lazy.force l)))
let of_lazy_sexp l = of_lazy_message (lazy (Sexp (Lazy.force l)))
let of_lazy_t lazy_t = of_lazy_info lazy_t
let%template of_string message = (of_message [@mode portable]) (String message)
let createf format = Printf.ksprintf of_string format
let of_thunk f = of_lazy_message (lazy (String (f ())))

let%template[@kind k = (bits64, float64, value)] create ?here ?strict tag x sexp_of_x =
  match strict with
  | None -> of_lazy_message (lazy (Tag_sexp (tag, sexp_of_x x, here)))
  | Some () -> of_message (Tag_sexp (tag, sexp_of_x x, here))
;;

let%template create_s sexp = (of_message [@mode portable]) (Sexp sexp)
let tag t ~tag = of_cons (Cons_tag_t (tag, t))
let tag_s_lazy t ~tag = of_lazy_cons (lazy (Cons_tag_arg ("", Lazy.force tag, t)))
let tag_s t ~tag = of_cons (Cons_tag_arg ("", tag, t))
let tag_arg t tag x sexp_of_x = of_lazy_cons (lazy (Cons_tag_arg (tag, sexp_of_x x, t)))
let of_list ts = of_cons (Cons_list ts)

exception Exn of t

let () =
  (* We install a custom exn-converter rather than use
     [exception Exn of t [@@deriving sexp]] to eliminate the extra
     wrapping of "(Exn ...)". *)
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Exn] (function
    | Exn t -> sexp_of_t t
    | _ ->
      (* Reaching this branch indicates a bug in sexplib. *)
      assert false)
;;

let to_exn t =
  if not (is_computed t)
  then Exn t
  else (
    match to_message t with
    | Message.Exn { global = exn } -> Modes.Contended_via_portable.unwrap exn
    | _ -> Exn t)
;;

let of_exn ?backtrace exn =
  let backtrace =
    match backtrace with
    | None -> None
    | Some `Get -> Some (Stdlib.Printexc.get_backtrace ())
    | Some (`This s) -> Some s
  in
  match exn, backtrace with
  | Exn t, None -> t
  | Exn t, Some backtrace ->
    of_lazy_message (lazy (With_backtrace (to_message t, backtrace)))
  | _, None -> of_message (Message.Exn { global = Modes.Contended_via_portable.wrap exn })
  | _, Some backtrace ->
    of_lazy_message (lazy (With_backtrace (Sexp (Exn.sexp_of_t exn), backtrace)))
;;

include%template Pretty_printer.Register_pp [@mode portable] (struct
    type nonrec t = t

    let module_name = "Base.Info"
    let pp ppf t = Stdlib.Format.pp_print_string ppf (to_string_hum t)
  end)

module Internal_repr = struct
  include Message

  let to_info = of_message
  let of_info = to_message
end
