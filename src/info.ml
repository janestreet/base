(* This module is trying to minimize dependencies on modules in Core, so as to allow
   [Info], [Error], and [Or_error] to be used in as many places as possible. Please avoid
   adding new dependencies. *)

open! Import
include Info_intf
module String = String0

module Message = struct
  type t =
    | Could_not_construct of Sexp.t
    | String of string
    | Exn of exn
    | Sexp of Sexp.t
    | Tag_sexp of string * Sexp.t * Source_code_position0.t option
    | Tag_t of string * t
    | Tag_arg of string * Sexp.t * t
    | Of_list of int option * t list
    | With_backtrace of t * string (* backtrace *)
  [@@deriving_inline sexp_of]

  let rec sexp_of_t =
    (function
     | Could_not_construct arg0__001_ ->
       let res0__002_ = Sexp.sexp_of_t arg0__001_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Could_not_construct"; res0__002_ ]
     | String arg0__003_ ->
       let res0__004_ = sexp_of_string arg0__003_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "String"; res0__004_ ]
     | Exn arg0__005_ ->
       let res0__006_ = sexp_of_exn arg0__005_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Exn"; res0__006_ ]
     | Sexp arg0__007_ ->
       let res0__008_ = Sexp.sexp_of_t arg0__007_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Sexp"; res0__008_ ]
     | Tag_sexp (arg0__009_, arg1__010_, arg2__011_) ->
       let res0__012_ = sexp_of_string arg0__009_
       and res1__013_ = Sexp.sexp_of_t arg1__010_
       and res2__014_ = sexp_of_option Source_code_position0.sexp_of_t arg2__011_ in
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "Tag_sexp"; res0__012_; res1__013_; res2__014_ ]
     | Tag_t (arg0__015_, arg1__016_) ->
       let res0__017_ = sexp_of_string arg0__015_
       and res1__018_ = sexp_of_t arg1__016_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Tag_t"; res0__017_; res1__018_ ]
     | Tag_arg (arg0__019_, arg1__020_, arg2__021_) ->
       let res0__022_ = sexp_of_string arg0__019_
       and res1__023_ = Sexp.sexp_of_t arg1__020_
       and res2__024_ = sexp_of_t arg2__021_ in
       Sexplib0.Sexp.List
         [ Sexplib0.Sexp.Atom "Tag_arg"; res0__022_; res1__023_; res2__024_ ]
     | Of_list (arg0__025_, arg1__026_) ->
       let res0__027_ = sexp_of_option sexp_of_int arg0__025_
       and res1__028_ = sexp_of_list sexp_of_t arg1__026_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "Of_list"; res0__027_; res1__028_ ]
     | With_backtrace (arg0__029_, arg1__030_) ->
       let res0__031_ = sexp_of_t arg0__029_
       and res1__032_ = sexp_of_string arg1__030_ in
       Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "With_backtrace"; res0__031_; res1__032_ ]
      : t -> Sexplib0.Sexp.t)
  ;;

  [@@@end]

  let rec to_sexps_hum t ac =
    match t with
    | Could_not_construct _ as t -> sexp_of_t t :: ac
    | String string -> Atom string :: ac
    | Exn exn -> Exn.sexp_of_t exn :: ac
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

  (* We use a global [state ref] so we can mutate [state], but still [globalize] with no
     cost and without duplicating state. *)
  type info = { state : state ref } [@@unboxed]

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

  (* This is a no-op, since [info] is unboxed. *)
  let globalize_info { state } = { state }

  (* We keep a list of stack_frames while computing, rather than using the call stack. *)
  type stack_frame =
    | In_info of info
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
    match !(info.state) with
    | Initial cons ->
      info.state := Computing;
      compute_constructor cons (In_info info :: stack)
    | Computing ->
      compute_message (Could_not_construct (Atom "cycle while computing message")) stack
    | Final message -> compute_message message stack

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
       | exception exn -> compute_message (Could_not_construct (Exn.sexp_of_t exn)) stack)

  and compute_message message stack =
    match stack with
    | [] -> message
    | In_info info :: stack ->
      info.state := Final message;
      compute_message message stack
    | In_tag_arg (tag, arg) :: stack ->
      compute_message (Tag_arg (tag, arg, message)) stack
    | In_tag_t tag :: stack -> compute_message (Tag_t (tag, message)) stack
    | In_list { fwd_prefix; rev_suffix } :: stack ->
      compute_info_list ~fwd_prefix ~rev_suffix:(message :: rev_suffix) stack
  ;;

  (* Helper functions for converting and constructing [info]. *)

  let to_message info = compute_info info []
  let of_message message = { state = ref (Final message) }

  let is_computed info =
    match !(info.state) with
    | Initial _ | Computing -> false
    | Final _ -> true
  ;;

  let of_cons cons = { state = ref (Initial cons) }
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

let globalize = Computed.globalize_info
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
let of_string message = of_message (String message)
let createf format = Printf.ksprintf of_string format
let of_thunk f = of_lazy_message (lazy (String (f ())))

let create ?here ?strict tag x sexp_of_x =
  match strict with
  | None -> of_lazy_message (lazy (Tag_sexp (tag, sexp_of_x x, here)))
  | Some () -> of_message (Tag_sexp (tag, sexp_of_x x, here))
;;

let create_s sexp = of_message (Sexp sexp)
let tag t ~tag = of_cons (Cons_tag_t (tag, t))
let tag_s_lazy t ~tag = of_lazy_cons (lazy (Cons_tag_arg ("", Lazy.force tag, t)))
let tag_s t ~tag = of_cons (Cons_tag_arg ("", tag, t))
let tag_arg t tag x sexp_of_x = of_lazy_cons (lazy (Cons_tag_arg (tag, sexp_of_x x, t)))
let of_list ts = of_cons (Cons_list ts)

exception Exn of t

let () =
  (* We install a custom exn-converter rather than use
     [exception Exn of t [@@deriving_inline sexp] ... [@@@end]] to eliminate the extra
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
    | Message.Exn exn -> exn
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
  | _, None -> of_message (Message.Exn exn)
  | _, Some backtrace ->
    of_lazy_message (lazy (With_backtrace (Sexp (Exn.sexp_of_t exn), backtrace)))
;;

include Pretty_printer.Register_pp (struct
  type nonrec t = t

  let module_name = "Base.Info"
  let pp ppf t = Stdlib.Format.pp_print_string ppf (to_string_hum t)
end)

module Internal_repr = struct
  include Message

  let to_info = of_message
  let of_info = to_message
end
