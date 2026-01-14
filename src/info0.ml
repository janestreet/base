(* This module is trying to minimize dependencies on modules in Core, so as to allow
   [Info], [Error], and [Or_error] to be used in as many places as possible. Please avoid
   adding new dependencies. *)

open! Import
include Info_intf.Definitions
module Sexp = Sexp0
module String = String0
module Domain = Basement.Stdlib_shim.Domain
module Portable_atomic = Basement.Portable_atomic

module Message = struct
  type t =
    | Could_not_construct of Sexp.t
    | String of string
    | Exn of exn Modes.Global.t
    | Sexp of Sexp.t
    | Tag_sexp of string * Sexp.t * Source_code_position0.t option
    | Tag_t of string * t
    | Tag_arg of string * Sexp.t * t
    | Of_list of int option * t list
    | With_backtrace of t * string (* backtrace *)
  [@@deriving sexp_of]

  let rec to_sexps_hum (t : t) ac =
    match t with
    | Could_not_construct _ as t -> sexp_of_t t :: ac
    | String string -> Atom string :: ac
    | Exn { global = exn } -> Exn.sexp_of_t exn :: ac
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
    | Staged_portable of portable_staged_info
    | Staged_nonportable of staged_info

  and portable_staged_info = { state : portable_state Portable_atomic.t } [@@unboxed]
  and staged_info = { state : state ref Modes.Contended_via_portable.t } [@@unboxed]

  (* A [state] starts as a [constructor]. When forced, it is marked [Computing] to avoid
     cycles. When finished, the final [message] is recorded. *)
  and state =
    | Initial of info lazy_t constructor
    | Computing
    | Final of Message.t

  (* Just as [state] is a specialized implementation of [Lazy], [portable_state] is a
     specialized implementation of [Portable_lazy]. (The states correspond directly to the
     states implementing [Portable_lazy.t].)
  *)
  and portable_state =
    | Portable_initial of info Portable_lazy.t constructor
    | Portable_computing of Domain.id
    (* Unlike [state], we record the domain ID to detect if we are erroneously computing
       ourselves recursively, or if it's simply that another domain is already computing
       the value. *)
    | Portable_final of Message.t

  (* Recursive constructors for [Info.t]. Others can be built directly as [Message.t]. *)
  and 'lazy_cons constructor =
    | Cons_lazy_info of 'lazy_cons
    (* ['lazy_cons] is [info lazy] for nonportable constructors and [info Portable_lazy.t]
       for portable ones *)
    | Cons_list of info list
    | Cons_tag_arg of string * Sexp.t * info
    | Cons_tag_t of string * info

  (* We keep a list of stack_frames while computing, rather than using the call stack. *)
  type 'info stack_frame =
    | In_tag_arg of string * Sexp.t
    | In_tag_t of string
    | In_list of
        { fwd_prefix : info list
        ; rev_suffix : Message.t list
        }
    | In_info of 'info

  (* The (portable) prefix of a portable stack is a series of frames that produce portable
     values. The (nonportable) suffix does not produce portable values. The distinction is
     important because we want to be able to store the values produced from the prefix in
     a [Portable_atomic.t], which only accepts portable values.
  *)
  module Portable_stack = struct
    type t =
      #{ portable : portable_staged_info stack_frame list
       ; nonportable : staged_info stack_frame list Modes.Portable_via_contended.t
       }

    let of_nonportable_suffix nonportable =
      #{ portable = []; nonportable = Modes.Portable_via_contended.wrap nonportable }
    ;;

    let push x #{ portable; nonportable } = #{ portable = x :: portable; nonportable }
  end

  let%template compute_frame (frame @ p) (message @ p) =
    match frame with
    | In_info info -> `Info info
    | In_tag_arg (tag, arg) -> `Frame (Tag_arg (tag, arg, message))
    | In_tag_t tag -> `Frame (Tag_t (tag, message))
    | In_list { fwd_prefix; rev_suffix } -> `List (fwd_prefix, message :: rev_suffix)
  [@@mode p = (portable, nonportable)]
  ;;

  (* The [M] modules allow us to use a form of mode polymorphism in
     [compute_info_list_wrapper]. [M [@portable]] wraps portable values, [M] wraps
     non-portable values.
  *)

  module%template [@mode portable] M = Modes.Portable

  module M = struct
    type 'a t = 'a [@@warning "-unused-type-declaration"]

    let wrap_list x = x
    let unwrap_list x = x
    let wrap x = x
    let unwrap x = x
  end

  (* A mode-polymorphic way of computing the [Cons_list] constructor. An alternative
     implementation would be to duplicate this code (once for each mode), but we found it
     to be sufficiently complex to warrant factoring it out.
  *)
  let%template[@inline always] compute_info_list_wrapper
    ~fwd_prefix
    ~rev_suffix
    ~compute_info
    ~compute_message
    ~push_stack
    stack
    =
    let module M = M [@mode p] in
    match fwd_prefix with
    | info :: fwd_prefix ->
      compute_info info (push_stack (In_list { fwd_prefix; rev_suffix }) stack)
    | [] ->
      let infos =
        List.fold (M.wrap_list rev_suffix) ~init:(M.wrap []) ~f:(fun tail message ->
          let tail = M.unwrap tail in
          let message =
            match M.unwrap message with
            | Of_list (_, messages) ->
              M.wrap_list messages @ M.wrap_list tail |> M.unwrap_list
            | message -> message :: tail
          in
          M.wrap message)
        |> M.unwrap
      in
      compute_message (Of_list (None, infos)) stack
  [@@mode p = (portable, nonportable)]
  ;;

  (* A mode-polymorphic way of computing constructors. Similarly to
     [compute_info_list_wrapper], we found that the complexity warranted factoring it out.
  *)
  let%template[@inline always] compute_constructor_wrapper
    (cons @ p)
    stack
    ~compute_info
    ~compute_message
    ~compute_info_list
    ~lazy_force
    ~push_stack
    =
    match cons with
    | Cons_tag_arg (tag, arg, info) ->
      compute_info info (push_stack (In_tag_arg (tag, arg)) stack)
    | Cons_tag_t (tag, info) -> compute_info info (push_stack (In_tag_t tag) stack)
    | Cons_list infos -> compute_info_list ~fwd_prefix:infos ~rev_suffix:[] stack
    | Cons_lazy_info lazy_info ->
      (match lazy_force lazy_info with
       | info -> compute_info info stack
       | exception exn -> compute_message (Could_not_construct (sexp_of_exn exn)) stack)
  [@@mode p = (portable, nonportable)]
  ;;

  (* The following mutually-recursive functions compute a [Message.t] from an [info]. All
     calls below are tail calls: we want to avoid using the call stack in favor of our own
     manual stack. *)
  let rec compute_info (info : info) stack =
    match info.global with
    | Constant message -> compute_message message stack
    | Staged_portable portable_info ->
      compute_staged_portable portable_info (Portable_stack.of_nonportable_suffix stack)
    | Staged_nonportable info -> compute_staged_nonportable info stack

  and compute_info_portable (info : info @ portable) (stack @ portable) =
    match info.global with
    | Constant message -> compute_message_portable message stack
    | Staged_portable portable_info -> compute_staged_portable portable_info stack
    | Staged_nonportable info -> Modes.Contended_via_portable.refute_portable info.state

  (* This is a hand-written implementation of [Lazy]. *)
  and compute_staged_nonportable info stack =
    let state = Modes.Contended_via_portable.unwrap info.state in
    match !state with
    | Initial cons ->
      state := Computing;
      compute_constructor cons (In_info info :: stack)
    | Computing ->
      compute_message (Could_not_construct (Atom "cycle while computing message")) stack
    | Final message -> compute_message message stack

  (* Inlined from the implementation of [Portable_lazy]. Defunctionalizing the thunk we're
     computing allows us to use our manual stack rather than the callstack, which lets us
     be tail recursive and avoid blowing the stack when computing highly nested error
     values.

     See also [compute_staged_nonportable], which is similarly inlined from [Lazy] but is
     simpler.
  *)
  and compute_staged_portable info stack =
    match Portable_atomic.get info.state with
    | Portable_initial cons as uncomputed ->
      let computing = Portable_computing (Domain.self ()) in
      (match Portable_atomic.compare_and_set info.state uncomputed computing with
       | false ->
         (* Someone else beat us to starting the thunk! This can only happen to us once,
            so we just try again without any calls to cpu_relax. *)
         compute_staged_portable info stack
       | true ->
         compute_constructor_portable cons (Portable_stack.push (In_info info) stack))
    | Portable_computing id ->
      if equal (Domain.self () :> int) (id :> int)
      then
        compute_message_portable
          (Could_not_construct (Atom "cycle while computing message"))
          stack
      else (
        (* Someone else is forcing the lazy; relax then keep trying. *)
        Domain.cpu_relax ();
        compute_staged_portable info stack)
    | Portable_final message -> compute_message_portable message stack

  and compute_info_list ~fwd_prefix ~rev_suffix stack =
    compute_info_list_wrapper
      ~fwd_prefix
      ~rev_suffix
      stack
      ~compute_message
      ~compute_info
      ~push_stack:(fun x xs -> x :: xs)

  and compute_info_list_portable ~fwd_prefix ~rev_suffix stack =
    [%template compute_info_list_wrapper [@mode portable]]
      ~fwd_prefix
      ~rev_suffix
      stack
      ~compute_message:compute_message_portable
      ~compute_info:compute_info_portable
      ~push_stack:Portable_stack.push

  and compute_constructor cons stack =
    compute_constructor_wrapper
      cons
      stack
      ~compute_info
      ~compute_message
      ~compute_info_list
      ~lazy_force:Lazy.force
      ~push_stack:(fun x xs -> x :: xs)

  and compute_constructor_portable cons stack =
    [%template compute_constructor_wrapper [@mode portable]]
      cons
      stack
      ~compute_info:compute_info_portable
      ~compute_message:compute_message_portable
      ~compute_info_list:compute_info_list_portable
      ~lazy_force:Portable_lazy.force
      ~push_stack:Portable_stack.push

  and compute_message_portable (message @ portable) (stack : Portable_stack.t @ portable) =
    match stack with
    | #{ portable = []; nonportable } ->
      compute_message message (Modes.Portable_via_contended.unwrap nonportable)
    | #{ portable = frame :: portable; nonportable } ->
      let stack : Portable_stack.t = #{ portable; nonportable } in
      (match [%template compute_frame [@mode portable]] frame message with
       | `Frame message -> compute_message_portable message stack
       | `List (fwd_prefix, rev_suffix) ->
         compute_info_list_portable ~fwd_prefix ~rev_suffix stack
       | `Info info ->
         Portable_atomic.set info.state (Portable_final message);
         compute_message_portable message stack)

  and compute_message message stack =
    match stack with
    | [] -> message
    | frame :: stack ->
      (match compute_frame frame message with
       | `Frame message -> compute_message message stack
       | `List (fwd_prefix, rev_suffix) -> compute_info_list ~fwd_prefix ~rev_suffix stack
       | `Info info ->
         Modes.Contended_via_portable.unwrap info.state := Final message;
         compute_message message stack)
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
    | Staged_portable info ->
      (match Portable_atomic.get info.state with
       | Portable_initial _ | Portable_computing _ -> false
       | Portable_final _ -> true)
    | Staged_nonportable info ->
      let state = Modes.Contended_via_portable.unwrap info.state in
      (match !state with
       | Initial _ | Computing -> false
       | Final _ -> true)
  ;;

  let of_cons cons =
    Staged_nonportable { state = ref (Initial cons) |> Modes.Contended_via_portable.wrap }
    |> Modes.Global.wrap
  ;;

  let%template of_cons cons : info =
    { global = Staged_portable { state = Portable_atomic.make (Portable_initial cons) } }
  [@@mode portable]
  ;;

  let of_lazy_info lazy_info = of_cons (Cons_lazy_info lazy_info)

  let%template of_portable_lazy_info lazy_info =
    (of_cons [@mode portable]) (Cons_lazy_info lazy_info)
  ;;

  let of_thunked_cons lazy_cons = of_cons (Cons_lazy_info (lazy (of_cons (lazy_cons ()))))

  let%template of_thunked_cons lazy_cons =
    (of_cons [@mode portable])
      (Cons_lazy_info
         (Portable_lazy.from_fun (fun () -> (of_cons [@mode portable]) (lazy_cons ()))))
  [@@mode portable]
  ;;

  let of_thunked_message lazy_message =
    of_cons (Cons_lazy_info (lazy (of_message (lazy_message ()))))
  ;;

  let%template of_thunked_message lazy_message =
    (of_cons [@mode portable])
      (Cons_lazy_info
         (Portable_lazy.from_fun (fun () ->
            (of_message [@mode portable]) (lazy_message ()))))
  [@@mode portable]
  ;;
end

open Computed

type t = Computed.info

let globalize x = x
let invariant _ = ()

(* It is OK to use [Message.to_sexp_hum], which is not stable, because [t_of_sexp] below
   can handle any sexp. *)
let sexp_of_t t = Message.to_sexp_hum (to_message t)
let%template t_of_sexp sexp = (of_message [@mode portable]) (Sexp sexp)
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
let%template of_string message = (of_message [@mode portable]) (String message)
let%template create_s sexp = (of_message [@mode portable]) (Sexp sexp)
let createf format = Printf.ksprintf of_string format
let createf_portable format = Printf.ksprintf (fun string () -> of_string string) format

(* Lazies *)

let of_lazy_t = of_lazy_info
let of_lazy l = of_thunked_message (fun () -> String (Lazy.force l))
let of_lazy_sexp l = of_thunked_message (fun () -> Sexp (Lazy.force l))

(* Portable lazies *)

let of_portable_lazy_t = of_portable_lazy_info

let%template of_portable_lazy l =
  (of_thunked_message [@mode portable]) (fun () -> String (Portable_lazy.force l))
;;

let%template of_portable_lazy_sexp l =
  (of_thunked_message [@mode portable]) (fun () -> Sexp (Portable_lazy.force l))
;;

let%template of_thunk f = (of_thunked_message [@mode p]) (fun () -> String (f ()))
[@@mode p = (portable, nonportable)]
;;

let%template[@kind k = (bits64, float64, value)] [@mode p = (portable, nonportable)] create
  ?here
  ?strict
  tag
  x
  sexp_of_x
  =
  match strict with
  | None -> (of_thunked_message [@mode p]) (fun () -> Tag_sexp (tag, sexp_of_x x, here))
  | Some () -> (of_message [@mode p]) (Tag_sexp (tag, sexp_of_x x, here))
;;

[%%template
[@@@mode.default p = (portable, nonportable)]

let tag t ~tag = (of_cons [@mode p]) (Cons_tag_t (tag, t))
let tag_s t ~tag = (of_cons [@mode p]) (Cons_tag_arg ("", tag, t))
let of_list ts = (of_cons [@mode p]) (Cons_list ts)]

let tag_s_lazy t ~tag = of_thunked_cons (fun () -> Cons_tag_arg ("", Lazy.force tag, t))

let%template tag_s_portable_lazy t ~tag =
  (of_thunked_cons [@mode p]) (fun () -> Cons_tag_arg ("", Portable_lazy.force tag, t))
[@@mode p = (portable, nonportable)]
;;

let%template tag_arg t tag x sexp_of_x =
  (of_thunked_cons [@mode p]) (fun () -> Cons_tag_arg (tag, sexp_of_x x, t))
[@@mode p = (portable, nonportable)]
;;

exception Exn of t @@ portable

let () =
  (* We install a custom exn-converter rather than use
     [exception Exn of t [@@deriving sexp]] to eliminate the extra wrapping of "(Exn
     ...)". *)
  Sexplib0.Sexp_conv.Exn_converter.add [%extension_constructor Exn] (function
    | Exn t -> sexp_of_t t
    | _ ->
      (* Reaching this branch indicates a bug in sexplib. *)
      assert false)
;;

let portabilize (t : t) : t =
  match t.global with
  (* As an optimization: if the value is already known-portable, there's no need to force
     its computation. *)
  | Staged_portable x -> { global = Staged_portable x }
  | Constant x -> { global = Constant x }
  | Staged_nonportable _ -> [%template of_message [@mode portable]] (to_message t)
;;

let to_exn t =
  if not (is_computed t)
  then Exn (portabilize t)
  else (
    match to_message t with
    | Exn { global = exn } -> exn
    | _ -> Exn (portabilize t))
;;

let%template of_exn ?backtrace exn =
  let backtrace =
    match backtrace with
    | None -> None
    | Some `Get -> Some (Stdlib.Printexc.get_backtrace ())
    | Some (`This s) -> Some s
  in
  match exn, backtrace with
  | Exn t, None -> t
  | Exn t, Some backtrace ->
    (of_thunked_message [@mode portable]) (fun () ->
      With_backtrace (to_message t, backtrace))
  | _, None -> of_message (Exn { global = exn })
  | _, Some backtrace ->
    (of_thunked_message [@mode portable]) (fun () ->
      With_backtrace (Sexp (Exn.sexp_of_t exn), backtrace))
;;

include%template Pretty_printer.Register_pp [@mode portable] (struct
    type nonrec t = t

    let module_name = "Base.Info"
    let pp ppf t = Stdlib.Format.pp_print_string ppf (to_string_hum t)
  end)

module Internal_repr = struct
  include Message

  let%template to_info = (of_message [@mode p]) [@@mode p = (portable, nonportable)]
  let of_info = to_message
end

module Portable = struct
  type nonrec t = t Modes.Portable.t
  [@@deriving compare ~localize, equal ~localize, hash, sexp, sexp_grammar]

  let globalize t =
    t |> Modes.Portable.unwrap |> globalize |> portabilize |> Modes.Portable.wrap
  ;;

  let create_s sexp = sexp |> create_s |> Modes.Portable.wrap
  let of_string str = str |> of_string |> Modes.Portable.wrap

  let%template of_list xs =
    xs |> Modes.Portable.unwrap_list |> (of_list [@mode portable]) |> Modes.Portable.wrap
  ;;

  let of_portable_lazy_sexp sexp = sexp |> of_portable_lazy_sexp |> Modes.Portable.wrap
  let of_portable_lazy str = str |> of_portable_lazy |> Modes.Portable.wrap

  let of_portable_lazy_t t =
    t
    |> Portable_lazy.map ~f:[%eta (Modes.Portable.unwrap : t -> _)]
    |> of_portable_lazy_t
    |> Modes.Portable.wrap
  ;;

  let%template create ?here ?strict message arg sexp_of_arg =
    (create [@kind k] [@mode portable]) ?here ?strict message arg sexp_of_arg
    |> Modes.Portable.wrap
  [@@kind k = (bits64, float64, value)]
  ;;

  let%template tag_s t ~tag =
    t |> Modes.Portable.unwrap |> (tag_s [@mode portable]) ~tag |> Modes.Portable.wrap
  ;;

  let%template tag t ~tag =
    t |> Modes.Portable.unwrap |> (tag [@mode portable]) ~tag |> Modes.Portable.wrap
  ;;

  let%template tag_arg t message x f =
    (tag_arg [@mode portable]) (Modes.Portable.unwrap t) message x f
    |> Modes.Portable.wrap
  ;;

  let%template of_thunk f = (of_thunk [@mode portable]) f |> Modes.Portable.wrap
  let createf fmt = Printf.ksprintf of_string fmt
end

let of_portable = Modes.Portable.unwrap
let to_portable = Modes.Portable.wrap
let to_portable_portabilize t = to_portable (portabilize t)
