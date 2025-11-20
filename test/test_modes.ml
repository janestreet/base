open! Base

(* Demonstrate the mode safety of [Modes.At_locality]. We avoid [match] to show that
   dynamic dispatch is not required. We avoid [Obj] and [external] to show that the
   operations can be implemented safely. The implementation in [modes.ml] is essentially a
   hand-optimized version of this one to avoid (heap or stack) allocation.

   We can't demonstrate the safety of the mode-crossing, because the compiler is not smart
   enough about GADTs. *)
module _ : Modes.At_locality.Without_crossing = struct
  type actually_local = Modes.At_locality.actually_local

  type global = Modes.At_locality.global
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type local = Modes.At_locality.local
  [@@deriving compare ~localize, equal ~localize, hash, sexp_of, sexp_grammar]

  type (+'a, 'locality) t =
    | Local : 'a -> ('a, local) t
    | Global : global_ 'a -> ('a, _) t

  let wrap a = Global a
  let unwrap (type locality) (Local a | Global a : (_, locality) t) = a
  let wrap_local a = exclave_ Local a
  let unwrap_local (type locality) (Local a | Global a : (_, locality) t @ local) = a
  let unwrap_global (Global a : (_, global) t) = a
  let to_local t = exclave_ wrap_local (unwrap_local t)
  let to_global t = wrap (unwrap t)
  let globalize globalize_a _ t = wrap (globalize_a (unwrap_local t))
  let globalize_global t = wrap (unwrap_global t)
  let equal equal_a _ x y = equal_a (unwrap x) (unwrap y)
  let compare compare_a _ x y = compare_a (unwrap x) (unwrap y)
  let equal__local equal_a _ x y = equal_a (unwrap_local x) (unwrap_local y) [@nontail]

  let compare__local compare_a _ x y =
    compare_a (unwrap_local x) (unwrap_local y) [@nontail]
  ;;

  let hash_fold_t hash_fold_a _ state t = hash_fold_a state (unwrap t)
  let sexp_of_t sexp_of_a _ t = sexp_of_a (unwrap t)

  let t_sexp_grammar
    : type a. a Sexplib0.Sexp_grammar.t -> _ -> (a, _) t Sexplib0.Sexp_grammar.t
    =
    fun grammar _ -> Sexplib0.Sexp_grammar.coerce grammar
  ;;
end

module _ = struct
  (* witness that subtyping works: *)
  let _ =
    fun (type a) (t : (a, Modes.At_locality.global) Modes.At_locality.t) ->
    (t :> (a, Modes.At_locality.local) Modes.At_locality.t)
  ;;
end

module M : sig
  type t

  val return_at_max : string -> t @ global many portable uncontended
end = struct
  (* Demonstrate that [failwithf] can be used to produce a value at the maximum mode of
     all mode axes (that we remembered to list here). *)

  type t

  let return_at_max : string -> t @ global many portable uncontended (* unique *) =
    fun message -> Printf.failwithf "My message: %s" message ()
  ;;
end

let _ : _ = M.return_at_max
