open! Import

module Global = struct
  include Modes_intf.Definitions.Global

  type 'a t = { global_ global : 'a } [@@unboxed]

  let compare__local compare a b = compare a.global b.global
  let compare compare a b = compare__local compare a b
  let equal__local equal a b = equal a.global b.global
  let equal equal a b = equal__local equal a b
  let hash_fold_t hash state t = hash state t.global
  let t_of_sexp of_sexp sexp = { global = of_sexp sexp }
  let sexp_of_t sexp_of t = sexp_of t.global

  let t_sexp_grammar : 'a. 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t =
    Sexplib0.Sexp_grammar.coerce
  ;;

  let globalize _ { global } = { global }
  let wrap global = { global }
  let unwrap { global } = global
  let map { global = x } ~f = { global = f x }

  external wrap_list : 'a list -> 'a t list = "%identity"
  external unwrap_list : ('a t list[@local_opt]) -> ('a list[@local_opt]) = "%identity"
  external wrap_option : 'a option -> 'a t option = "%identity"

  external unwrap_option
    :  ('a t option[@local_opt])
    -> ('a option[@local_opt])
    = "%identity"

  external wrap_either : ('a, 'b) Either.t -> ('a t, 'b t) Either.t = "%identity"

  external unwrap_either
    :  (('a t, 'b t) Either.t[@local_opt])
    -> (('a, 'b) Either.t[@local_opt])
    = "%identity"

  external wrap_first : ('a, 'b) Either.t -> ('a t, 'b) Either.t = "%identity"

  external unwrap_first
    :  (('a t, 'b) Either.t[@local_opt])
    -> (('a, 'b) Either.t[@local_opt])
    = "%identity"

  external wrap_second : ('a, 'b) Either.t -> ('a, 'b t) Either.t = "%identity"

  external unwrap_second
    :  (('a, 'b t) Either.t[@local_opt])
    -> (('a, 'b) Either.t[@local_opt])
    = "%identity"

  external wrap_result : ('a, 'b) Result.t -> ('a t, 'b t) Result.t = "%identity"

  external unwrap_result
    :  (('a t, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external wrap_ok : ('a, 'b) Result.t -> ('a t, 'b) Result.t = "%identity"

  external unwrap_ok
    :  (('a t, 'b) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  external wrap_error : ('a, 'b) Result.t -> ('a, 'b t) Result.t = "%identity"

  external unwrap_error
    :  (('a, 'b t) Result.t[@local_opt])
    -> (('a, 'b) Result.t[@local_opt])
    = "%identity"

  module Global_wrapper = struct
    type nonrec 'a t = 'a t

    let wrap = wrap
    let unwrap = unwrap
  end

  module Local_wrapper = struct
    type 'a t = 'a

    let wrap = Fn.id
    let unwrap = Fn.id
  end

  module Poly_fn1 (Input : T) (Output : T) (F : Wrapped_fn1(Input)(Output).S) :
    Poly_fn1 with type input := Input.t and type output := Output.t = struct
    module Global = F (Global_wrapper)
    module Local = F (Local_wrapper)

    let fn_global a = unwrap (Global.fn (wrap a)) [@nontail]
    let fn_local = Local.fn
  end

  module Poly_fn2
      (Input1 : T)
      (Input2 : T)
      (Output : T)
      (F : Wrapped_fn2(Input1)(Input2)(Output).S) :
    Poly_fn2
    with type input1 := Input1.t
     and type input2 := Input2.t
     and type output := Output.t = struct
    module Global = F (Global_wrapper)
    module Local = F (Local_wrapper)

    let fn_global a b = unwrap (Global.fn (wrap a) (wrap b)) [@nontail]
    let fn_local = Local.fn
  end

  module Poly_fn3
      (Input1 : T)
      (Input2 : T)
      (Input3 : T)
      (Output : T)
      (F : Wrapped_fn3(Input1)(Input2)(Input3)(Output).S) :
    Poly_fn3
    with type input1 := Input1.t
     and type input2 := Input2.t
     and type input3 := Input3.t
     and type output := Output.t = struct
    module Global = F (Global_wrapper)
    module Local = F (Local_wrapper)

    let fn_global a b c = unwrap (Global.fn (wrap a) (wrap b) (wrap c)) [@nontail]
    let fn_local = Local.fn
  end
end

module Export = struct
  type 'a _global = 'a Global.t = { global_ global : 'a } [@@unboxed]
end
