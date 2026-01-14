open! Import

type 'a t = 'a or_null [@@or_null_reexport]

external is_null : _ or_null @ immutable local -> bool @@ portable = "%is_null"

[%%template
  external unsafe_value : 'a t @ m -> 'a @ m @@ portable = "%obj_magic"
  [@@mode m = (global, local)]]

external really_unsafe_value
  :  ('a t[@local_opt])
  -> ('a[@local_opt])
  @@ portable
  = "%identity"
[@@alert unsafe_optimizations_if_misapplied]

module Optional_syntax = struct
  module Optional_syntax = struct
    let[@zero_alloc] [@inline] is_none t = is_null t

    let[@inline] unsafe_value t =
      (really_unsafe_value [@alert "-unsafe_optimizations_if_misapplied"]) t
    ;;
  end
end

include (
struct
  type 'a t = 'a or_null [@@deriving globalize, sexp ~stackify]
end :
  sig
  @@ portable
    type 'a t = 'a or_null [@@deriving globalize, sexp ~stackify]
  end
  with type 'a t := 'a or_null)

[%%rederive.portable type ('a : value mod non_null) t = 'a or_null [@@deriving hash]]

let[@cold] raise_value_exn ~here =
  let error =
    if Source_code_position.is_dummy here
    then Error.of_string "Or_null.value_exn Null"
    else Error.create "Or_null.value_exn Null" here Source_code_position0.sexp_of_t
  in
  Error.raise error
;;

[%%template
[@@@mode.default m = (global, local)]

let[@inline] compare compare_elt t1 t2 =
  match t1, t2 with
  | Null, Null -> 0
  | Null, This _ -> -1
  | This _, Null -> 1
  | This elt1, This elt2 -> compare_elt elt1 elt2
;;

let[@inline] equal equal_elt t1 t2 =
  match t1, t2 with
  | Null, Null -> true
  | Null, This _ | This _, Null -> false
  | This elt1, This elt2 -> equal_elt elt1 elt2
;;

let[@inline] value_exn ~(here : [%call_pos]) t =
  match t with
  | This x -> x
  | Null -> raise_value_exn ~here
;;

let[@inline] value_or_thunk o ~default =
  match o with
  | This x -> x
  | Null -> default () [@exclave_if_local m]
;;

let[@inline] iter o ~f =
  match o with
  | Null -> ()
  | This a -> f a
;;

let[@inline] value_map t ~default ~(local_ f) =
  match t with
  | This x -> f x [@exclave_if_local m]
  | Null -> default
;;

let[@inline] both t1 t2 =
  match #(t1, t2) with
  | #(Null, _) | #(_, Null) -> Null
  | #(This x, This y) -> This (x, y) [@exclave_if_local m]
;;

let[@inline] bind t ~(local_ f) =
  match t with
  | This x -> f x [@exclave_if_local m]
  | Null -> Null
;;

let[@inline] to_list t =
  match t with
  | Null -> []
  | This x -> [ x ] [@exclave_if_local m]
;;

let[@inline] this a = This a [@exclave_if_local m]

let[@inline] first_this_thunk t t' =
  match t with
  | This _ -> t
  | Null -> t' () [@exclave_if_local m]
;;

let[@inline] fold t ~init ~f =
  match t with
  | Null -> init
  | This x -> f init x [@exclave_if_local n]
[@@mode m = m, n = (global, local)]
;;

let[@inline] mem t a ~equal =
  match t with
  | Null -> false
  | This a' -> equal a a'
;;

let[@inline] exists t ~f =
  match t with
  | Null -> false
  | This x -> f x
;;

let[@inline] for_all t ~f =
  match t with
  | Null -> true
  | This x -> f x
;;

let[@inline] find t ~f =
  match t with
  | Null -> Null
  | This x when f x -> This x
  | This _ -> Null
;;

let[@inline] to_array = function
  | Null -> [||]
  | This x -> [| x |] [@exclave_if_local m]
;;

let[@inline] call x ~f =
  match f with
  | Null -> ()
  | This f -> f x
;;

let[@inline] to_option = function
  | Null -> None
  | This x -> Some x [@exclave_if_local m]
;;

let[@inline] of_option = function
  | None -> Null
  | Some x -> This x
;;]

let%template[@inline] map_to_option t ~f =
  match t with
  | Null -> None
  | This x -> Some (f x) [@exclave_if_stack a]
[@@alloc a = (heap, stack)]
;;

[%%template
let[@inline] [@mode local] map t ~f = exclave_
  match t with
  | Null -> Null
  | This a -> This (f a)
;;

let[@inline] [@mode global] map t ~f =
  match t with
  | Null -> Null
  | This a -> This (f a)
;;

let[@inline] [@mode local] this_if_thunk b f = exclave_
  match b with
  | true -> This (f ())
  | false -> Null
;;

let[@inline] [@mode global] this_if_thunk b f =
  match b with
  | true -> This (f ())
  | false -> Null
;;

let[@inline] [@mode local] merge a b ~f = exclave_
  match a, b with
  | Null, Null -> Null
  | Null, This x | This x, Null -> This x
  | This x, This y -> This (f x y)
;;

let[@inline] [@mode global] merge a b ~f =
  match a, b with
  | Null, Null -> Null
  | Null, This x | This x, Null -> This x
  | This x, This y -> This (f x y)
;;

let[@inline] [@mode local] try_with f = exclave_
  match f () with
  | x -> This x
  | exception _ -> Null
;;

let[@inline] [@mode global] try_with f =
  match f () with
  | x -> This x
  | exception _ -> Null
;;

let[@inline] [@mode local] try_with_join f = exclave_
  match f () with
  | x -> x
  | exception _ -> Null
;;

let[@inline] [@mode global] try_with_join f =
  match f () with
  | x -> x
  | exception _ -> Null
;;

let value t ~default = exclave_
  (really_unsafe_value [@alert "-unsafe_optimizations_if_misapplied"])
    (Bool.select (is_null t) (This default) t)
[@@inline] [@@mode local]
;;

let value t ~default =
  (really_unsafe_value [@alert "-unsafe_optimizations_if_misapplied"])
    (Bool.select (is_null t) (This default) t)
[@@inline] [@@mode global]
;;

[@@@mode.default m = (global, local)]

let[@inline] this_if b x =
  let this = This x in
  Bool.select b this Null [@exclave_if_local m]
;;

let[@inline] first_this t t' =
  let is_null = is_null t in
  Bool.select is_null t' t [@exclave_if_local m]
;;]

(* redefine [is_null] as a let bind so we can mark it [@zero_alloc]. It is not exposed as
   an external and externals exposed as vals cannot be marked [@zero_alloc]. *)
let[@inline] is_null t = is_null t
let[@inline] is_this t = not (is_null t)
let[@inline] length t = Bool.to_int (is_this t)

module Let_syntax = struct
  let return = this
  let[@inline] ( >>= ) t f = bind t ~f
  let[@inline] ( >>| ) t f = map t ~f

  module Let_syntax = struct
    let return = this
    let bind = bind
    let map = map
    let both = both

    module Open_on_rhs = struct end
  end
end

module Local = struct
  module Let_syntax = struct
    let%template return = (this [@mode local])

    module Let_syntax = struct
      let%template return = (this [@mode local])
      let%template bind = (bind [@mode local])
      let%template map = (map [@mode local])
      let%template both = (both [@mode local])

      module Open_on_rhs = struct end
    end
  end
end
