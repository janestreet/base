open! Import
module Bool = Bool0
module Error = Error0
module Source_code_position = Source_code_position0

module T = struct
  type 'a t = 'a or_null =
    | Null
    | This of 'a
end

include T

external is_null : 'a t -> bool = "%obj_is_int"
external unsafe_value : 'a t -> 'a = "%field0"
external really_unsafe_value : 'a t -> 'a = "%field0"

module Optional_syntax = struct
  module Optional_syntax = struct
    let[@zero_alloc] [@inline] is_none t = is_null t

    let[@inline] unsafe_value t =
      (really_unsafe_value [@alert "-unsafe_optimizations_if_misapplied"]) t
    ;;
  end
end

[%%rederive.portable type 'a t = 'a or_null [@@deriving globalize, sexp, hash]]

let[@cold] raise_value_exn ~here =
  let error =
    if Source_code_position.is_dummy here
    then Error.of_string "Or_null.value_exn Null"
    else Error.create "Or_null.value_exn Null" here Source_code_position.sexp_of_t
  in
  Error.raise error
;;

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

let[@inline] value_exn ?(here = Stdlib.Lexing.dummy_pos) t =
  match t with
  | This x -> x
  | Null -> raise_value_exn ~here
;;

let[@inline] value_or_thunk o ~default =
  match o with
  | This x -> x
  | Null -> default ()
;;

let[@inline] iter o ~f =
  match o with
  | Null -> ()
  | This a -> f a
;;

let[@inline] value_map t ~default ~f =
  match t with
  | This x -> f x
  | Null -> default
;;

let[@inline] both t1 t2 =
  match t1, t2 with
  | Null, _ | _, Null -> Null
  | This x, This y -> This (x, y)
;;

let[@inline] bind t ~f =
  match t with
  | This x -> f x
  | Null -> Null
;;

let[@inline] to_list t =
  match t with
  | Null -> []
  | This x -> [ x ]
;;

let[@inline] this a = This a

let[@inline] first_this_thunk t t' =
  match t with
  | This _ -> t
  | Null -> t' ()
;;

let[@inline] fold t ~init ~f =
  match t with
  | Null -> init
  | This x -> f init x
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
  | This x -> [| x |]
;;

let[@inline] call x ~f =
  match f with
  | Null -> ()
  | This f -> f x
;;

let[@inline] to_option = function
  | Null -> None
  | This x -> Some x
;;

let[@inline] of_option = function
  | None -> Null
  | Some x -> This x
;;

let[@inline] map_to_option t ~f =
  match t with
  | Null -> None
  | This x -> Some (f x)
;;

let[@inline] map t ~f =
  match t with
  | Null -> Null
  | This a -> This (f a)
;;

let[@inline] this_if_thunk b f =
  match b with
  | true -> This (f ())
  | false -> Null
;;

let[@inline] merge a b ~f =
  match a, b with
  | Null, Null -> Null
  | Null, This x | This x, Null -> This x
  | This x, This y -> This (f x y)
;;

let[@inline] try_with f =
  match f () with
  | x -> This x
  | exception _ -> Null
;;

let[@inline] try_with_join f =
  match f () with
  | x -> x
  | exception _ -> Null
;;

let value t ~default =
  match t with
  | Null -> default
  | This x -> x
[@@inline]
;;

let[@inline] this_if b x =
  let this = This x in
  Bool.select b this Null
;;

let[@inline] first_this t t' =
  let is_null = is_null t in
  Bool.select is_null t' t
;;

(* redefine [is_null] as a let bind so we can mark it [@zero_alloc]. It is not exposed as
   an external and externals exposed as vals cannot be marked [@zero_alloc]. *)
let[@inline] is_null t = is_null t
let[@inline] is_this t = not (is_null t)
let[@inline] length t = bool_to_int (is_this t)

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
    let return = this

    module Let_syntax = struct
      let return = this
      let bind = bind
      let map = map
      let both = both

      module Open_on_rhs = struct end
    end
  end
end

let[@inline] invariant invariant_a = iter ~f:invariant_a

module Export = Basement.Or_null_shim.Export
