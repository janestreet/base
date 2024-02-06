open Import0

let compare_abstract ~type_name _ _ =
  Printf.ksprintf
    failwith
    "Compare called on the type %s, which is abstract in an implementation."
    type_name
;;

let equal_abstract ~type_name _ _ =
  Printf.ksprintf
    failwith
    "Equal called on the type %s, which is abstract in an implementation."
    type_name
;;

type 'a compare = 'a -> 'a -> int
type 'a compare__local = 'a -> 'a -> int
type 'a equal = 'a -> 'a -> bool
type 'a equal__local = 'a -> 'a -> bool

module Comparable = struct
  module type S = sig
    type t

    val compare : t compare
  end

  module type S1 = sig
    type 'a t

    val compare : 'a compare -> 'a t compare
  end

  module type S2 = sig
    type ('a, 'b) t

    val compare : 'a compare -> 'b compare -> ('a, 'b) t compare
  end

  module type S3 = sig
    type ('a, 'b, 'c) t

    val compare : 'a compare -> 'b compare -> 'c compare -> ('a, 'b, 'c) t compare
  end

  module type S_local = sig
    type t

    val compare__local : t compare__local
  end

  module type S_local1 = sig
    type 'a t

    val compare__local : 'a compare__local -> 'a t compare__local
  end

  module type S_local2 = sig
    type ('a, 'b) t

    val compare__local
      :  'a compare__local
      -> 'b compare__local
      -> ('a, 'b) t compare__local
  end

  module type S_local3 = sig
    type ('a, 'b, 'c) t

    val compare__local
      :  'a compare__local
      -> 'b compare__local
      -> 'c compare__local
      -> ('a, 'b, 'c) t compare__local
  end
end

module Equal = struct
  module type S = sig
    type t

    val equal : t equal
  end

  module type S1 = sig
    type 'a t

    val equal : 'a equal -> 'a t equal
  end

  module type S2 = sig
    type ('a, 'b) t

    val equal : 'a equal -> 'b equal -> ('a, 'b) t equal
  end

  module type S3 = sig
    type ('a, 'b, 'c) t

    val equal : 'a equal -> 'b equal -> 'c equal -> ('a, 'b, 'c) t equal
  end

  module type S_local = sig
    type t

    val equal__local : t equal__local
  end

  module type S_local1 = sig
    type 'a t

    val equal__local : 'a equal__local -> 'a t equal__local
  end

  module type S_local2 = sig
    type ('a, 'b) t

    val equal__local : 'a equal__local -> 'b equal__local -> ('a, 'b) t equal__local
  end

  module type S_local3 = sig
    type ('a, 'b, 'c) t

    val equal__local
      :  'a equal__local
      -> 'b equal__local
      -> 'c equal__local
      -> ('a, 'b, 'c) t equal__local
  end
end

module Builtin = struct
  let compare_bool : bool compare = Poly.compare
  let compare_bool__local : bool compare__local = Poly.compare
  let compare_char : char compare = Poly.compare
  let compare_char__local : char compare__local = Poly.compare
  let compare_float : float compare = Poly.compare
  let compare_float__local : float compare__local = Poly.compare
  let compare_int : int compare = Poly.compare
  let compare_int__local : int compare__local = Poly.compare
  let compare_int32 : int32 compare = Poly.compare
  let compare_int32__local : int32 compare__local = Poly.compare
  let compare_int64 : int64 compare = Poly.compare
  let compare_int64__local : int64 compare__local = Poly.compare
  let compare_nativeint : nativeint compare = Poly.compare
  let compare_nativeint__local : nativeint compare__local = Poly.compare
  let compare_string : string compare = Poly.compare
  let compare_string__local : string compare__local = Poly.compare
  let compare_bytes : bytes compare = Poly.compare
  let compare_bytes__local : bytes compare__local = Poly.compare
  let compare_unit : unit compare = Poly.compare
  let compare_unit__local : unit compare__local = Poly.compare

  let compare_array__local compare_elt a b =
    if phys_equal a b
    then 0
    else (
      let len_a = Array0.length a in
      let len_b = Array0.length b in
      let ret = compare len_a len_b in
      if ret <> 0
      then ret
      else (
        let rec loop i =
          if i = len_a
          then 0
          else (
            let l = Array0.unsafe_get a i
            and r = Array0.unsafe_get b i in
            let res = compare_elt l r in
            if res <> 0 then res else loop (i + 1))
        in
        loop 0 [@nontail]))
  ;;

  let compare_array compare_elt a b = compare_array__local compare_elt a b

  let rec compare_list compare_elt a b =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let res = compare_elt x y in
      if res <> 0 then res else compare_list compare_elt xs ys
  ;;

  let rec compare_list__local compare_elt__local a b =
    match a, b with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x :: xs, y :: ys ->
      let res = compare_elt__local x y in
      if res <> 0 then res else compare_list__local compare_elt__local xs ys
  ;;

  let compare_option compare_elt a b =
    match a, b with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> compare_elt a b
  ;;

  let compare_option__local compare_elt__local a b =
    match a, b with
    | None, None -> 0
    | None, Some _ -> -1
    | Some _, None -> 1
    | Some a, Some b -> compare_elt__local a b
  ;;

  let compare_ref compare_elt a b = compare_elt !a !b
  let compare_ref__local compare_elt a b = compare_elt !a !b
  let equal_bool : bool equal = Poly.equal
  let equal_bool__local : bool equal__local = Poly.equal
  let equal_char : char equal = Poly.equal
  let equal_char__local : char equal__local = Poly.equal
  let equal_int : int equal = Poly.equal
  let equal_int__local : int equal__local = Poly.equal
  let equal_int32 : int32 equal = Poly.equal
  let equal_int32__local : int32 equal__local = Poly.equal
  let equal_int64 : int64 equal = Poly.equal
  let equal_int64__local : int64 equal__local = Poly.equal
  let equal_nativeint : nativeint equal = Poly.equal
  let equal_nativeint__local : nativeint equal__local = Poly.equal
  let equal_string : string equal = Poly.equal
  let equal_string__local : string equal__local = Poly.equal
  let equal_bytes : bytes equal = Poly.equal
  let equal_bytes__local : bytes equal__local = Poly.equal
  let equal_unit : unit equal = Poly.equal
  let equal_unit__local : unit equal__local = Poly.equal

  (* [Poly.equal] is IEEE compliant, which is not what we want here. *)
  let equal_float x y = equal_int (compare_float x y) 0
  let equal_float__local x y = equal_int (compare_float__local x y) 0

  let equal_array__local equal_elt a b =
    phys_equal a b
    ||
    let len_a = Array0.length a in
    let len_b = Array0.length b in
    equal len_a len_b
    &&
    let rec loop i =
      i = len_a
      ||
      let l = Array0.unsafe_get a i
      and r = Array0.unsafe_get b i in
      equal_elt l r && loop (i + 1)
    in
    loop 0 [@nontail]
  ;;

  let equal_array equal_elt a b = equal_array__local equal_elt a b

  let rec equal_list equal_elt a b =
    match a, b with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x :: xs, y :: ys -> equal_elt x y && equal_list equal_elt xs ys
  ;;

  let rec equal_list__local equal_elt__local a b =
    match a, b with
    | [], [] -> true
    | [], _ | _, [] -> false
    | x :: xs, y :: ys -> equal_elt__local x y && equal_list__local equal_elt__local xs ys
  ;;

  let equal_option equal_elt a b =
    match a, b with
    | None, None -> true
    | None, Some _ | Some _, None -> false
    | Some a, Some b -> equal_elt a b
  ;;

  let equal_option__local equal_elt__local a b =
    match a, b with
    | None, None -> true
    | None, Some _ | Some _, None -> false
    | Some a, Some b -> equal_elt__local a b
  ;;

  let equal_ref equal_elt a b = equal_elt !a !b
  let equal_ref__local equal_elt a b = equal_elt !a !b
end
