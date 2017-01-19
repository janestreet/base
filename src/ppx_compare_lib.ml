open Import0

let phys_equal = phys_equal
external polymorphic_compare : 'a -> 'a -> int  = "%compare"

let compare_abstract ~type_name _ _ =
  Printf.ksprintf failwith
    "Compare called on the type %s, which is abtract in an implementation."
    type_name

type 'a compare = 'a -> 'a -> int

module Builtin = struct
  type 'a t = 'a compare

  let compare_bool      : bool      t = Poly.compare
  let compare_char      : char      t = Poly.compare
  let compare_float     : float     t = Poly.compare
  let compare_int       : int       t = Poly.compare
  let compare_int32     : int32     t = Poly.compare
  let compare_int64     : int64     t = Poly.compare
  let compare_nativeint : nativeint t = Poly.compare
  let compare_string    : string    t = Poly.compare
  let compare_unit      : unit      t = Poly.compare

  let compare_array compare_elt a b =
    if phys_equal a b then
      0
    else
      let len_a = Array0.length a in
      let len_b = Array0.length b in
      let ret = compare len_a len_b in
      if ret <> 0 then ret
      else
        let rec loop i =
          if i = len_a then
            0
          else
            let l = Array0.unsafe_get a i
            and r = Array0.unsafe_get b i in
            let res = compare_elt l r in
            if res <> 0 then res
            else loop (i + 1)
        in
        loop 0

  let rec compare_list compare_elt a b =
    match a, b with
    | [] , [] -> 0
    | [] , _  -> -1
    | _  , [] -> 1
    | x::xs, y::ys ->
      let res = compare_elt x y in
      if res <> 0 then res
      else compare_list compare_elt xs ys

  let compare_option compare_elt a b =
    match a, b with
    | None  , None   -> 0
    | None  , Some _ -> -1
    | Some _, None   -> 1
    | Some a, Some b -> compare_elt a b

  let compare_ref compare_elt a b = compare_elt !a !b
end
