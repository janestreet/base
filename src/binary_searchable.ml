open! Import
include Binary_searchable_intf.Definitions

[@@@warning "-incompatible-with-upstream"]

module type%template Arg = sig
  type ('a : k) elt
  type ('a : k) t

  val get : 'a t -> int -> 'a elt
  val length : _ t -> int
end
[@@kind k = (value, immediate, immediate64)]

module%template.portable
  [@kind k = (value, immediate, immediate64)] Make_gen
    (T : Arg
  [@kind k]) =
struct
  let get = T.get
  let length = T.length

  let binary_search ?pos ?len t ~compare how v = exclave_
    Binary_search.binary_search ?pos ?len t ~get ~length ~compare how v
  ;;

  let binary_search_segmented ?pos ?len t ~segment_of how = exclave_
    Binary_search.binary_search_segmented ?pos ?len t ~get ~length ~segment_of how
  ;;
end

module%template.portable [@modality p] Make (T : Indexable) =
Make_gen [@modality p] (struct
    include T

    type 'a elt = T.elt
    type 'a t = T.t
  end)

module%template.portable
  [@kind k = (value, immediate, immediate64)] [@modality p] Make1
    (T : Indexable1
  [@kind k]) =
Make_gen [@kind k] [@modality p] (struct
    type ('a : k) elt = 'a
    type ('a : k) t = 'a T.t

    let get = T.get
    let length = T.length
  end)
