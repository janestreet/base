open! Import
include Binary_searchable_intf.Definitions

[@@@warning "-incompatible-with-upstream"]

[%%template
[@@@mode.default m = (global, local)]
[@@@kind.default k = (value, value mod external64)]

module type Arg = sig
  type ('a : k) elt
  type ('a : k) t

  val get : 'a t @ m' -> int -> 'a elt @ m' [@@mode m' = (global, m)]
  val length : _ t @ m' -> int [@@mode m' = (global, m)]
end

module%template.portable Make_gen (T : Arg [@mode m] [@kind k]) = struct
  let[@mode m' = (global, m)] get = (T.get [@mode m'])
  let[@mode m' = (global, m)] length = (T.length [@mode m'])

  let[@mode m' = (global, m)] binary_search ?pos ?len t ~compare how v = exclave_
    (Binary_search.binary_search [@mode m'])
      ?pos
      ?len
      t
      ~get:(get [@mode m'])
      ~length:(length [@mode m'])
      ~compare
      how
      v
  ;;

  let[@mode m' = (global, m)] binary_search_segmented ?pos ?len t ~segment_of how
    = exclave_
    (Binary_search.binary_search_segmented [@mode m'])
      ?pos
      ?len
      t
      ~get:(get [@mode m'])
      ~length:(length [@mode m'])
      ~segment_of
      how
  ;;
end

module%template.portable [@modality p] Make (T : Indexable [@mode m]) =
Make_gen [@mode m] [@modality p] (struct
    include T

    type 'a elt = T.elt
    type 'a t = T.t
  end)

module%template.portable [@modality p] Make1 (T : Indexable1 [@mode m] [@kind k]) =
Make_gen [@mode m] [@kind k] [@modality p] (struct
    type ('a : k) elt = 'a
    type ('a : k) t = 'a T.t

    let[@mode m' = (global, m)] get = (T.get [@mode m'])
    let[@mode m' = (global, m)] length = (T.length [@mode m'])
  end)]
