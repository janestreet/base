open! Import
include Blit_intf.Definitions

[@@@warning "-incompatible-with-upstream"]

module%template.portable Make1_phantom2_distinct
    (Src : sig
       type ('elt : k, 'p1, 'p2) t

       val length : local_ (_, _, _) t -> int
     end)
    (Dst : sig
       type ('elt : k, 'p1, 'p2) t

       val length : local_ (_, _, _) t -> int
       val create_like : len:int -> local_ ('elt, _, _) Src.t -> ('elt, _, _) t
       val unsafe_blit : (('elt, _, _) Src.t, ('elt, _, _) t) blit
     end) :
  S1_phantom2_distinct
  [@kind k]
  with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Src.t
  with type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Dst.t = struct
  let unsafe_blit = Dst.unsafe_blit

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    Ordered_collection_common.check_pos_len_exn
      ~pos:src_pos
      ~len
      ~total_length:(Src.length src);
    Ordered_collection_common.check_pos_len_exn
      ~pos:dst_pos
      ~len
      ~total_length:(Dst.length dst);
    if len > 0 then unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;

  let blito
    ~src
    ?(src_pos = 0)
    ?(src_len = Src.length src - src_pos)
    ~dst
    ?(dst_pos = 0)
    ()
    =
    blit ~src ~src_pos ~len:src_len ~dst ~dst_pos
  ;;

  (* [sub] and [subo] ensure that every position of the created sequence is populated by
     an element of the source array.  Thus every element of [dst] below is well
     defined. *)
  let sub src ~pos ~len =
    Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length:(Src.length src);
    let dst = Dst.create_like ~len src in
    if len > 0 then unsafe_blit ~src ~src_pos:pos ~dst ~dst_pos:0 ~len;
    dst
  ;;

  let subo ?(pos = 0) ?len src =
    sub
      src
      ~pos
      ~len:
        (match len with
         | Some i -> i
         | None -> Src.length src - pos)
  ;;
end
[@@kind k = (value, immediate, immediate64)]

module%template.portable [@modality p] Make1 (Sequence : Sequence1 [@kind k]) = struct
  module Seq = struct
    include Sequence

    type ('a : k, _, _) t = 'a Sequence.t
  end

  include Make1_phantom2_distinct [@kind k] [@modality p] (Seq) (Seq)
end
[@@kind k = (value, immediate, immediate64)]

module%template.portable
  [@modality p] Make (Sequence : sig
    include Sequence

    val create : len:int -> t
    val unsafe_blit : (t, t) blit
  end) =
struct
  module Sequence = struct
    type (_, _, _) t = Sequence.t

    open Sequence

    let create_like ~len _ = create ~len
    let length = length
    let unsafe_blit = unsafe_blit
  end

  include Make1_phantom2_distinct [@modality p] (Sequence) (Sequence)
end

module%template.portable
  [@modality p] Make_distinct
    (Src : Sequence)
    (Dst : sig
       include Sequence

       val create : len:int -> t
       val unsafe_blit : (Src.t, t) blit
     end) =
  Make1_phantom2_distinct [@modality p]
    (struct
      type (_, _, _) t = Src.t

      open Src

      let length = length
    end)
    (struct
      type (_, _, _) t = Dst.t

      open Dst

      let length = length
      let create_like ~len _ = create ~len
      let unsafe_blit = unsafe_blit
    end)

module%template.portable Make_to_string
    (T : sig
       type t
     end)
    (To_bytes : S_distinct with type src := T.t with type dst := bytes) =
struct
  open To_bytes

  let sub src ~pos ~len =
    Bytes0.unsafe_to_string ~no_mutation_while_string_reachable:(sub src ~pos ~len)
  ;;

  let subo ?pos ?len src =
    Bytes0.unsafe_to_string ~no_mutation_while_string_reachable:(subo ?pos ?len src)
  ;;
end
