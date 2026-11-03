open! Import0
module Bytes = Bytes0
module Ordered_collection_common = Ordered_collection_common0
include Blit_intf.Definitions

let possibly_unsafe_bytes_to_string b =
  Bytes.unsafe_to_string ~no_mutation_while_string_reachable:b
;;

module Make1_phantom2_distinct
    (Src : sig
       type ('elt, 'p1, 'p2) t

       val length : 'elt 'p1 'p2. ('elt, 'p1, 'p2) t -> int
     end)
    (Dst : sig
       type ('elt, 'p1, 'p2) t

       val length : 'elt 'p1 'p2. ('elt, 'p1, 'p2) t -> int

       val create_like
         : 'elt 'p1 'p2 'p3 'p4.
         len:int -> ('elt, 'p1, 'p2) Src.t -> ('elt, 'p3, 'p4) t

       val unsafe_blit
         : 'elt 'p1 'p2 'p3 'p4.
         (('elt, 'p1, 'p2) Src.t, ('elt, 'p3, 'p4) t) blit
     end) :
  S1_phantom2_distinct
  with type ('elt, 'p1, 'p2) src := ('elt, 'p1, 'p2) Src.t
  with type ('elt, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Dst.t = struct
  let unsafe_blit = Dst.unsafe_blit

  let blit (type a) ~(src : (a, _, _) Src.t) ~src_pos ~dst ~dst_pos ~len =
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
     an element of the source array. Thus every element of [dst] below is well defined. *)
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

module Make1 (Sequence : Sequence1) = struct
  module Seq = struct
    include Sequence

    type ('a, _, _) t = 'a Sequence.t
  end

  include Make1_phantom2_distinct (Seq) (Seq)
end

module Make1_zero_alloc (Sequence : Sequence1_zero_alloc) = struct
  let[@inline] unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    Sequence.unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;

  let blit (type a) ~(src : a Sequence.t) ~src_pos ~dst ~dst_pos ~len =
    Ordered_collection_common.check_pos_len_exn
      ~pos:src_pos
      ~len
      ~total_length:(Sequence.length src);
    Ordered_collection_common.check_pos_len_exn
      ~pos:dst_pos
      ~len
      ~total_length:(Sequence.length dst);
    if len > 0 then unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;

  let blito
    ~src
    ?(src_pos = 0)
    ?(src_len = Sequence.length src - src_pos)
    ~dst
    ?(dst_pos = 0)
    ()
    =
    blit ~src ~src_pos ~len:src_len ~dst ~dst_pos
  ;;

  (* [sub] and [subo] ensure that every position of the created sequence is populated by
     an element of the source array. Thus every element of [dst] below is well defined. *)
  let sub src ~pos ~len =
    Ordered_collection_common.check_pos_len_exn
      ~pos
      ~len
      ~total_length:(Sequence.length src);
    let dst = Sequence.create_like ~len src in
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
         | None -> Sequence.length src - pos)
  ;;
end

module Make (Sequence : sig
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

  include Make1_phantom2_distinct (Sequence) (Sequence)
end

module Make_distinct
    (Src : Sequence)
    (Dst : sig
       include Sequence

       val create : len:int -> t
       val unsafe_blit : (Src.t, t) blit
     end) =
  Make1_phantom2_distinct
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

module Make_to_string
    (T : sig
       type t
     end)
    (To_bytes : S_distinct with type src := T.t with type dst := bytes) =
struct
  open To_bytes

  let sub src ~pos ~len = possibly_unsafe_bytes_to_string (sub src ~pos ~len)
  let subo ?pos ?len src = possibly_unsafe_bytes_to_string (subo ?pos ?len src)
end
