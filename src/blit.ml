open! Import
include Blit_intf.Definitions

let%template[@mode l = (global, local), u = aliased] possibly_unsafe_bytes_to_string b =
  Bytes0.unsafe_to_string ~no_mutation_while_string_reachable:b [@exclave_if_local l]
;;

let%template[@mode l = (global, local), u = unique] possibly_unsafe_bytes_to_string b =
  Bytes0.unique_to_string b [@exclave_if_local l]
;;

[%%template
[@@@mode.default u = (aliased, unique), v = (read_write, read, immutable)]
[@@@alloc.default a @ l = (heap @ global, stack @ local)]

module%template.portable Make1_phantom2_distinct
    (Src : sig
       type ('elt : k, 'p1, 'p2) t

       val length : ('elt : k) 'p1 'p2. ('elt, 'p1, 'p2) t @ local v -> int
     end)
    (Dst : sig
       type ('elt : k, 'p1, 'p2) t

       val length : ('elt : k) 'p1 'p2. ('elt, 'p1, 'p2) t @ local -> int

       val create_like
         : ('elt : k) 'p1 'p2 'p3 'p4.
         len:int -> ('elt, 'p1, 'p2) Src.t @ local v -> ('elt, 'p3, 'p4) t @ l u
       [@@alloc a @ l = (heap @ global, a @ l)]

       val unsafe_blit
         : ('elt : k) 'p1 'p2 'p3 'p4.
         ((('elt, 'p1, 'p2) Src.t, ('elt, 'p3, 'p4) t) blit[@mode v])
     end) :
  S1_phantom2_distinct
  [@kind.explicit k] [@mode u v] [@alloc a]
  with type ('elt : k, 'p1, 'p2) src := ('elt, 'p1, 'p2) Src.t
  with type ('elt : k, 'p1, 'p2) dst := ('elt, 'p1, 'p2) Dst.t = struct
  let unsafe_blit = Dst.unsafe_blit

  let blit (type a : k) ~(src : (a, _, _) Src.t) ~src_pos ~dst ~dst_pos ~len =
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

  [@@@alloc.default a = (heap, a)]

  (* [sub] and [subo] ensure that every position of the created sequence is populated by
     an element of the source array. Thus every element of [dst] below is well defined. *)
  let sub src ~pos ~len =
    (Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length:(Src.length src);
     let dst = (Dst.create_like [@alloc a]) ~len src in
     if len > 0 then unsafe_blit ~src ~src_pos:pos ~dst:(borrow_ dst) ~dst_pos:0 ~len;
     dst)
    [@exclave_if_stack a]
  ;;

  let subo ?(pos = 0) ?len src =
    (sub [@alloc a])
      src
      ~pos
      ~len:
        (match len with
         | Some i -> i
         | None -> Src.length src - pos) [@exclave_if_stack a]
  ;;
end
[@@kind.explicit_plus_unmangled
  k
  = ( value
    , value mod external64
    , value_or_null
    , value_or_null mod separable
    , value_or_null mod external64 non_float )]

module%template.portable
  [@modality p] Make1
    (Sequence : Sequence1
  [@kind.explicit k] [@mode u v] [@alloc a]) =
struct
  module Seq = struct
    include Sequence

    type ('a : k, _, _) t = 'a Sequence.t
  end

  include
    Make1_phantom2_distinct [@kind.explicit k] [@modality p] [@mode u v] [@alloc a]
      (Seq)
      (Seq)
end
[@@kind.explicit_plus_unmangled
  k
  = ( value
    , value mod external64
    , value_or_null
    , value_or_null mod separable
    , value_or_null mod external64 non_float )]

module%template.portable
  [@modality p] Make1_zero_alloc
    (Sequence : Sequence1_zero_alloc
  [@kind.explicit k] [@mode u v] [@alloc a]) =
struct
  let[@inline] unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
    Sequence.unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len
  ;;

  let blit (type a : k) ~(src : a Sequence.t) ~src_pos ~dst ~dst_pos ~len =
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

  [@@@alloc.default a = (heap, a)]

  (* [sub] and [subo] ensure that every position of the created sequence is populated by
     an element of the source array. Thus every element of [dst] below is well defined. *)
  let sub src ~pos ~len =
    (Ordered_collection_common.check_pos_len_exn
       ~pos
       ~len
       ~total_length:(Sequence.length src);
     let dst = (Sequence.create_like [@alloc a]) ~len src in
     if len > 0 then unsafe_blit ~src ~src_pos:pos ~dst:(borrow_ dst) ~dst_pos:0 ~len;
     dst)
    [@exclave_if_stack a]
  ;;

  let subo ?(pos = 0) ?len src =
    (sub [@alloc a])
      src
      ~pos
      ~len:
        (match len with
         | Some i -> i
         | None -> Sequence.length src - pos) [@exclave_if_stack a]
  ;;
end
[@@kind.explicit_plus_unmangled
  k
  = ( value
    , value mod external64
    , value_or_null
    , value_or_null mod separable
    , value_or_null mod external64 non_float )]

module%template.portable
  [@modality p] Make (Sequence : sig
    include Sequence [@mode v]

    val create : len:int -> t @ l u [@@alloc a @ l = (heap @ global, a @ l)]
    val unsafe_blit : ((t, t) blit[@mode v])
  end) =
struct
  module Sequence = struct
    type (_, _, _) t = Sequence.t

    open Sequence

    let create_like ~len _ = (create [@alloc a]) ~len [@exclave_if_stack a]
    [@@alloc a = (heap, a)]
    ;;

    let length = length
    let unsafe_blit = unsafe_blit
  end

  include
    Make1_phantom2_distinct [@modality p] [@mode u v] [@alloc a] (Sequence) (Sequence)
end

module%template.portable
  [@modality p] Make_distinct
    (Src : Sequence
  [@mode v])
    (Dst : sig
       include Sequence

       val create : len:int -> t @ l u [@@alloc a @ l = (heap @ global, a @ l)]
       val unsafe_blit : ((Src.t, t) blit[@mode v])
     end) =
  Make1_phantom2_distinct [@modality p] [@mode u v] [@alloc a]
    (struct
      type (_, _, _) t = Src.t

      open Src

      let length = length
    end)
    (struct
      type (_, _, _) t = Dst.t

      open Dst

      let length = length

      let create_like ~len _ = (create [@alloc a]) ~len [@exclave_if_stack a]
      [@@alloc a = (heap, a)]
      ;;

      let unsafe_blit = unsafe_blit
    end)

module%template.portable Make_to_string
    (T : sig
       type t
     end)
    (To_bytes : S_distinct
                [@mode u v] [@alloc a]
                with type src := T.t
                with type dst := bytes) =
struct
  open To_bytes

  [@@@alloc.default a @ l = (heap @ global, a @ l)]

  let sub src ~pos ~len =
    (possibly_unsafe_bytes_to_string [@mode l u])
      ((sub [@alloc a]) src ~pos ~len) [@exclave_if_stack a]
  ;;

  let subo ?pos ?len src =
    (possibly_unsafe_bytes_to_string [@mode l u])
      ((subo [@alloc a]) ?pos ?len src) [@exclave_if_stack a]
  ;;
end]
