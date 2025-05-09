open! Import
include Capsule_intf.Definitions
module Expert = Basement.Capsule

module Password = struct
  type 'k t : value mod contended portable = 'k Expert.Password.t
end

module Data = struct
  type ('a, 'k) t : value mod contended portable = ('a, 'k) Expert.Data.t

  let create = Expert.Data.create
  let return = Expert.Data.inject
  let get_id = Expert.Data.project
  let get_id_contended = Expert.Data.project
  let both = Expert.Data.both
  let fst = Expert.Data.fst
  let snd = Expert.Data.snd
  let[@inline] map t ~f ~password = Expert.Data.map t ~password ~f
  let[@inline] get t ~f ~password = Expert.Data.extract t ~f ~password
  let[@inline] get_contended t ~f ~password = Expert.Data.extract t ~password ~f
  let[@inline] bind t ~f ~password = Expert.Data.bind t ~password ~f
  let[@inline] iter t ~f ~password = Expert.Data.iter t ~password ~f
end

module Mutex = struct
  type 'k t : value mod contended portable = 'k Expert.Mutex.t

  type packed : value mod contended portable = Expert.Mutex.packed = P : 'k t -> packed
  [@@unboxed] [@@unsafe_allow_any_mode_crossing]

  let create () =
    let (P (type k) (key : k Expert.Key.t)) = Expert.create () in
    P (Expert.Mutex.create key)
  ;;

  let create_m () : (module With_mutex) =
    let (P (type k) (t : k t)) = create () in
    (module struct
      type nonrec k = k

      let mutex = t
    end)
  ;;

  module Create () = (val create_m ())

  let with_lock = Expert.Mutex.with_lock
end

module Isolated = struct
  type ('a, 'k) inner : value mod contended portable =
    { key : 'k Expert.Key.t
    ; data : ('a, 'k) Data.t @@ aliased
    }

  type 'a t : value mod contended portable = P : ('a, 'k) inner -> 'a t [@@unboxed]

  let create f =
    let (P key) = Expert.create () in
    let data = Data.create f in
    P { key; data }
  ;;

  let with_unique_gen (P { key; data }) ~f =
    let result, key =
      Expert.Key.access key ~f:(fun access -> f (Expert.Data.unwrap ~access data))
    in
    P { key; data }, result
  ;;

  let with_unique t ~f =
    with_unique_gen t ~f:(fun x -> { Modes.Aliased.aliased = f x }) [@nontail]
  ;;

  let with_shared_gen (P { key; data }) ~f =
    Expert.Key.access_shared key ~f:(fun access ->
      f (Expert.Data.unwrap_shared ~access data))
    [@nontail]
  ;;

  let with_shared = with_shared_gen
end
