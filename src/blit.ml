open! Import

module List = Base_list
module Array = StdLabels.Array

let _log s a sexp_of_a =
  Printf.eprintf "%s\n%!" (Sexp.to_string_hum ([%sexp_of: string * a] (s, a)));
;;

let ok_exn  = Or_error.ok_exn
let raise_s = Error.raise_s

include Blit_intf

module type Sequence_gen = sig
  type 'a elt
  type 'a t [@@deriving sexp_of]
  val length : _ t -> int
  type 'a z
  val create_bool : len:int -> bool z t
  val get : 'a z t -> int -> 'a elt
  val set : 'a z t -> int -> 'a elt -> unit
end

module Make_gen
    (Elt : sig
       type 'a t
       val equal : bool t -> bool t -> bool
       val of_bool : bool -> bool t
     end)
    (Src : Sequence_gen with type 'a elt := 'a Elt.t)
    (Dst : sig
       include Sequence_gen
         with type 'a elt := 'a Elt.t
         with type 'a z := 'a Src.z
       val create_like : len:int -> 'a Src.t -> 'a t
       val unsafe_blit : ('a Src.t, 'a t) blit
       val overlapping_src_dst
         :  [ `Do_not_check
            | `Check of ('a Src.t -> 'a t)
            ]
     end) = struct

  let unsafe_blit = Dst.unsafe_blit

  let blit ~src ~src_pos ~dst ~dst_pos ~len =
    Ordered_collection_common.check_pos_len_exn
      ~pos:src_pos ~len ~length:(Src.length src);
    Ordered_collection_common.check_pos_len_exn
      ~pos:dst_pos ~len ~length:(Dst.length dst);
    if len > 0 then unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len;
  ;;

  let blito
        ~src ?(src_pos = 0) ?(src_len = Src.length src - src_pos) ~dst ?(dst_pos = 0)
        () =
    blit ~src ~src_pos ~len:src_len ~dst ~dst_pos;
  ;;

  (* [sub] and [subo] ensure that every position of the created sequence is populated by
     an element of the source array.  Thus every element of [dst] below is well
     defined. *)
  let sub src ~pos ~len =
    Ordered_collection_common.check_pos_len_exn ~pos ~len ~length:(Src.length src);
    let dst = Dst.create_like ~len src in
    if len > 0 then unsafe_blit ~src ~src_pos:pos ~dst ~dst_pos:0 ~len;
    dst
  ;;

  let subo ?(pos = 0) ?len src =
    sub src ~pos ~len:(match len with Some i -> i | None -> Src.length src - pos)
  ;;

  let init ~len ~create ~set ~f =
    let t = create ~len in
    for i = 0 to len - 1 do
      set t i (f i);
    done;
    t
  ;;

  (* Test [blit]. *)
  let%test_unit _ =
    let elt1 = Elt.of_bool true in
    let elt2 = Elt.of_bool false in
    assert (not (Elt.equal elt1 elt2));
    let src_bit i = if i land 0x1 = 0 then elt1 else elt2 in
    let dst_bit i = if i land 0x1 = 0 then elt2 else elt1 in
    let n = 4 in
    for src_length = 0 to n do
      for dst_length = 0 to n do
        for src_pos = 0 to src_length do
          for dst_pos = 0 to dst_length do
            for src_len = 0 to min (src_length - src_pos) (dst_length - dst_pos) do
              try
                let is_in_range i = i >= dst_pos && i < dst_pos + src_len in
                let check length get =
                  fun name sequence ~expect ->
                    for i = 0 to length sequence - 1 do
                      if not (Elt.equal (get sequence i) (expect i)) then
                        raise_s [%message "bug" (name : string) (i : int)]
                    done;
                in
                let check_src = check Src.length Src.get in
                let check_dst = check Dst.length Dst.get in
                let src =
                  init ~len:src_length ~create:Src.create_bool ~set:Src.set ~f:src_bit
                in
                assert (Src.length src = src_length);
                let dst =
                  init ~len:dst_length ~create:Dst.create_bool ~set:Dst.set ~f:dst_bit
                in
                assert (Dst.length dst = dst_length);
                let init_src () =
                  for i = 0 to src_length - 1 do
                    Src.set src i (src_bit i);
                  done
                in
                blito ~src ~src_pos ~src_len ~dst ~dst_pos ();
                check_src "blit src" src ~expect:src_bit;
                check_dst "blit dst" dst ~expect:(fun i ->
                  if is_in_range i
                  then src_bit (src_pos + i - dst_pos)
                  else dst_bit i);
                begin match Dst.overlapping_src_dst with
                | `Do_not_check -> ()
                | `Check src_to_dst ->
                  if dst_pos + src_len <= src_length then begin
                    init_src ();
                    let dst = src_to_dst src in
                    if false then begin
                      blito ~src ~src_pos ~src_len ~dst ~dst_pos ();
                      check_dst "blit dst overlapping" dst ~expect:(fun i ->
                        src_bit (if is_in_range i then (src_pos + i - dst_pos) else i));
                    end;
                  end;
                end;
                (* Check [sub]. *)
                init_src ();
                let dst = sub src ~pos:src_pos ~len:src_len in
                check_src "sub src" src ~expect:src_bit;
                check_dst "sub dst" dst ~expect:(fun i -> src_bit (src_pos + i));
              with exn ->
                raise_s [%message
                  "bug"
                    (exn : exn)
                    (src_length : int) (src_pos : int)
                    (dst_length : int) (dst_pos : int)]
            done;
          done;
        done;
      done;
    done
  ;;

end

module Make1
    (Sequence : sig
       include Sequence_gen with type 'a elt := 'a poly
       val create_like : len:int -> 'a t -> 'a t
       val unsafe_blit : ('a t, 'a t) blit
     end) =
  Make_gen
    (struct
      type 'a t = 'a
      let equal = (=)
      let of_bool = Fn.id
    end)
    (Sequence)
    (struct
      include Sequence
      let overlapping_src_dst = `Check Fn.id
    end)

module Make1_generic
    (Elt : Elt1)
    (Sequence : Sequence1 with type 'a elt := 'a Elt.t) =
  Make_gen
    (Elt)
    (Sequence)
    (struct
      include Sequence
      let overlapping_src_dst = `Check Fn.id
    end)

module Elt_to_elt1 (Elt : Elt) = struct
  type 'a t = Elt.t
  let equal = Elt.equal
  let of_bool = Elt.of_bool
end

module Make
    (Elt : Elt)
    (Sequence : sig
       include Sequence with type elt := Elt.t
       val unsafe_blit : (t, t) blit
     end) = struct
  module Sequence = struct
    type 'a t = Sequence.t [@@deriving sexp_of]
    type 'a z = unit
    open Sequence
    let create_like ~len _ = create ~len
    let length = length
    let get = get
    let set = set
    let unsafe_blit = unsafe_blit
    let create_bool = create
    let overlapping_src_dst = `Check Fn.id
  end
  include Make_gen (Elt_to_elt1 (Elt)) (Sequence) (Sequence)
end

module Make_distinct
    (Elt : Elt)
    (Src : Sequence with type elt := Elt.t)
    (Dst : sig
       include Sequence with type elt := Elt.t
       val unsafe_blit : (Src.t, t) blit
     end) =
  Make_gen
    (Elt_to_elt1 (Elt))
    (struct
      type 'a t = Src.t [@@deriving sexp_of]
      type 'a z = unit
      open Src
      let length = length
      let get = get
      let set = set
      let create_bool = create
    end)
    (struct
      type 'a t = Dst.t [@@deriving sexp_of]
      open Dst
      let length = length
      let get = get
      let set = set
      let create_bool = create
      let create_like ~len _ = create ~len
      let unsafe_blit = unsafe_blit
      let overlapping_src_dst = `Do_not_check
    end)

(* This unit test checks that when [blit] calls [unsafe_blit], the slices are valid.
   It also checks that [blit] doesn't call [unsafe_blit] when there is a range error. *)
let%test_module _ =
  (module struct

    let blit_was_called = ref false

    let slices_are_valid = ref (Ok ())

    module B =
      Make
        (struct
          type t = bool
          let equal (t1 : t) t2 = t1 = t2
          let of_bool = Fn.id
        end)
        (struct
          type t = bool array [@@deriving sexp_of]
          let create ~len = Array.make len false
          let length = Array.length
          let get = Array.get
          let set = Array.set
          let unsafe_blit ~src ~src_pos ~dst ~dst_pos ~len =
            blit_was_called := true;
            slices_are_valid :=
              Or_error.try_with (fun () ->
                assert (len >= 0);
                assert (src_pos >= 0);
                assert (src_pos + len <= Array.length src);
                assert (dst_pos >= 0);
                assert (dst_pos + len <= Array.length dst));
            Array.blit ~src ~src_pos ~dst ~dst_pos ~len;
          ;;
        end)
    ;;

    let%test_unit _ =
      let opts = [ None; Some (-1); Some 0; Some 1; Some 2 ] in
      List.iter [ 0; 1; 2 ] ~f:(fun src ->
        List.iter [ 0; 1; 2 ] ~f:(fun dst ->
          List.iter opts ~f:(fun src_pos ->
            List.iter opts ~f:(fun src_len ->
              List.iter opts ~f:(fun dst_pos ->
                try begin
                  let check f =
                    blit_was_called := false;
                    slices_are_valid := Ok ();
                    match Or_error.try_with f with
                    | Error _ -> assert (not !blit_was_called);
                    | Ok () -> ok_exn !slices_are_valid
                  in
                  check (fun () ->
                    B.blito
                      ~src:(Array.make src false) ?src_pos ?src_len
                      ~dst:(Array.make dst false) ?dst_pos
                      ());
                  check (fun () ->
                    ignore (B.subo (Array.make src false) ?pos:src_pos ?len:src_len
                            : bool array));
                end
                with exn ->
                  raise_s [%message
                    "failure"
                      (exn : exn)
                      (src : int) (src_pos : int option) (src_len : int option)
                      (dst : int) (dst_pos : int option)])))))
    ;;
  end)
