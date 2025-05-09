open! Import
module Obj_array = Base.Exported_for_specific_uses.Obj_array

(* Invariant tests *)
module%test [@tags "no-js"] _ : module type of struct
  include Obj_array
end = struct
  type t = Obj_array.t

  let invariant = Obj_array.invariant

  (* We test that constructors satisfy the invariant, especially when given floats. *)

  open struct
    let test ?cr ?(allow_nonfloat = false) ?(allow_empty = false) ~(here : [%call_pos]) t =
      (* assertions for calling convention ... *)
      if not allow_empty then assert (Obj_array.length t > 0);
      if not allow_nonfloat
      then
        for pos = 0 to Obj_array.length t - 1 do
          assert (Stdlib.Obj.tag (Obj_array.get t pos) = Stdlib.Obj.double_tag)
        done;
      (* ... [require]* for test itself *)
      require_does_not_raise ~here ?cr (fun () -> Obj_array.invariant t)
    ;;

    let obj (float : float) = Stdlib.Obj.repr float
  end

  (* creators *)

  let empty = Obj_array.empty
  let%expect_test _ = test Obj_array.empty ~allow_empty:true
  let get_empty = Obj_array.get_empty
  let%expect_test _ = test (Obj_array.get_empty ()) ~allow_empty:true
  let singleton = Obj_array.singleton
  let%expect_test _ = test (Obj_array.singleton (obj 0.))
  let create_zero = Obj_array.create_zero
  let%expect_test _ = test (Obj_array.create_zero ~len:1) ~allow_nonfloat:true
  let create = Obj_array.create
  let%expect_test _ = test (Obj_array.create (obj 0.) ~len:1)
  let copy = Obj_array.copy
  let%expect_test _ = test (Obj_array.copy (Obj_array.singleton (obj 0.)))
  let sub = Obj_array.sub
  let%expect_test _ = test (Obj_array.sub (Obj_array.singleton (obj 0.)) ~pos:0 ~len:1)
  let subo = Obj_array.subo
  let%expect_test _ = test (Obj_array.subo (Obj_array.singleton (obj 0.)))

  (* accessors *)

  let sexp_of_t = Obj_array.sexp_of_t
  let blit = Obj_array.blit
  let blito = Obj_array.blito
  let unsafe_blit = Obj_array.unsafe_blit
  let length = Obj_array.length
  let[@zero_alloc] get t i = Obj_array.get t i
  let[@zero_alloc] unsafe_get t i = Obj_array.unsafe_get t i
  let set = Obj_array.set
  let unsafe_set = Obj_array.unsafe_set
  let swap = Obj_array.swap
  let set_with_caml_modify = Obj_array.set_with_caml_modify
  let unsafe_set_assuming_currently_int = Obj_array.unsafe_set_assuming_currently_int

  let unsafe_set_int_assuming_currently_int =
    Obj_array.unsafe_set_int_assuming_currently_int
  ;;

  let unsafe_set_int = Obj_array.unsafe_set_int
  let unsafe_set_omit_phys_equal_check = Obj_array.unsafe_set_omit_phys_equal_check
  let unsafe_set_with_caml_modify = Obj_array.unsafe_set_with_caml_modify
  let unsafe_clear_if_pointer = Obj_array.unsafe_clear_if_pointer
end
