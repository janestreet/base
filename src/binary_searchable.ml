open! Import

let polymorphic_equals = (=)
open! Int_replace_polymorphic_compare
include Binary_searchable_intf

module type Arg_without_tests = sig
  type 'a elt
  type 'a t
  val get : 'a t -> int -> 'a elt
  val length : _ t -> int
end

module type Arg = sig
  include Arg_without_tests
  module For_test : sig
    val compare  : bool elt -> bool elt -> int
    val small    : bool elt
    val big      : bool elt
    val of_array : bool elt array -> bool t
  end
end

module Make_gen_without_tests (T : Arg_without_tests) = struct
  let get = T.get
  let length = T.length

  let binary_search ?pos ?len t ~compare how v =
    Binary_search.binary_search ?pos ?len t ~get ~length ~compare how v

  let binary_search_segmented ?pos ?len t ~segment_of how =
    Binary_search.binary_search_segmented ?pos ?len t ~get ~length ~segment_of how
end

module Make_gen (T : Arg) = struct

  include Make_gen_without_tests (T)

  let%test_module "test_binary_searchable" = (module struct
    let compare = T.For_test.compare
    let elt_compare = compare

    let s = T.For_test.small
    let b = T.For_test.big

    let binary_search ?pos ?len ~compare t how v =
      binary_search ?pos ?len ~compare (T.For_test.of_array t) how v

    let (=) = polymorphic_equals

    let%test _ = binary_search ~compare [|          |]  `First_equal_to s = None
    let%test _ = binary_search ~compare [| s        |]  `First_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s        |]  `First_equal_to b = None
    let%test _ = binary_search ~compare [| s ; b    |]  `First_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s ; b    |]  `First_equal_to b = Some 1
    let%test _ = binary_search ~compare [| b ; b    |]  `First_equal_to s = None
    let%test _ = binary_search ~compare [| s ; s    |]  `First_equal_to b = None
    let%test _ = binary_search ~compare [| s ; b ; b |] `First_equal_to b = Some 1
    let%test _ = binary_search ~compare [| s ; s ; b |] `First_equal_to s = Some 0
    let%test _ = binary_search ~compare [| b ; b ; b |] `First_equal_to s = None

    let%test _ = binary_search ~compare [|          |]  `Last_equal_to s = None
    let%test _ = binary_search ~compare [| s        |]  `Last_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s        |]  `Last_equal_to b = None
    let%test _ = binary_search ~compare [| s ; b    |]  `Last_equal_to b = Some 1
    let%test _ = binary_search ~compare [| s ; b    |]  `Last_equal_to s = Some 0
    let%test _ = binary_search ~compare [| b ; b    |]  `Last_equal_to s = None
    let%test _ = binary_search ~compare [| s ; s    |]  `Last_equal_to b = None
    let%test _ = binary_search ~compare [| s ; b ; b |] `Last_equal_to b = Some 2
    let%test _ = binary_search ~compare [| s ; s ; b |] `Last_equal_to s = Some 1
    let%test _ = binary_search ~compare [| b ; b; b |]  `Last_equal_to s = None

    let%test _ = binary_search ~compare [||] `First_greater_than_or_equal_to s    = None
    let%test _ = binary_search ~compare [| b |] `First_greater_than_or_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s |] `First_greater_than_or_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s |] `First_strictly_greater_than s    = None

    let%test _ = binary_search ~compare [||] `Last_less_than_or_equal_to  s   = None
    let%test _ = binary_search ~compare [| b |] `Last_less_than_or_equal_to s = None
    let%test _ = binary_search ~compare [| s |] `Last_less_than_or_equal_to s = Some 0
    let%test _ = binary_search ~compare [| s |] `Last_strictly_less_than s = None

    let create_test_case (num_s, num_b) =
      let arr = Array.make (num_s + num_b) b in
      for i = 0 to num_s -1 do
        arr.(i) <- s
      done;
      arr
    ;;

    let only_small   = (10_000, 0)
    let only_big   =  (0, 10_000)

    let both = (2531, 4717)

    let%test _ =
      match binary_search (create_test_case only_small) ~compare `First_equal_to s with
      | None   -> false
      | Some _ -> true

    let%test _ =
      let arr = create_test_case both in
      match binary_search arr ~compare `First_equal_to b with
      | None -> false
      | Some v -> v = 2531

    let%test _ =
      let arr = create_test_case only_small in
      binary_search arr ~compare `First_equal_to b = None

    let create_deterministic_test () =
      Array.init 100_000 (fun i -> if i > 50_000 then b else s)

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_equal_to s = Some 0

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_equal_to s = Some 50_000

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_greater_than_or_equal_to s = Some 0

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_less_than_or_equal_to s = Some 50_000

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_strictly_greater_than s = Some 50_001

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_strictly_less_than b = Some 50_000

    (* tests around a gap*)
    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_equal_to b  = Some 50_001

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_equal_to b = Some 99_999

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_greater_than_or_equal_to b = Some 50_001

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_less_than_or_equal_to b = Some 99_999

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `First_strictly_greater_than b = None

    let%test _ =
      let arr = create_deterministic_test () in
      binary_search arr ~compare `Last_strictly_less_than b = Some 50_000

    (* test beginning of array *)

    let%test _ =
      let arr = create_test_case only_big in
      binary_search arr ~compare `First_equal_to s  = None

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `Last_equal_to s = None

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `First_greater_than_or_equal_to s = Some 0

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `Last_less_than_or_equal_to s = None

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `First_strictly_greater_than s = Some 0

    let%test _ =
      let arr = create_test_case only_big  in
      binary_search arr ~compare `Last_strictly_less_than b = None


    (* test end of array *)

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `First_equal_to b  = None

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `Last_equal_to b = None

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `First_greater_than_or_equal_to b = None

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `Last_less_than_or_equal_to b = Some 9_999

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `First_strictly_greater_than s = None

    let%test _ =
      let arr = create_test_case only_small  in
      binary_search arr ~compare `Last_strictly_less_than b = Some 9_999

    let%test_unit _ =
      for length = 0 to 5 do
        for num_s = 0 to length do
          let arr = Array.init length (fun i -> if i < num_s then s else b) in
          for pos = -1 to length do
            for len = -1 to length + 1 do
              (*try*)
              let should_raise =
                Exn.does_raise (fun () ->
                  Ordered_collection_common.check_pos_len_exn ~pos ~len ~length)
              in
              let result =
                Result.try_with (fun () ->
                  binary_search arr ~pos ~len ~compare:elt_compare `Last_equal_to s)
              in
              match should_raise, result with
              | true , Error _   -> ()
              | true , Ok _      -> failwith "expected it to raise but it didn't"
              | false, Error _   -> failwith "expected it to not raise, but it raised"
              | false, Ok result ->
                let searched = num_s - 1 in
                let correct_result =
                  if searched < pos then None
                  else if len = 0 then None
                  else if searched >= pos + len then Some(pos + len - 1)
                  else Some searched
                in
                if not (correct_result = result) then failwith "Wrong result"
                (*with exn ->
                  failwiths "binary_search bug"
                  (exn, `length length, `search_key search_key, `pos pos, `len len)
                  <:sexp_of< exn * [ `length of int ] * [ `search_key of int ]
                 * [ `pos of int ] * [ `len of int ] >>*)
            done;
          done;
        done;
      done
    ;;

    let binary_search_segmented a = binary_search_segmented (T.For_test.of_array a)

    (*test for binary_search_segmented*)
    let%test _ =
      let arr = create_deterministic_test () in
      let segment_of x = if x = b then `Right else `Left in
      binary_search_segmented arr ~segment_of `Last_on_left    = Some 50_000 &&
      binary_search_segmented arr ~segment_of `First_on_right  = Some 50_001

    let%test _ =
      let arr = create_deterministic_test () in
      let segment_of _ = `Right in
      binary_search_segmented arr ~segment_of `Last_on_left    = None &&
      binary_search_segmented arr ~segment_of `First_on_right  = Some 0

    let%test _ =
      let arr = create_deterministic_test () in
      let segment_of _ = `Left in
      binary_search_segmented arr ~segment_of `Last_on_left    = Some 99_999 &&
      binary_search_segmented arr ~segment_of `First_on_right  = None

  end)

end

module Make_without_tests (T : Indexable_without_tests) =
  Make_gen_without_tests (struct
    type 'a elt = T.elt
    type 'a t   = T.t
    include (T : Indexable_without_tests with type elt := T.elt with type t := T.t)
  end)

module Make (T : Indexable) =
  Make_gen (struct
    type 'a elt = T.elt
    type 'a t   = T.t
    include (T : Indexable with type elt := T.elt with type t := T.t)
  end)

module Make1_without_tests (T : Indexable1_without_tests) =
  Make_gen_without_tests (struct
    type 'a elt = 'a
    type 'a t = 'a T.t
    let get = T.get
    let length = T.length
  end)

module Make1 (T : Indexable1) =
  Make_gen (struct
    type 'a elt = 'a
    type 'a t   = 'a T.t

    let get    = T.get
    let length = T.length

    module For_test = struct
      include T.For_test

      let compare (a : bool) b = Caml.compare a b
      let small = false
      let big = true
    end
  end)
