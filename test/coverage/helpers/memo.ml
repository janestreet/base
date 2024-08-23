open! Base
open Base_quickcheck
open Expect_test_helpers_base
include Memo_intf.Definitions

let memoize generator =
  Lazy.from_fun (fun () ->
    let queue = Queue.create () in
    Test.with_sample_exn generator ~f:(fun sequence ->
      Sequence.iter sequence ~f:(Queue.enqueue queue));
    Queue.to_list queue)
;;

let quickcheck_m
  (type a)
  ~(here : [%call_pos])
  ?cr
  (module M : Memoized with type t = a)
  ~f
  =
  quickcheck_m
    ~here
    ?cr
    (module struct
      type t = M.t [@@deriving quickcheck ~shrinker, sexp_of]

      let quickcheck_generator = Generator.create (fun ~size:_ ~random:_ -> assert false)
    end)
    ~examples:(Lazy.force M.sample)
    ~config:{ Test.default_config with test_count = 0 }
    ~f
;;
