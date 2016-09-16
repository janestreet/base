open! Import

type 'a t = 'a lazy_t [@@deriving sexp]

let map t ~f = lazy (f (Lazy.force t))
let compare compare_a t1 t2 = compare_a (Lazy.force t1) (Lazy.force t2)
let hash_fold_t = Hash.Builtin.hash_fold_lazy_t

include (Lazy : module type of Lazy with type 'a t := 'a t)

include Monad.Make (struct
  type nonrec 'a t = 'a t

  let return x = from_val x

  let bind t ~f = lazy (force (f (force t)))

  let map = map

  let map = `Custom map
end)

let%test_module _ = (module struct

  let%test_unit _ =
    let r = ref 0 in
    let t = return () >>= fun () -> incr r; return () in
    assert (!r = 0);
    force t;
    assert (!r = 1);
    force t;
    assert (!r = 1)
  ;;

  let%test_unit _ =
    let r = ref 0 in
    let t = return () >>= fun () -> lazy (incr r) in
    assert (!r = 0);
    force t;
    assert (!r = 1);
    force t;
    assert (!r = 1)
  ;;

end)

module T_unforcing = struct
  type nonrec 'a t = 'a t

  let sexp_of_t sexp_of_a t =
    if is_val t
    then sexp_of_a (force t)
    else sexp_of_string "<unforced lazy>"
  ;;
end

let%test_module _ = (module struct

  module M1 = struct
    type nonrec t = { x : int t } [@@deriving sexp_of]
  end

  module M2 = struct
    type t = { x : int T_unforcing.t } [@@deriving sexp_of]
  end

  let%test_unit _ =
    let v = lazy 42 in
    let (_ : int) =
      (* no needed, but the purpose of this test is not to test this compiler
         optimization *)
      force v
    in
    assert (is_val v);
    let t1 = { M1. x = v } in
    let t2 = { M2. x = v } in
    assert (M1.sexp_of_t t1 = M2.sexp_of_t t2)
  ;;

  let%test_unit _ =
    let t1 = { M1. x = lazy (40 + 2) } in
    let t2 = { M2. x = lazy (40 + 2) } in
    assert (M1.sexp_of_t t1 <> M2.sexp_of_t t2);
    assert (is_val t1.x);
    assert (not (is_val t2.x))
  ;;
end)
