open! Import

let%expect_test "[Id.sexp_of_t]" =
  let id = Type_equal.Id.create ~name:"some-type-id" [%sexp_of: unit] in
  print_s [%sexp (id : _ Type_equal.Id.t)];
  [%expect {| some-type-id |}]
;;

let%test_module "Type_equal.Id" =
  (module struct
    open Type_equal.Id

    let t1 = create ~name:"t1" [%sexp_of: _]
    let t2 = create ~name:"t2" [%sexp_of: _]
    let%test _ = same t1 t1
    let%test _ = not (same t1 t2)
    let%test _ = Option.is_some (same_witness t1 t1)
    let%test _ = Option.is_none (same_witness t1 t2)
    let%test_unit _ = ignore (same_witness_exn t1 t1 : (_, _) Type_equal.t)
    let%test _ = Result.is_error (Result.try_with (fun () -> same_witness_exn t1 t2))
  end)
;;

(* This test shows that we need [conv] even though [Type_equal.T] is exposed. *)
let%test_module "Type_equal" =
  (module struct
    open Type_equal

    let id = Id.create ~name:"int" [%sexp_of: int]

    module A : sig
      type t

      val id : t Id.t
    end = struct
      type t = int

      let id = id
    end

    module B : sig
      type t

      val id : t Id.t
    end = struct
      type t = int

      let id = id
    end

    let _a_to_b (a : A.t) =
      let eq = Id.same_witness_exn A.id B.id in
      (conv eq a : B.t)
    ;;

    (* the following is rejected by the compiler *)
    (* let _a_to_b (a : A.t) =
     *   let T = Id.same_witness_exn A.id B.id in
     *   (a : B.t)
     *)

    module C = struct
      type 'a t
    end

    module Liftc = Lift (C)

    let _ac_to_bc (ac : A.t C.t) =
      let eq = Liftc.lift (Id.same_witness_exn A.id B.id) in
      (conv eq ac : B.t C.t)
    ;;
  end)
;;

let%expect_test "Create*" =
  let test id1 id2 =
    let same_according_to_id = Type_equal.Id.same id1 id2 in
    let eq = if same_according_to_id then "==" else "<>" in
    print_s [%sexp (id1 : _ Type_equal.Id.t), (eq : string), (id2 : _ Type_equal.Id.t)];
    let uid1 = Type_equal.Id.uid id1 in
    let uid2 = Type_equal.Id.uid id2 in
    let same_according_to_uid = Type_equal.Id.Uid.equal uid1 uid2 in
    if Bool.( <> ) same_according_to_id same_according_to_uid
    then
      print_cr
        [%here]
        [%message
          "[Type_equal.Id] and [Type_equal.Id.Uid] disagree"
            (id1 : _ Type_equal.Id.t)
            (id2 : _ Type_equal.Id.t)
            (uid1 : Type_equal.Id.Uid.t)
            (uid2 : Type_equal.Id.Uid.t)
            (same_according_to_id : bool)
            (same_according_to_uid : bool)]
  in
  let module Bool =
    Type_equal.Id.Create0 (struct
      type t = bool [@@deriving sexp_of]

      let name = "bool"
    end)
  in
  (* self comparison *)
  test Bool.type_equal_id Bool.type_equal_id;
  [%expect {| (bool == bool) |}];
  let module Int =
    Type_equal.Id.Create0 (struct
      type t = int [@@deriving sexp_of]

      let name = "int"
    end)
  in
  (* another self comparison *)
  test Int.type_equal_id Int.type_equal_id;
  [%expect {| (int == int) |}];
  (* non-self comparison *)
  test Int.type_equal_id Bool.type_equal_id;
  [%expect {| (int <> bool) |}];
  (* re-creating the same type *)
  test Int.type_equal_id (Type_equal.Id.create ~name:"Stdlib.int" sexp_of_int);
  [%expect {| (int <> Stdlib.int) |}];
  let module Option =
    Type_equal.Id.Create1 (struct
      type 'a t = 'a option [@@deriving sexp_of]

      let name = "option"
    end)
  in
  (* 1-ary vs 0-ary *)
  test (Option.type_equal_id Int.type_equal_id) Int.type_equal_id;
  [%expect {| ((option int) <> int) |}];
  (* 1-ary applied twice to same argument *)
  test (Option.type_equal_id Int.type_equal_id) (Option.type_equal_id Int.type_equal_id);
  [%expect {| ((option int) == (option int)) |}];
  (* 1-ary with different argument *)
  test (Option.type_equal_id Int.type_equal_id) (Option.type_equal_id Bool.type_equal_id);
  [%expect {| ((option int) <> (option bool)) |}];
  let module Either =
    Type_equal.Id.Create2 (struct
      type ('a, 'b) t = ('a, 'b) Either.t [@@deriving sexp_of]

      let name = "either"
    end)
  in
  (* 2-ary vs 0-ary *)
  test (Either.type_equal_id Int.type_equal_id Bool.type_equal_id) Int.type_equal_id;
  [%expect {| ((either int bool) <> int) |}];
  (* 2-ary vs 1-ary *)
  test
    (Either.type_equal_id Int.type_equal_id Bool.type_equal_id)
    (Option.type_equal_id Int.type_equal_id);
  [%expect {| ((either int bool) <> (option int)) |}];
  (* 2-ary applied twice to same arguments *)
  test
    (Either.type_equal_id Int.type_equal_id Bool.type_equal_id)
    (Either.type_equal_id Int.type_equal_id Bool.type_equal_id);
  [%expect {| ((either int bool) == (either int bool)) |}];
  (* 2-ary with different arguments *)
  test
    (Either.type_equal_id Int.type_equal_id Bool.type_equal_id)
    (Either.type_equal_id Bool.type_equal_id Int.type_equal_id);
  [%expect {| ((either int bool) <> (either bool int)) |}];
  let module Tuple3 =
    Type_equal.Id.Create3 (struct
      type ('a, 'b, 'c) t = 'a * 'b * 'c [@@deriving sexp_of]

      let name = "tuple3"
    end)
  in
  (* 3-ary vs 0-ary *)
  test
    (Tuple3.type_equal_id
       Int.type_equal_id
       Bool.type_equal_id
       (Option.type_equal_id Bool.type_equal_id))
    Int.type_equal_id;
  [%expect {| ((tuple3 int bool (option bool)) <> int) |}];
  (* 3-ary vs 1-ary *)
  test
    (Tuple3.type_equal_id
       Int.type_equal_id
       Bool.type_equal_id
       (Option.type_equal_id Bool.type_equal_id))
    (Option.type_equal_id Int.type_equal_id);
  [%expect {| ((tuple3 int bool (option bool)) <> (option int)) |}];
  (* 3-ary vs 2-ary *)
  test
    (Tuple3.type_equal_id
       Int.type_equal_id
       Bool.type_equal_id
       (Option.type_equal_id Bool.type_equal_id))
    (Either.type_equal_id Int.type_equal_id Bool.type_equal_id);
  [%expect {| ((tuple3 int bool (option bool)) <> (either int bool)) |}];
  (* 3-ary applied twice to same arguments *)
  test
    (Tuple3.type_equal_id
       Int.type_equal_id
       Bool.type_equal_id
       (Option.type_equal_id Bool.type_equal_id))
    (Tuple3.type_equal_id
       Int.type_equal_id
       Bool.type_equal_id
       (Option.type_equal_id Bool.type_equal_id));
  [%expect {| ((tuple3 int bool (option bool)) == (tuple3 int bool (option bool))) |}];
  (* 3-ary with different arguments *)
  test
    (Tuple3.type_equal_id
       Int.type_equal_id
       Bool.type_equal_id
       (Option.type_equal_id Bool.type_equal_id))
    (Tuple3.type_equal_id Int.type_equal_id Bool.type_equal_id Int.type_equal_id);
  [%expect {| ((tuple3 int bool (option bool)) <> (tuple3 int bool int)) |}]
;;
