open! Import

type ('a, 'b) t = T : ('a, 'a) t [@@deriving_inline sexp_of]

let sexp_of_t :
      'a 'b.
      ('a -> Sexplib0.Sexp.t) -> ('b -> Sexplib0.Sexp.t) -> ('a, 'b) t -> Sexplib0.Sexp.t
  =
  fun (type a__003_ b__004_)
    :  ((a__003_ -> Sexplib0.Sexp.t) -> (b__004_ -> Sexplib0.Sexp.t)
    -> (a__003_, b__004_) t -> Sexplib0.Sexp.t) ->
  fun _of_a__001_ _of_b__002_ T -> Sexplib0.Sexp.Atom "T"
;;

[@@@end]

type ('a, 'b) equal = ('a, 'b) t

include Type_equal_intf.Type_equal_defns (struct
  type ('a, 'b) t = ('a, 'b) equal
end)

let refl = T
let sym (type a b) (T : (a, b) t) : (b, a) t = T
let trans (type a b c) (T : (a, b) t) (T : (b, c) t) : (a, c) t = T
let conv (type a b) (T : (a, b) t) (a : a) : b = a

module Lift (X : sig
  type 'a t
end) =
struct
  let lift (type a b) (T : (a, b) t) : (a X.t, b X.t) t = T
end

module Lift2 (X : sig
  type ('a1, 'a2) t
end) =
struct
  let lift (type a1 b1 a2 b2) (T : (a1, b1) t) (T : (a2, b2) t)
    : ((a1, a2) X.t, (b1, b2) X.t) t
    =
    T
  ;;
end

module Lift3 (X : sig
  type ('a1, 'a2, 'a3) t
end) =
struct
  let lift (type a1 b1 a2 b2 a3 b3) (T : (a1, b1) t) (T : (a2, b2) t) (T : (a3, b3) t)
    : ((a1, a2, a3) X.t, (b1, b2, b3) X.t) t
    =
    T
  ;;
end

let detuple2 (type a1 a2 b1 b2) (T : (a1 * a2, b1 * b2) t) : (a1, b1) t * (a2, b2) t =
  T, T
;;

let tuple2 (type a1 a2 b1 b2) (T : (a1, b1) t) (T : (a2, b2) t) : (a1 * a2, b1 * b2) t = T

module Id = struct
  (* [key] is an extensible GADT used to mint, and pattern match on, type witnesses. *)
  type _ key = ..

  module Uid = struct
    (* A unique id contains an [int] representing a (possibly parameterized) type, and a
       list of uids for the parameters to that type. *)
    type t = T of int * t list [@@deriving_inline compare, hash, sexp_of]

    let rec compare =
      (fun a__005_ b__006_ ->
         if Stdlib.( == ) a__005_ b__006_
         then 0
         else (
           match a__005_, b__006_ with
           | T (_a__007_, _a__009_), T (_b__008_, _b__010_) ->
             (match compare_int _a__007_ _b__008_ with
              | 0 -> compare_list compare _a__009_ _b__010_
              | n -> n))
        : t -> t -> int)
    ;;

    let rec (hash_fold_t :
              Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state)
      =
      (fun hsv arg ->
         match arg with
         | T (_a0, _a1) ->
           let hsv = hsv in
           let hsv =
             let hsv = hsv in
             hash_fold_int hsv _a0
           in
           hash_fold_list hash_fold_t hsv _a1
        : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state)

    and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
      let func arg =
        Ppx_hash_lib.Std.Hash.get_hash_value
          (let hsv = Ppx_hash_lib.Std.Hash.create () in
           hash_fold_t hsv arg)
      in
      fun x -> func x
    ;;

    let rec sexp_of_t =
      (fun (T (arg0__013_, arg1__014_)) ->
         let res0__015_ = sexp_of_int arg0__013_
         and res1__016_ = sexp_of_list sexp_of_t arg1__014_ in
         Sexplib0.Sexp.List [ Sexplib0.Sexp.Atom "T"; res0__015_; res1__016_ ]
        : t -> Sexplib0.Sexp.t)
    ;;

    [@@@end]

    include Comparable.Make (struct
      type nonrec t = t

      let compare = compare
      let sexp_of_t = sexp_of_t
    end)

    (* We use the extension constructor id for a [key] as the unique id for its type. *)
    let create (key : _ key) args =
      let tag =
        Stdlib.Obj.Extension_constructor.id (Stdlib.Obj.Extension_constructor.of_val key)
      in
      T (tag, args)
    ;;
  end

  (* Every type-equal id must support these operations. *)
  module type S = sig
    type t

    (* How to render values of the type. *)
    val sexp_of_t : t -> Sexp.t

    (* A unique id for this type. *)
    val uid : Uid.t

    (* Name of the type-equal id. *)
    val id_name : string

    (* Sexp of the type-equal id. *)
    val id_sexp : Sexp.t

    (* [key] value for the type. *)
    val type_key : t key

    (* type equality: given another key, produce an [equal] if they represent the same
       type instance *)
    val type_equal : 'a key -> (t, 'a) equal option
  end

  (* An [Id.t] is a first-class module implementing the above operations. *)
  type 'a t = (module S with type t = 'a)

  let uid (type a) ((module A) : a t) = A.uid
  let name (type a) ((module A) : a t) = A.id_name
  let sexp_of_t (type a) _ ((module A) : a t) = A.id_sexp
  let to_sexp (type a) ((module A) : a t) = A.sexp_of_t
  let hash t = Uid.hash (uid t)
  let hash_fold_t state t = Uid.hash_fold_t state (uid t)

  let same_witness (type a b) ((module A) : a t) ((module B) : b t) =
    A.type_equal B.type_key
  ;;

  let same_witness_exn t1 t2 =
    match same_witness t1 t2 with
    | Some equal -> equal
    | None ->
      Error.raise_s
        (Sexp.message
           "Type_equal.Id.same_witness_exn got different ids"
           [ ( ""
             , sexp_of_pair (sexp_of_t sexp_of_opaque) (sexp_of_t sexp_of_opaque) (t1, t2)
             )
           ])
  ;;

  let same t1 t2 =
    match same_witness t1 t2 with
    | Some _ -> true
    | None -> false
  ;;

  include Type_equal_intf.Type_equal_id_defns (struct
    type nonrec 'a t = 'a t
  end)

  module Create0 (T : Arg0) = struct
    type _ key += T0 : T.t key

    let type_equal_id : T.t t =
      (module struct
        type t = T.t

        let id_name = T.name
        let id_sexp = Sexp.Atom id_name
        let sexp_of_t = T.sexp_of_t
        let type_key = T0
        let uid = Uid.create type_key []

        let type_equal (type other) (otherkey : other key) : (t, other) equal option =
          match otherkey with
          | T0 -> Some T
          | _ -> None
        ;;
      end)
    ;;
  end

  module Create1 (T : Arg1) = struct
    type _ key += T1 : 'a key -> 'a T.t key

    let type_equal_id (type a) ((module A) : a t) : a T.t t =
      (module struct
        type t = A.t T.t

        let id_name = T.name
        let id_sexp = Sexp.List [ Atom id_name; A.id_sexp ]
        let sexp_of_t t = T.sexp_of_t A.sexp_of_t t
        let type_key = T1 A.type_key
        let uid = Uid.create type_key [ A.uid ]

        let type_equal (type other) (otherkey : other key) : (t, other) equal option =
          match otherkey with
          | T1 akey ->
            (match A.type_equal akey with
             | Some T -> Some T
             | None -> None)
          | _ -> None
        ;;
      end)
    ;;
  end

  module Create2 (T : Arg2) = struct
    type _ key += T2 : 'a key * 'b key -> ('a, 'b) T.t key

    let type_equal_id (type a b) ((module A) : a t) ((module B) : b t) : (a, b) T.t t =
      (module struct
        type t = (A.t, B.t) T.t

        let id_name = T.name
        let id_sexp = Sexp.List [ Atom id_name; A.id_sexp; B.id_sexp ]
        let sexp_of_t t = T.sexp_of_t A.sexp_of_t B.sexp_of_t t
        let type_key = T2 (A.type_key, B.type_key)
        let uid = Uid.create type_key [ A.uid; B.uid ]

        let type_equal (type other) (otherkey : other key) : (t, other) equal option =
          match otherkey with
          | T2 (akey, bkey) ->
            (match A.type_equal akey, B.type_equal bkey with
             | Some T, Some T -> Some T
             | None, _ | _, None -> None)
          | _ -> None
        ;;
      end)
    ;;
  end

  module Create3 (T : Arg3) = struct
    type _ key += T3 : 'a key * 'b key * 'c key -> ('a, 'b, 'c) T.t key

    let type_equal_id
      (type a b c)
      ((module A) : a t)
      ((module B) : b t)
      ((module C) : c t)
      : (a, b, c) T.t t
      =
      (module struct
        type t = (A.t, B.t, C.t) T.t

        let id_name = T.name
        let id_sexp = Sexp.List [ Atom id_name; A.id_sexp; B.id_sexp; C.id_sexp ]
        let sexp_of_t t = T.sexp_of_t A.sexp_of_t B.sexp_of_t C.sexp_of_t t
        let type_key = T3 (A.type_key, B.type_key, C.type_key)
        let uid = Uid.create type_key [ A.uid; B.uid; C.uid ]

        let type_equal (type other) (otherkey : other key) : (t, other) equal option =
          match otherkey with
          | T3 (akey, bkey, ckey) ->
            (match A.type_equal akey, B.type_equal bkey, C.type_equal ckey with
             | Some T, Some T, Some T -> Some T
             | None, _, _ | _, None, _ | _, _, None -> None)
          | _ -> None
        ;;
      end)
    ;;
  end

  let create (type a) ~name sexp_of_t =
    let module T =
      Create0 (struct
        type t = a

        let name = name
        let sexp_of_t = sexp_of_t
      end)
    in
    T.type_equal_id
  ;;
end
