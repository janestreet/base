open! Import
open Type_equal_intf.Definitions
module Sexp = Sexp0

type ('a : any, 'b : any) t = T : ('a : any). ('a, 'a) t [@@deriving sexp_of ~stackify]
type ('a : any, 'b : any) equal = ('a, 'b) t

include Type_equal_defns (struct
    type ('a : any, 'b : any) t = ('a, 'b) equal
  end)

let refl = T
let sym (type (a : any) (b : any)) (T : (a, b) t) : (b, a) t = T

let trans (type (a : any) (b : any) (c : any)) (T : (a, b) t) (T : (b, c) t) : (a, c) t =
  T
;;

let%template conv (type (a : k) (b : k)) (T : (a, b) t) (a : a @ l v) : b @ l v = a
[@@kind k = (value_or_null, value_or_null & bits64)]
[@@mode l = (global, local), v = (read_write, read, immutable)]
;;

[%%template
[@@@kind_set.define smaller_all = (any, value)]
[@@@kind_set.define all = (smaller_all, value_or_null)]

module Lift (X : sig
    type ('a : k1) t
  end) =
struct
  let lift (type (a : k1) (b : k1)) (T : (a, b) t) : (a X.t, b X.t) t = T
end
[@@kind.explicit k1 = all]

module Lift = Lift [@kind.explicit value]

module Lift2 (X : sig
    type ('a1 : k1, 'a2 : k2) t
  end) =
struct
  let lift
    (type (a1 : k1) (b1 : k1) (a2 : k2) (b2 : k2))
    (T : (a1, b1) t)
    (T : (a2, b2) t)
    : ((a1, a2) X.t, (b1, b2) X.t) t
    =
    T
  ;;
end
[@@kind.explicit k1 = all, k2 = all]

module Lift2 = Lift2 [@kind.explicit value value]

[@@@kind.default k1 = smaller_all, k2 = smaller_all, k3 = smaller_all]

module Lift3 (X : sig
    type ('a1 : k1, 'a2 : k2, 'a3 : k3) t
  end) =
struct
  let lift
    (type (a1 : k1) (b1 : k1) (a2 : k2) (b2 : k2) (a3 : k3) (b3 : k3))
    (T : (a1, b1) t)
    (T : (a2, b2) t)
    (T : (a3, b3) t)
    : ((a1, a2, a3) X.t, (b1, b2, b3) X.t) t
    =
    T
  ;;
end

[@@@kind.default k4 = smaller_all]

module Lift4 (X : sig
    type ('a1 : k1, 'a2 : k2, 'a3 : k3, 'a4 : k4) t
  end) =
struct
  let lift
    (type (a1 : k1) (b1 : k1) (a2 : k2) (b2 : k2) (a3 : k3) (b3 : k3) (a4 : k4) (b4 : k4))
    (T : (a1, b1) t)
    (T : (a2, b2) t)
    (T : (a3, b3) t)
    (T : (a4, b4) t)
    : ((a1, a2, a3, a4) X.t, (b1, b2, b3, b4) X.t) t
    =
    T
  ;;
end]

let detuple2 (type a1 a2 b1 b2) (T : (a1 * a2, b1 * b2) t) : (a1, b1) t * (a2, b2) t =
  T, T
;;

let tuple2 (type a1 a2 b1 b2) (T : (a1, b1) t) (T : (a2, b2) t) : (a1 * a2, b1 * b2) t = T

module Id = struct
  (* [key] is an extensible GADT used to mint, and pattern match on, type witnesses. *)
  type (_ : any) key = ..

  module Uid = struct
    (* A unique id contains an [int] representing a (possibly parameterized) type, and a
       list of uids for the parameters to that type. *)
    type t = T of int * t list [@@deriving compare ~localize, hash, sexp_of ~stackify]

    include%template Comparable.Make [@mode local] [@modality portable] (struct
        type nonrec t = t

        let[@mode l = (local, global)] compare = (compare [@mode l])
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
    type t : any

    (* How to render values of the type. *)
    val sexp_of_t : t -> Sexp.t

    (* A unique id for this type. *)
    val uid : Uid.t

    (* Name of the type-equal id. *)
    val id_name : string

    (* Sexp of the type-equal id. *)
    val id_sexp : Sexp.t

    (* [key] value for the type. *)
    val type_key : t key @@ immutable

    (* type equality: given another key, produce an [equal] if they represent the same
       type instance *)
    val type_equal : ('a : any). 'a key @ immutable -> (t, 'a) equal option [@@zero_alloc]
  end

  type ('a : any) t = { immutable : (module S with type t = 'a) @@ immutable } [@@unboxed]

  let%template uid (type a : any) ({ immutable = (module M) } : a t) = M.uid
  [@@mode m = (global, local)]
  ;;

  let%template[@mode m = (global, local)] name
    (type a : any)
    ({ immutable = (module M) } : a t)
    =
    M.id_name
  ;;

  let sexp_of_t (type a : any) _ ({ immutable = (module M) } : a t) = M.id_sexp
  let to_sexp (type a : any) ({ immutable = (module M) } : a t) = M.sexp_of_t
  let hash t = Uid.hash (uid t)
  let hash_fold_t state t = Uid.hash_fold_t state (uid t)

  let same_witness
    (type (a : any) (b : any))
    ({ immutable = (module A) } : a t)
    ({ immutable = (module B) } : b t)
    =
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

  include Type_equal_id_defns (struct
      type nonrec ('a : any) t = 'a t
    end)

  module%template.portable Create0 (T : Arg0) = struct
    type _ key += T0 : T.t key

    let type_equal_id : T.t t =
      { immutable =
          (module struct
            type t = T.t

            let id_name = T.name
            let id_sexp = Sexp.Atom id_name
            let sexp_of_t = T.sexp_of_t
            let type_key = T0
            let uid = Uid.create type_key []

            let type_equal (type other : any) (otherkey : other key)
              : (T.t, other) equal option
              =
              match otherkey with
              | T0 -> Some T
              | _ -> None
            ;;
          end)
      }
    ;;
  end

  module%template.portable Create1 (T : Arg1) = struct
    type _ key += T1 : 'a key -> 'a T.t key

    let type_equal_id (type a) ({ immutable = (module A) } : a t) : a T.t t =
      { immutable =
          (module struct
            type t = a T.t

            let id_name = T.name
            let id_sexp = Sexp.List [ Atom id_name; A.id_sexp ]
            let sexp_of_t t = T.sexp_of_t A.sexp_of_t t
            let type_key = T1 A.type_key
            let uid = Uid.create type_key [ A.uid ]

            let type_equal (type other : any) (otherkey : other key)
              : (a T.t, other) equal option
              =
              match otherkey with
              | T1 akey ->
                (match A.type_equal akey with
                 | Some T -> Some T
                 | None -> None)
              | _ -> None
            ;;
          end)
      }
    ;;
  end

  module%template.portable
    [@kind a = (value, immediate64), b = value] Create2
      (T : Arg2
    [@kind a b]) =
  struct
    type _ key += T2 : ('a : a) ('b : b). 'a key * 'b key -> ('a, 'b) T.t key

    let type_equal_id
      (type (a : a) (b : b))
      ({ immutable = (module A) } : a t)
      ({ immutable = (module B) } : b t)
      : (a, b) T.t t
      =
      { immutable =
          (module struct
            type t = (a, b) T.t

            let id_name = T.name
            let id_sexp = Sexp.List [ Atom id_name; A.id_sexp; B.id_sexp ]
            let sexp_of_t t = T.sexp_of_t A.sexp_of_t B.sexp_of_t t
            let type_key = T2 (A.type_key, B.type_key)
            let uid = Uid.create type_key [ A.uid; B.uid ]

            let type_equal (type other : any) (otherkey : other key)
              : ((a, b) T.t, other) equal option
              =
              match otherkey with
              | T2 (akey, bkey) ->
                (match A.type_equal akey, B.type_equal bkey with
                 | Some T, Some T -> Some T
                 | None, _ | _, None -> None)
              | _ -> None
            ;;
          end)
      }
    ;;
  end

  module%template.portable Create3 (T : Arg3) = struct
    type _ key += T3 : 'a key * 'b key * 'c key -> ('a, 'b, 'c) T.t key

    let type_equal_id
      (type a b c)
      ({ immutable = (module A) } : a t)
      ({ immutable = (module B) } : b t)
      ({ immutable = (module C) } : c t)
      : (a, b, c) T.t t
      =
      { immutable =
          (module struct
            type t = (a, b, c) T.t

            let id_name = T.name
            let id_sexp = Sexp.List [ Atom id_name; A.id_sexp; B.id_sexp; C.id_sexp ]
            let sexp_of_t t = T.sexp_of_t A.sexp_of_t B.sexp_of_t C.sexp_of_t t
            let type_key = T3 (A.type_key, B.type_key, C.type_key)
            let uid = Uid.create type_key [ A.uid; B.uid; C.uid ]

            let type_equal (type other : any) (otherkey : other key)
              : ((a, b, c) T.t, other) equal option
              =
              match otherkey with
              | T3 (akey, bkey, ckey) ->
                (match A.type_equal akey, B.type_equal bkey, C.type_equal ckey with
                 | Some T, Some T, Some T -> Some T
                 | None, _, _ | _, None, _ | _, _, None -> None)
              | _ -> None
            ;;
          end)
      }
    ;;
  end

  module%template.portable Create4 (T : Arg4) = struct
    type _ key += T4 : 'a key * 'b key * 'c key * 'd key -> ('a, 'b, 'c, 'd) T.t key

    let type_equal_id
      (type a b c d)
      ({ immutable = (module A) } : a t)
      ({ immutable = (module B) } : b t)
      ({ immutable = (module C) } : c t)
      ({ immutable = (module D) } : d t)
      : (a, b, c, d) T.t t
      =
      { immutable =
          (module struct
            type t = (a, b, c, d) T.t

            let id_name = T.name

            let id_sexp =
              Sexp.List [ Atom id_name; A.id_sexp; B.id_sexp; C.id_sexp; D.id_sexp ]
            ;;

            let sexp_of_t t =
              T.sexp_of_t A.sexp_of_t B.sexp_of_t C.sexp_of_t D.sexp_of_t t
            ;;

            let type_key = T4 (A.type_key, B.type_key, C.type_key, D.type_key)
            let uid = Uid.create type_key [ A.uid; B.uid; C.uid; D.uid ]

            let type_equal (type other : any) (otherkey : other key)
              : ((a, b, c, d) T.t, other) equal option
              =
              match otherkey with
              | T4 (akey, bkey, ckey, dkey) ->
                (match
                   ( A.type_equal akey
                   , B.type_equal bkey
                   , C.type_equal ckey
                   , D.type_equal dkey )
                 with
                 | Some T, Some T, Some T, Some T -> Some T
                 | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None -> None)
              | _ -> None
            ;;
          end)
      }
    ;;
  end

  let%template create (type a : any) ~name sexp_of_t : a t =
    let open struct
      type _ key += T0 : a key
    end in
    { immutable =
        (module struct
          type t = a

          let id_name = name
          let id_sexp = Sexp.Atom id_name
          let sexp_of_t = sexp_of_t
          let type_key = T0
          let uid = Uid.create type_key []

          let type_equal (type other : any) (otherkey : other key)
            : (a, other) equal option
            =
            match otherkey with
            | T0 -> Some T
            | _ -> None
          ;;
        end)
    }
  [@@mode p = (nonportable, portable)]
  ;;
end
