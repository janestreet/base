open! Import
open Type_equal_intf.Definitions
module Sexp = Sexp0

type ('a, 'b) t = T : 'a. ('a, 'a) t [@@deriving sexp_of ~localize]
type ('a, 'b) equal = ('a, 'b) t

include Type_equal_defns (struct
    type ('a, 'b) t = ('a, 'b) equal
  end)

let refl = T
let sym (type a b) (T : (a, b) t) : (b, a) t = T
let trans (type a b c) (T : (a, b) t) (T : (b, c) t) : (a, c) t = T
let conv (type a b) (T : (a, b) t) (a : a) : b = a

[%%template
[@@@kind.default k1 = (any, value)]

module Lift (X : sig
    type 'a t
  end) =
struct
  let lift (type a b) (T : (a, b) t) : (a X.t, b X.t) t = T
end

[@@@kind.default k2 = (any, value)]

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

[@@@kind.default k3 = (any, value)]

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

[@@@kind.default k4 = (any, value)]

module Lift4 (X : sig
    type ('a1, 'a2, 'a3, 'a4) t
  end) =
struct
  let lift
    (type a1 b1 a2 b2 a3 b3 a4 b4)
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
  type _ key = ..

  module Uid = struct
    (* A unique id contains an [int] representing a (possibly parameterized) type, and a
       list of uids for the parameters to that type. *)
    type t = T of int * t list [@@deriving compare ~localize, hash, sexp_of ~localize]

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
  type 'a t =
    { (* How to render values of the type. *)
      sexp_of_t : 'a -> Sexp.t
    ; (* A unique id for this type. *)
      uid : Uid.t
    ; (* Name of the type-equal id. *)
      id_name : string
    ; (* Sexp of the type-equal id. *)
      id_sexp : Sexp.t
    ; (* [key] value for the type. *)
      type_key : 'a key
    ; (* type equality: given another key, produce an [equal] if they represent the same
         type instance *)
      type_equal : 'b. 'b key -> ('a, 'b) equal option
    }

  let uid a = a.uid
  let name a = a.id_name
  let sexp_of_t _ a = a.id_sexp
  let to_sexp a = a.sexp_of_t
  let hash t = Uid.hash (uid t)
  let hash_fold_t state t = Uid.hash_fold_t state (uid t)
  let same_witness a b = a.type_equal b.type_key

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
      type nonrec 'a t = 'a t
    end)

  module%template.portable Create0 (T : Arg0) = struct
    type _ key += T0 : T.t key

    let type_equal_id : T.t t =
      let id_name = T.name in
      let id_sexp = Sexp.Atom id_name in
      let sexp_of_t = T.sexp_of_t in
      let type_key = T0 in
      let uid = Uid.create type_key [] in
      let type_equal (type other) (otherkey : other key) : (T.t, other) equal option =
        match otherkey with
        | T0 -> Some T
        | _ -> None
      in
      { id_name; id_sexp; sexp_of_t; type_key; uid; type_equal }
    ;;
  end

  module%template.portable Create1 (T : Arg1) = struct
    type _ key += T1 : 'a key -> 'a T.t key

    let type_equal_id (type a) (a : a t) : a T.t t =
      let id_name = T.name in
      let id_sexp = Sexp.List [ Atom id_name; a.id_sexp ] in
      let sexp_of_t t = T.sexp_of_t a.sexp_of_t t in
      let type_key = T1 a.type_key in
      let uid = Uid.create type_key [ a.uid ] in
      let type_equal (type other) (otherkey : other key) : (a T.t, other) equal option =
        match otherkey with
        | T1 akey ->
          (match a.type_equal akey with
           | Some T -> Some T
           | None -> None)
        | _ -> None
      in
      { id_name; id_sexp; sexp_of_t; type_key; uid; type_equal }
    ;;
  end

  module%template.portable Create2 (T : Arg2) = struct
    type _ key += T2 : 'a key * 'b key -> ('a, 'b) T.t key

    let type_equal_id (type a b) (a : a t) (b : b t) : (a, b) T.t t =
      let id_name = T.name in
      let id_sexp = Sexp.List [ Atom id_name; a.id_sexp; b.id_sexp ] in
      let sexp_of_t t = T.sexp_of_t a.sexp_of_t b.sexp_of_t t in
      let type_key = T2 (a.type_key, b.type_key) in
      let uid = Uid.create type_key [ a.uid; b.uid ] in
      let type_equal (type other) (otherkey : other key)
        : ((a, b) T.t, other) equal option
        =
        match otherkey with
        | T2 (akey, bkey) ->
          (match a.type_equal akey, b.type_equal bkey with
           | Some T, Some T -> Some T
           | None, _ | _, None -> None)
        | _ -> None
      in
      { id_name; id_sexp; sexp_of_t; type_key; uid; type_equal }
    ;;
  end

  module%template.portable Create3 (T : Arg3) = struct
    type _ key += T3 : 'a key * 'b key * 'c key -> ('a, 'b, 'c) T.t key

    let type_equal_id (type a b c) (a : a t) (b : b t) (c : c t) : (a, b, c) T.t t =
      let id_name = T.name in
      let id_sexp = Sexp.List [ Atom id_name; a.id_sexp; b.id_sexp; c.id_sexp ] in
      let sexp_of_t t = T.sexp_of_t a.sexp_of_t b.sexp_of_t c.sexp_of_t t in
      let type_key = T3 (a.type_key, b.type_key, c.type_key) in
      let uid = Uid.create type_key [ a.uid; b.uid; c.uid ] in
      let type_equal (type other) (otherkey : other key)
        : ((a, b, c) T.t, other) equal option
        =
        match otherkey with
        | T3 (akey, bkey, ckey) ->
          (match a.type_equal akey, b.type_equal bkey, c.type_equal ckey with
           | Some T, Some T, Some T -> Some T
           | None, _, _ | _, None, _ | _, _, None -> None)
        | _ -> None
      in
      { id_name; id_sexp; sexp_of_t; type_key; uid; type_equal }
    ;;
  end

  module%template.portable Create4 (T : Arg4) = struct
    type _ key += T4 : 'a key * 'b key * 'c key * 'd key -> ('a, 'b, 'c, 'd) T.t key

    let type_equal_id (type a b c d) (a : a t) (b : b t) (c : c t) (d : d t)
      : (a, b, c, d) T.t t
      =
      let id_name = T.name in
      let id_sexp =
        Sexp.List [ Atom id_name; a.id_sexp; b.id_sexp; c.id_sexp; d.id_sexp ]
      in
      let sexp_of_t t = T.sexp_of_t a.sexp_of_t b.sexp_of_t c.sexp_of_t d.sexp_of_t t in
      let type_key = T4 (a.type_key, b.type_key, c.type_key, d.type_key) in
      let uid = Uid.create type_key [ a.uid; b.uid; c.uid; d.uid ] in
      let type_equal (type other) (otherkey : other key)
        : ((a, b, c, d) T.t, other) equal option
        =
        match otherkey with
        | T4 (akey, bkey, ckey, dkey) ->
          (match
             a.type_equal akey, b.type_equal bkey, c.type_equal ckey, d.type_equal dkey
           with
           | Some T, Some T, Some T, Some T -> Some T
           | None, _, _, _ | _, None, _, _ | _, _, None, _ | _, _, _, None -> None)
        | _ -> None
      in
      { id_name; id_sexp; sexp_of_t; type_key; uid; type_equal }
    ;;
  end

  let%template create (type a) ~name sexp_of_t =
    let open struct
      type _ key += T0 : a key
    end in
    let id_name = name in
    let id_sexp = Sexp.Atom id_name in
    let sexp_of_t = sexp_of_t in
    let type_key = T0 in
    let uid = Uid.create type_key [] in
    let type_equal (type other) (otherkey : other key) : (a, other) equal option =
      match otherkey with
      | T0 -> Some T
      | _ -> None
    in
    { id_name; id_sexp; sexp_of_t; type_key; uid; type_equal }
  [@@mode p = (nonportable, portable)]
  ;;
end
