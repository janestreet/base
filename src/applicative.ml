open! Import
module List = List0
include Applicative_intf.Definitions

[%%template
module%template.portable Make3 (X : Basic3) :
  S3 with type ('a, 'p, 'q) t := ('a, 'p, 'q) X.t = struct
  include X

  let ( <*> ) = apply
  let derived_map t ~f = return f <*> t

  let map =
    match X.map with
    | `Define_using_apply -> derived_map
    | `Custom x -> x
  ;;

  let ( >>| ) t f = map t ~f
  let map2 ta tb ~f = map ~f ta <*> tb
  let map3 ta tb tc ~f = map ~f ta <*> tb <*> tc
  let all ts = List.fold_right ts ~init:(return []) ~f:(map2 ~f:(fun x xs -> x :: xs))
  let both ta tb = map2 ta tb ~f:(fun a b -> a, b)
  let ( *> ) u v = return (fun () y -> y) <*> u <*> v
  let ( <* ) u v = return (fun x () -> x) <*> u <*> v
  let all_unit ts = List.fold ts ~init:(return ()) ~f:( *> )

  module Applicative_infix = struct
    let ( <*> ) = ( <*> )
    let ( *> ) = ( *> )
    let ( <* ) = ( <* )
    let ( >>| ) = ( >>| )
  end
end

module%template.portable [@modality p] Make2 (X : Basic2) :
  S2 with type ('a, 'p) t := ('a, 'p) X.t = Make3 [@modality p] (struct
    include X

    type ('a, 'p, _) t = ('a, 'p) X.t
  end)

module%template.portable [@modality p] Make (X : Basic) : S with type 'a t := 'a X.t =
Make3 [@modality p] (struct
    include X

    type ('a, _, _) t = 'a X.t
  end)

[@@@mode.default m = (global, local)]

module%template.portable Make3_using_map2 (X : Basic3_using_map2 [@mode m]) :
  S3 [@mode m] with type ('a, 'p, 'q) t := ('a, 'p, 'q) X.t = struct
  include X

  let apply tf ta = map2 tf ta ~f:(fun f a -> f a)
  let ( <*> ) = apply
  let derived_map t ~f = map2 ~f:(fun () -> f) (return ()) t [@nontail]

  let map =
    match X.map with
    | `Define_using_map2 -> derived_map
    | `Custom map -> map
  ;;

  let ( >>| ) t f = map t ~f
  let both ta tb = map2 ta tb ~f:(fun a b -> a, b)

  let map3 ta tb tc ~f =
    let res = map2 (both ta tb) tc ~f:(fun (a, b) c -> f a b c) in
    res
  ;;

  let all ts = List.fold_right ts ~init:(return []) ~f:(map2 ~f:(fun x xs -> x :: xs))
  let ( *> ) u v = map2 u v ~f:(fun () y -> y)
  let ( <* ) u v = map2 u v ~f:(fun x () -> x)
  let all_unit ts = List.fold ts ~init:(return ()) ~f:( *> )

  module Applicative_infix = struct
    let ( <*> ) = ( <*> )
    let ( *> ) = ( *> )
    let ( <* ) = ( <* )
    let ( >>| ) = ( >>| )
  end
end

module%template.portable
  [@modality p] Make2_using_map2
    (X : Basic2_using_map2
  [@mode m]) : S2 [@mode m] with type ('a, 'p) t := ('a, 'p) X.t =
Make3_using_map2 [@mode m] [@modality p] (struct
    include X

    type ('a, 'p, _) t = ('a, 'p) X.t
  end)

module%template.portable [@modality p] Make_using_map2 (X : Basic_using_map2 [@mode m]) :
  S [@mode m] with type 'a t := 'a X.t = Make3_using_map2 [@mode m] [@modality p] (struct
    include X

    type ('a, _, _) t = 'a X.t
  end)

module%template.portable [@modality p] Of_monad3 (M : Monad.S3 [@mode m]) :
  S3 [@mode m] with type ('a, 'p, 'q) t := ('a, 'p, 'q) M.t =
Make3_using_map2 [@mode m] [@modality p] (struct
    type ('a, 'p, 'q) t = ('a, 'p, 'q) M.t

    let return = M.return

    let map2 mx my ~f =
      M.bind mx ~f:(fun x -> M.map my ~f:(fun y -> f x y) [@nontail]) [@nontail]
    ;;

    let map = `Custom M.map
  end)

module%template.portable [@modality p] Of_monad2 (M : Monad.S2 [@mode m]) :
  S2 [@mode m] with type ('a, 'p) t := ('a, 'p) M.t =
Of_monad3 [@mode m] [@modality p] (struct
    include M

    type ('a, 'p, _) t = ('a, 'p) M.t
  end)

module%template.portable [@modality p] Of_monad (M : Monad.S [@mode m]) :
  S [@mode m] with type 'a t := 'a M.t = Of_monad3 [@mode m] [@modality p] (struct
    include M

    type ('a, _, _) t = 'a M.t
  end)

module%template.portable [@modality p] Compose (F : S [@mode m]) (G : S [@mode m]) :
  S [@mode m] with type 'a t = 'a F.t G.t = struct
  type 'a t = 'a F.t G.t

  include Make_using_map2 [@mode m] [@modality p] (struct
      type nonrec 'a t = 'a t

      let return a = G.return (F.return a)
      let map2 tx ty ~f = G.map2 tx ty ~f:(F.map2 ~f) [@nontail]
      let custom_map t ~f = G.map ~f:(F.map ~f) t [@nontail]
      let map = `Custom custom_map
    end)
end

module%template.portable [@modality p] Pair (F : S [@mode m]) (G : S [@mode m]) :
  S [@mode m] with type 'a t = 'a F.t * 'a G.t = struct
  type 'a t = 'a F.t * 'a G.t

  include Make_using_map2 [@mode m] [@modality p] (struct
      type nonrec 'a t = 'a t

      let return a = F.return a, G.return a
      let map2 tx ty ~f = F.map2 ~f (fst tx) (fst ty), G.map2 ~f (snd tx) (snd ty)
      let custom_map t ~f = F.map ~f (fst t), G.map ~f (snd t)
      let map = `Custom custom_map
    end)
end

[@@@modality.default p = (nonportable, portable)]

module Make_let_syntax3
    (X : sig
     @@ p
       include For_let_syntax3 [@mode m]
     end)
    (Intf : sig
       module type S
     end)
    (Impl : Intf.S) : sig @@ p
    include Let_syntax3 [@mode m]
  end
  with type ('a, 'p, 'q) t := ('a, 'p, 'q) X.t
  with module Open_on_rhs_intf := Intf = struct
  module Let_syntax = struct
    include X

    module Let_syntax = struct
      include X
      module Open_on_rhs = Impl
    end
  end
end

module Make_let_syntax2
    (X : sig
     @@ p
       include For_let_syntax2 [@mode m]
     end)
    (Intf : sig
       module type S
     end)
    (Impl : Intf.S) : sig @@ p
    include Let_syntax2 [@mode m]
  end
  with type ('a, 'p) t := ('a, 'p) X.t
  with module Open_on_rhs_intf := Intf =
  Make_let_syntax3 [@mode m] [@modality p]
    (struct
      include X

      type ('a, 'p, _) t = ('a, 'p) X.t
    end)
    (Intf)
    (Impl)

module Make_let_syntax
    (X : sig
     @@ p
       include For_let_syntax [@mode m]
     end)
    (Intf : sig
       module type S
     end)
    (Impl : Intf.S) : sig @@ p
    include Let_syntax [@mode m]
  end
  with type 'a t := 'a X.t
  with module Open_on_rhs_intf := Intf =
  Make_let_syntax3 [@mode m] [@modality p]
    (struct
      include X

      type ('a, _, _) t = 'a X.t
    end)
    (Intf)
    (Impl)]

module Ident = struct
  type 'a t = 'a

  let return = Fn.id
  let apply f a = f a
  let both a b = a, b
  let map3 a b c ~f = f a b c
  let map2 a b ~f = f a b
  let map a ~f = f a
  let all = Fn.id
  let all_unit = ignore

  module Applicative_infix = struct
    let ( <*> ) = apply
    let ( *> ) () y = y
    let ( <* ) x () = x
    let ( >>| ) t f = map t ~f
  end

  include Applicative_infix
end
