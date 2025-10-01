open! Import
module List = List0
include Applicative_intf.Definitions

[%%template
[@@@kind.default k = (value, value_or_null mod maybe_null)]

module%template.portable Make3 (X : Basic3 [@kind k]) :
  S3 [@kind k] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) X.t = struct
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

module%template.portable [@modality p] Make2 (X : Basic2 [@kind k]) :
  S2 [@kind k] with type ('a : k, 'p) t := ('a, 'p) X.t =
Make3 [@kind k] [@modality p] (struct
    include X

    type ('a : k, 'p, _) t = ('a, 'p) X.t
  end)

module%template.portable [@modality p] Make (X : Basic [@kind k]) :
  S [@kind k] with type ('a : k) t := 'a X.t = Make3 [@kind k] [@modality p] (struct
    include X

    type ('a : k, _, _) t = 'a X.t
  end)

[@@@mode.default m = (global, local)]

module%template.portable Make3_using_map2 (X : Basic3_using_map2 [@kind k] [@mode m]) :
  S3 [@kind k] [@mode m] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) X.t = struct
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
  [@kind k] [@mode m]) : S2 [@kind k] [@mode m] with type ('a : k, 'p) t := ('a, 'p) X.t =
Make3_using_map2 [@kind k] [@mode m] [@modality p] (struct
    include X

    type ('a : k, 'p, _) t = ('a, 'p) X.t
  end)

module%template.portable
  [@modality p] Make_using_map2
    (X : Basic_using_map2
  [@kind k] [@mode m]) : S [@kind k] [@mode m] with type ('a : k) t := 'a X.t =
Make3_using_map2 [@kind k] [@mode m] [@modality p] (struct
    include X

    type ('a : k, _, _) t = 'a X.t
  end)

module%template.portable [@modality p] Of_monad3 (M : Monad.S3 [@kind k] [@mode m]) :
  S3 [@kind k] [@mode m] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) M.t =
Make3_using_map2 [@kind k] [@mode m] [@modality p] (struct
    type ('a : k, 'p, 'q) t = ('a, 'p, 'q) M.t

    let return = M.return

    let map2 mx my ~f =
      M.bind mx ~f:(fun x -> M.map my ~f:(fun y -> f x y) [@nontail]) [@nontail]
    ;;

    let map = `Custom M.map
  end)

module%template.portable [@modality p] Of_monad2 (M : Monad.S2 [@kind k] [@mode m]) :
  S2 [@kind k] [@mode m] with type ('a : k, 'p) t := ('a, 'p) M.t =
Of_monad3 [@kind k] [@mode m] [@modality p] (struct
    include M

    type ('a : k, 'p, _) t = ('a, 'p) M.t
  end)

module%template.portable [@modality p] Of_monad (M : Monad.S [@kind k] [@mode m]) :
  S [@kind k] [@mode m] with type ('a : k) t := 'a M.t =
Of_monad3 [@kind k] [@mode m] [@modality p] (struct
    include M

    type ('a : k, _, _) t = 'a M.t
  end)

module%template.portable
  [@modality p] Compose
    (F : S
  [@kind k] [@mode m])
    (G : S
  [@kind k] [@mode m]) : S [@kind k] [@mode m] with type ('a : k) t = 'a F.t G.t = struct
  type ('a : k) t = 'a F.t G.t

  include Make_using_map2 [@kind k] [@mode m] [@modality p] (struct
      type nonrec ('a : k) t = 'a t

      let return a = G.return (F.return a)
      let map2 tx ty ~f = G.map2 tx ty ~f:(F.map2 ~f) [@nontail]
      let custom_map t ~f = G.map ~f:(F.map ~f) t [@nontail]
      let map = `Custom custom_map
    end)
end

module%template.portable
  [@modality p] Pair
    (F : S
  [@kind k] [@mode m])
    (G : S
  [@kind k] [@mode m]) : S [@kind k] [@mode m] with type ('a : k) t = 'a F.t * 'a G.t =
struct
  type ('a : k) t = 'a F.t * 'a G.t

  include Make_using_map2 [@kind k] [@mode m] [@modality p] (struct
      type nonrec ('a : k) t = 'a t

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
       include For_let_syntax3 [@kind k] [@mode m]
     end)
    (Intf : sig
       module type S
     end)
    (Impl : Intf.S) : sig @@ p
    include Let_syntax3 [@kind k] [@mode m]
  end
  with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) X.t
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
       include For_let_syntax2 [@kind k] [@mode m]
     end)
    (Intf : sig
       module type S
     end)
    (Impl : Intf.S) : sig @@ p
    include Let_syntax2 [@kind k] [@mode m]
  end
  with type ('a : k, 'p) t := ('a, 'p) X.t
  with module Open_on_rhs_intf := Intf =
  Make_let_syntax3 [@kind k] [@mode m] [@modality p]
    (struct
      include X

      type ('a : k, 'p, _) t = ('a, 'p) X.t
    end)
    (Intf)
    (Impl)

module Make_let_syntax
    (X : sig
     @@ p
       include For_let_syntax [@kind k] [@mode m]
     end)
    (Intf : sig
       module type S
     end)
    (Impl : Intf.S) : sig @@ p
    include Let_syntax [@kind k] [@mode m]
  end
  with type ('a : k) t := 'a X.t
  with module Open_on_rhs_intf := Intf =
  Make_let_syntax3 [@kind k] [@mode m] [@modality p]
    (struct
      include X

      type ('a : k, _, _) t = 'a X.t
    end)
    (Intf)
    (Impl)]

module%template [@mode p = (portable, nonportable)] Ident = struct
  type ('a : value_or_null) t = 'a

  let return x = x
  let apply f a = f a
  let both a b = a, b
  let map3 a b c ~f = f a b c
  let map2 a b ~f = f a b
  let map a ~f = f a
  let all x = x
  let all_unit = ignore

  module Applicative_infix = struct
    let ( <*> ) = apply
    let ( *> ) () y = y
    let ( <* ) x () = x
    let ( >>| ) t f = map t ~f
  end

  include Applicative_infix
end
