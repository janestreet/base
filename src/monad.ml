open! Import
module List = List0
include Monad_intf.Definitions

[%%template
[@@@kind.default k = (value, value_or_null mod maybe_null)]
[@@@mode.default m = (global, local)]

module%template.portable Make3_indexed (M : Basic3_indexed [@kind k] [@mode m]) :
  S3_indexed
  [@kind k] [@mode m]
  with type ('a : k, 'i, 'j, 'p, 'q) t := ('a, 'i, 'j, 'p, 'q) M.t = struct
  let bind = M.bind
  let return = M.return
  let map_via_bind ma ~f = M.bind ma ~f:(fun a -> M.return (f a)) [@nontail]

  let map =
    match M.map with
    | `Define_using_bind -> map_via_bind
    | `Custom x -> x
  ;;

  module Monad_infix = struct
    let ( >>= ) t f = bind t ~f
    let ( >>| ) t f = map t ~f
  end

  include Monad_infix

  module Let_syntax = struct
    let return = return

    include Monad_infix

    module Let_syntax = struct
      let return = return
      let bind = bind
      let map = map
      let both a b = bind a ~f:(fun a -> map b ~f:(fun b -> a, b) [@nontail]) [@nontail]

      module Open_on_rhs = struct end
    end
  end

  let join t = t >>= Fn.id
  let ignore_m t = map t ~f:ignore

  let all =
    let rec loop vs = function
      | [] -> return (List.rev vs)
      | t :: ts -> t >>= fun v -> loop (v :: vs) ts
    in
    fun ts -> loop [] ts
  ;;

  let rec all_unit = function
    | [] -> return ()
    | t :: ts -> t >>= fun () -> all_unit ts
  ;;
end

module%template.portable
  [@modality p] Make_indexed
    (M : Basic_indexed
  [@kind k] [@mode m]) :
  S_indexed [@kind k] [@mode m] with type ('a : k, 'i, 'j) t := ('a, 'i, 'j) M.t =
Make3_indexed [@kind k] [@mode m] [@modality p] (struct
    include M

    type ('a : k, 'i, 'j, _, _) t = ('a, 'i, 'j) M.t
  end)

module%template.portable [@modality p] Make3 (M : Basic3 [@kind k] [@mode m]) :
  S3 [@kind k] [@mode m] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) M.t =
Make3_indexed [@kind k] [@mode m] [@modality p] (struct
    include M

    type ('a : k, _, _, 'p, 'q) t = ('a, 'p, 'q) M.t
  end)

module%template.portable [@modality p] Make2 (M : Basic2 [@kind k] [@mode m]) :
  S2 [@kind k] [@mode m] with type ('a : k, 'p) t := ('a, 'p) M.t =
Make3_indexed [@kind k] [@mode m] [@modality p] (struct
    include M

    type ('a : k, _, _, 'p, _) t = ('a, 'p) M.t
  end)

module%template.portable [@modality p] Make (M : Basic [@kind k] [@mode m]) :
  S [@kind k] [@mode m] with type ('a : k) t := 'a M.t =
Make3_indexed [@kind k] [@mode m] [@modality p] (struct
    include M

    type ('a : k, _, _, _, _) t = 'a M.t
  end)

module%template.portable
  [@modality p] Of_monad3_indexed
    (Monad : S3_indexed
  [@kind k] [@mode m])
    (M : sig
       type ('a : k, 'i, 'j, 'p, 'q) t : k

       val to_monad
         : ('a : k) 'i 'j 'p 'q.
         ('a, 'i, 'j, 'p, 'q) t -> ('a, 'i, 'j, 'p, 'q) Monad.t

       val of_monad
         : ('a : k) 'i 'j 'p 'q.
         ('a, 'i, 'j, 'p, 'q) Monad.t -> ('a, 'i, 'j, 'p, 'q) t
     end) :
  S3_indexed
  [@kind k] [@mode m]
  with type ('a : k, 'i, 'j, 'p, 'q) t := ('a, 'i, 'j, 'p, 'q) M.t =
Make3_indexed [@kind k] [@mode m] [@modality p] (struct
    type ('a : k, 'i, 'j, 'p, 'q) t = ('a, 'i, 'j, 'p, 'q) M.t

    let return a = M.of_monad (Monad.return a)
    let bind t ~f = M.of_monad (Monad.bind (M.to_monad t) ~f:(fun a -> M.to_monad (f a)))
    let map = `Custom (fun t ~f -> M.of_monad (Monad.map (M.to_monad t) ~f))
  end)

module%template.portable
  [@modality p] Of_monad_indexed
    (Monad : S_indexed
  [@kind k] [@mode m])
    (M : sig
       type ('a : k, 'i, 'j) t : k

       val to_monad : ('a : k) 'i 'j. ('a, 'i, 'j) t -> ('a, 'i, 'j) Monad.t
       val of_monad : ('a : k) 'i 'j. ('a, 'i, 'j) Monad.t -> ('a, 'i, 'j) t
     end) :
  S_indexed [@kind k] [@mode m] with type ('a : k, 'i, 'j) t := ('a, 'i, 'j) M.t =
  Of_monad3_indexed [@kind k] [@mode m] [@modality p]
    (struct
      include Monad

      type ('a : k, 'i, 'j, _, _) t = ('a, 'i, 'j) Monad.t
    end)
    (struct
      include M

      type ('a : k, 'i, 'j, _, _) t = ('a, 'i, 'j) M.t
    end)

module%template.portable
  [@modality p] Of_monad3
    (Monad : S3
  [@kind k] [@mode m])
    (M : sig
       type ('a : k, 'p, 'q) t : k

       val to_monad : ('a : k) 'p 'q. ('a, 'p, 'q) t -> ('a, 'p, 'q) Monad.t
       val of_monad : ('a : k) 'p 'q. ('a, 'p, 'q) Monad.t -> ('a, 'p, 'q) t
     end) : S3 [@kind k] [@mode m] with type ('a : k, 'p, 'q) t := ('a, 'p, 'q) M.t =
  Of_monad3_indexed [@kind k] [@mode m] [@modality p]
    (struct
      include Monad

      type ('a : k, _, _, 'p, 'q) t = ('a, 'p, 'q) Monad.t
    end)
    (struct
      include M

      type ('a : k, _, _, 'p, 'q) t = ('a, 'p, 'q) M.t
    end)

module%template.portable
  [@modality p] Of_monad2
    (Monad : S2
  [@kind k] [@mode m])
    (M : sig
       type ('a : k, 'p) t : k

       val to_monad : ('a : k) 'p. ('a, 'p) t -> ('a, 'p) Monad.t
       val of_monad : ('a : k) 'p. ('a, 'p) Monad.t -> ('a, 'p) t
     end) : S2 [@kind k] [@mode m] with type ('a : k, 'p) t := ('a, 'p) M.t =
  Of_monad3_indexed [@kind k] [@mode m] [@modality p]
    (struct
      include Monad

      type ('a : k, _, _, 'p, _) t = ('a, 'p) Monad.t
    end)
    (struct
      include M

      type ('a : k, _, _, 'p, _) t = ('a, 'p) M.t
    end)

module%template.portable
  [@modality p] Of_monad
    (Monad : S
  [@kind k] [@mode m])
    (M : sig
       type ('a : k) t : k

       val to_monad : ('a : k). 'a t -> 'a Monad.t
       val of_monad : ('a : k). 'a Monad.t -> 'a t
     end) : S [@kind k] [@mode m] with type ('a : k) t := 'a M.t =
  Of_monad3_indexed [@kind k] [@mode m] [@modality p]
    (struct
      include Monad

      type ('a : k, _, _, _, _) t = 'a Monad.t
    end)
    (struct
      include M

      type ('a : k, _, _, _, _) t = 'a M.t
    end)]

module Ident = struct
  type ('a : value_or_null) t = 'a

  let[@inline] bind a ~f = (f [@inlined hint]) a
  let[@inline] map a ~f = (f [@inlined hint]) a

  external return
    : ('a : value_or_null).
    ('a[@local_opt]) -> ('a[@local_opt])
    @@ portable
    = "%identity"

  module Monad_infix = struct
    let[@inline] ( >>| ) a f = map a ~f
    let[@inline] ( >>= ) a f = bind a ~f
  end

  include Monad_infix

  module Let_syntax = struct
    let return = return

    include Monad_infix

    module Let_syntax = struct
      let return = return
      let bind = bind
      let map = map
      let[@inline] both a b = a, b

      module Open_on_rhs = struct end
    end

    let return = return
  end

  external join
    : ('a : value_or_null).
    ('a[@local_opt]) -> ('a[@local_opt])
    @@ portable
    = "%identity"

  external ignore_m
    : ('a : value_or_null).
    ('a[@local_opt]) -> unit
    @@ portable
    = "%ignore"

  external all_unit : (unit list[@local_opt]) -> unit @@ portable = "%ignore"

  external all
    : ('a : value_or_null).
    ('a list[@local_opt]) -> ('a list[@local_opt])
    @@ portable
    = "%identity"
end
