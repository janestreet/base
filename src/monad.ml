open! Import

include Monad_intf

module Make2 (M : Basic2) : S2 with type ('a, 'e) t := ('a, 'e) M.t = struct

  let bind   = M.bind
  let return = M.return

  let map_via_bind ma ~f = M.bind ma ~f:(fun a -> M.return (f a))

  let map =
    match M.map with
    | `Define_using_bind -> map_via_bind
    | `Custom x -> x

  module Monad_infix = struct
    let (>>=) t f = bind t ~f
    let (>>|) t f = map  t ~f
  end
  include Monad_infix

  module Let_syntax = struct

    let return = return
    include Monad_infix

    module Let_syntax = struct
      let return = return
      let bind   = bind
      let map    = map
      let both a b = a >>= fun a -> b >>| fun b -> (a, b)
      module Open_on_rhs  = struct end
    end
  end

  let join t = t >>= fun t' -> t'

  let ignore_m t = map t ~f:(fun _ -> ())

  let all =
    let rec loop vs = function
      | [] -> return (List.rev vs)
      | t :: ts -> t >>= fun v -> loop (v :: vs) ts
    in
    fun ts -> loop [] ts

  let rec all_ignore = function
    | [] -> return ()
    | t :: ts -> t >>= fun () -> all_ignore ts

end

module Make (M : Basic) : S with type 'a t := 'a M.t = struct
  include Make2 (struct
      type ('a, 'e) t = 'a M.t
      include (M : Basic with type 'a t := 'a M.t)
    end)
end

module Ident = struct
  type 'a t = 'a
  include Make (struct
      type nonrec 'a t = 'a t
      let bind a ~f = f a
      let return a = a
      let map = `Custom (fun a ~f -> f a)
    end)
end
