(** This module defines various abstract interfaces that are convenient when one needs a
    module that matches a bare signature with just a type. This sometimes occurs in
    functor arguments and in interfaces. *)

module type T = sig
  type t
end

[%%template
[@@@kind.default ka = (any, value)]

module type T1 = sig
  type ('a : ka) t
end

[@@@kind.default kb = (any, value)]

module type T2 = sig
  type ('a : ka, 'b : kb) t
end

[@@@kind.default kc = (any, value)]

module type T3 = sig
  type ('a : ka, 'b : kb, 'c : kc) t
end

[@@@kind.default kd = (any, value)]

module type T4 = sig
  type ('a : ka, 'b : kb, 'c : kc, 'd : kd) t
end]
