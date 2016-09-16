open! Import

type t = Less | Equal | Greater [@@deriving compare, hash, enumerate, sexp]

module Export = struct
  type _ordering = t =
  | Less
  | Equal
  | Greater
end

let of_int n =
  if n < 0
  then Less
  else if n = 0
  then Equal
  else Greater
;;

let to_int = function
  | Less    -> -1
  | Equal   -> 0
  | Greater -> 1
;;

let%test _ = of_int (-10) = Less
let%test _ = of_int (-1)  = Less
let%test _ = of_int 0     = Equal
let%test _ = of_int 1     = Greater
let%test _ = of_int 10    = Greater

let%test _ = of_int (Pervasives.compare 0 1) = Less
let%test _ = of_int (Pervasives.compare 1 1) = Equal
let%test _ = of_int (Pervasives.compare 1 0) = Greater

let%test _ = List.for_all (fun t -> t = (t |> to_int |> of_int)) all
let%test _ = List.for_all (fun i -> i = (i |> of_int |> to_int)) [ -1; 0; 1 ]
