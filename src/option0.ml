open! Import0

type 'a t = 'a option =
  | None
  | Some of 'a

let is_none : _ t -> bool = function
  | None -> true
  | _ -> false
;;

let is_some : _ t -> bool = function
  | Some _ -> true
  | _ -> false
;;
