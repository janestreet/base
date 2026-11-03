type 'a t = 'a option =
  | None
  | Some of 'a

val is_none : 'a. 'a t -> bool
val is_some : 'a. 'a t -> bool
