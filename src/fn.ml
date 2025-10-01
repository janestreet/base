open! Import

let const c _ = c

external ignore : ('a : any). ('a[@local_opt]) -> unit @@ portable = "%ignore"
[@@layout_poly]

(* this has the same behavior as [Stdlib.ignore] *)

let non f x = not (f x)

let forever f =
  let rec forever () =
    f ();
    forever ()
  in
  try forever () with
  | e -> e
;;

external id : ('a : any). ('a[@local_opt]) -> ('a[@local_opt]) @@ portable = "%identity"
[@@layout_poly]

external ( |> )
  : ('a : any) ('b : any).
  'a -> (('a -> 'b)[@local_opt]) -> 'b
  @@ portable
  = "%revapply"
[@@layout_poly]

(* The typical use case for these functions is to pass in functional arguments and get
   functions as a result. *)
let%template[@modality p = (nonportable, portable)] compose f g x = f (g x)
let%template[@modality p = (nonportable, portable)] flip f x y = f y x
let rec apply_n_times ~n f x = if n <= 0 then x else apply_n_times ~n:(n - 1) f (f x)
