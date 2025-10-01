open! Import
include Stdlib.Lazy

type 'a t = 'a lazy_t [@@deriving sexp ~stackify, sexp_grammar]

external force : ('a t[@local_opt]) -> 'a @@ portable = "%lazy_force"

let globalize = Globalize.globalize_lazy_t
let map t ~f = lazy (f (force t))

let compare__local compare_a t1 t2 =
  if phys_equal t1 t2 then 0 else compare_a (force t1) (force t2)
;;

let compare compare_a t1 t2 = compare__local compare_a t1 t2

let equal__local equal_a t1 t2 =
  if phys_equal t1 t2 then true else equal_a (force t1) (force t2)
;;

let equal equal_a t1 t2 = equal__local equal_a t1 t2
let hash_fold_t = Hash.Builtin.hash_fold_lazy_t
let peek t = if is_val t then Some (force t) else None

include%template Monad.Make [@modality portable] (struct
    type nonrec 'a t = 'a t

    let return x = from_val x
    let bind t ~f = lazy (force (f (force t)))
    let map = map
    let map = `Custom map
  end)

module T_unforcing = struct
  type nonrec 'a t = 'a t

  let is_val t = Obj.tag (Obj.repr t) <> Stdlib.Obj.lazy_tag

  let%template[@alloc a = (heap, stack)] sexp_of_t sexp_of_a t =
    if [@exclave_if_stack a] is_val t
    then sexp_of_a (force t)
    else (sexp_of_string [@alloc a]) "<unforced lazy>"
  ;;
end
