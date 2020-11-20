open! Import
include Caml.Lazy

type 'a t = 'a lazy_t [@@deriving_inline sexp, sexp_grammar]

let t_of_sexp : 'a. (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t =
  lazy_t_of_sexp
;;

let sexp_of_t : 'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
  sexp_of_lazy_t
;;

let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
  let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
    { tycon_names = [ "lazy_t" ]
    ; ggid = "j\132);\135qH\158\135\222H\001\007\004\158\218"
    ; types =
        [ ( "t"
          , Tyvar_parameterize
              ([ "a" ], Tyvar_instantiate (Tycon_index 0, [ Tyvar_index 0 ])) )
        ]
    }
  in
  let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
    { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
    ; instantiate_tycons = [ lazy_t_sexp_grammar ]
    ; generic_group = _the_generic_group
    ; origin = "lazy.ml"
    }
  in
  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    Ref ("t", _the_group)
  in
  t_sexp_grammar
;;

[@@@end]

let map t ~f = lazy (f (force t))

let compare compare_a t1 t2 =
  if phys_equal t1 t2 then 0 else compare_a (force t1) (force t2)
;;

let hash_fold_t = Hash.Builtin.hash_fold_lazy_t

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let return x = from_val x
    let bind t ~f = lazy (force (f (force t)))
    let map = map
    let map = `Custom map
  end)

module T_unforcing = struct
  type nonrec 'a t = 'a t

  let sexp_of_t sexp_of_a t =
    if is_val t then sexp_of_a (force t) else sexp_of_string "<unforced lazy>"
  ;;
end
