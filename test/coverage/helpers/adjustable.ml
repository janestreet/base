open! Base
include Adjustable_intf.Definitions

(* The following functions that generate unique values don't need to worry about integer
   overflow because we generate integers with [Generator.small_positive_or_zero_int]. *)

let rec unique_int n list =
  if List.mem list n ~equal:( = ) then unique_int (n + 1) list else n
;;

let non_overlapping_ints list =
  List.fold list ~init:[] ~f:(fun list n -> unique_int n list :: list) |> List.rev
;;

let unique (type a) (module A : S with type t = a) a list =
  A.set a (unique_int (A.get a) (List.map ~f:A.get list))
;;

let non_overlapping (type a) (module A : S with type t = a) list =
  List.map2_exn ~f:A.set list (non_overlapping_ints (List.map ~f:A.get list))
;;
