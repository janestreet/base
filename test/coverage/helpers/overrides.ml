open! Base
open Base_quickcheck

let quickcheck_generator_int = Generator.small_strictly_positive_int

(* We change the generator for lists to get larger lists. Instead of splitting the size
   budget between the list's lengths and its element sizes, we generate those two
   separately within the full size budget. *)
let quickcheck_generator_list g =
  Generator.small_positive_or_zero_int
  |> Generator.bind ~f:(fun length -> Generator.list_with_length g ~length)
;;

module Either = struct
  include Either

  let quickcheck_generator = Generator.either
  let quickcheck_observer = Observer.either
  let quickcheck_shrinker = Shrinker.either
end

module Maybe_bound = struct
  include Maybe_bound

  type 'a t = 'a Maybe_bound.t =
    | Incl of 'a
    | Excl of 'a
    | Unbounded
  [@@deriving equal, quickcheck]

  let to_list = function
    | Incl x | Excl x -> [ x ]
    | Unbounded -> []
  ;;
end

let quickcheck_m = Memo.quickcheck_m
