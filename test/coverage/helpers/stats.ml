open! Base

type t = { sizes : int Queue.t } [@@unboxed]

let create () = { sizes = Queue.create () }
let add t size = Queue.enqueue t.sizes size
let percentiles = [ 0; 50; 75; 90; 95; 99; 100 ]

type row =
  { percentile : int
  ; size : int
  ; count : int
  }

let print t =
  let zeros, sizes = Queue.to_array t.sizes |> Array.partition_tf ~f:(( = ) 0) in
  Array.sort sizes ~compare:Int.compare;
  let rows =
    if Array.is_empty sizes
    then []
    else
      List.map percentiles ~f:(fun percentile ->
        let len = Array.length sizes in
        let size = len * percentile / 100 |> Int.min (len - 1) |> Array.get sizes in
        let count =
          match
            Array.binary_search sizes ~compare `First_greater_than_or_equal_to size
          with
          | None -> 0
          | Some index -> Array.length sizes - index
        in
        { percentile; size; count })
      |> List.remove_consecutive_duplicates
           ~equal:(Comparable.lift Int.equal ~f:(fun row -> row.size))
           ~which_to_keep:`Last
  in
  Stdio.printf "  %% | size | count\n";
  Stdio.printf "----+------+------\n";
  if not (Array.is_empty zeros)
  then Stdio.printf "  - |    0 | %5d\n" (Array.length zeros);
  if not (Array.is_empty zeros || Array.is_empty sizes)
  then Stdio.printf "----+------+------\n";
  List.iter rows ~f:(fun { percentile; size; count } ->
    Stdio.printf "%3d | %4d | %5d\n" percentile size count)
;;
