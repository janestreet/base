open! Import


module T = struct
  type t = bytes [@@deriving_inline sexp]
  let t_of_sexp : Sexplib.Sexp.t -> t = bytes_of_sexp
  let sexp_of_t : t -> Sexplib.Sexp.t = sexp_of_bytes
  [@@@end]
  include Bytes0

  let module_name = "Base.Bytes"

  let pp fmt t = Caml.Format.fprintf fmt "%S" (to_string t)

  let equal (t1 : t) (t2 : t) =
    compare t1 t2 = 0

end

include T

include
  Blit.Make(struct
    include T
    let create ~len = create len
  end)

include Comparator.Make(T)
include Comparable.Validate(T)

include Pretty_printer.Register_pp(T)

module To_string = struct
  let sub = sub_string
  let subo ?(pos = 0) ?len src =
    sub src ~pos ~len:(match len with Some i -> i | None -> length src - pos)
  ;;
end

module From_string = struct
  let blit = blit_string
  let blito ~src ?(src_pos = 0) ?(src_len = String.length src - src_pos) ~dst ?(dst_pos = 0) () =
    blit ~src ~src_pos ~len:src_len ~dst ~dst_pos
end

let init n ~f =
  if n < 0 then Printf.invalid_argf "Bytes.init %d" n ();
  let t = create n in
  for i = 0 to n - 1 do
    unsafe_set t i (f i)
  done;
  t

let of_char_list l =
  let t = create (List.length l) in
  List.iteri l ~f:(fun i c -> set t i c);
  t

let to_list t =
  let rec loop t i acc =
    if i < 0
    then acc
    else loop t (i - 1) (unsafe_get t i :: acc)
  in
  loop t (length t - 1) []

let tr ~target ~replacement s =
  for i = 0 to length s - 1 do
    if Char.equal (unsafe_get s i) target
    then unsafe_set s i replacement
  done

module Replace_polymorphic_compare = struct
  let equal = equal
  let compare (x : t) y = compare x y
  let ascending = compare
  let descending x y = compare y x
  let ( >= ) x y = Poly.( >= ) (x : t) y
  let ( <= ) x y = Poly.( <= ) (x : t) y
  let ( =  ) x y = Poly.( =  ) (x : t) y
  let ( >  ) x y = Poly.( >  ) (x : t) y
  let ( <  ) x y = Poly.( <  ) (x : t) y
  let ( <> ) x y = Poly.( <> ) (x : t) y
  let min (x : t) y = if x < y then x else y
  let max (x : t) y = if x > y then x else y
  let between t ~low ~high = low <= t && t <= high
  let clamp_unchecked t ~min ~max =
    if t < min then min else if t <= max then t else max

  let clamp_exn t ~min ~max =
    assert (min <= max);
    clamp_unchecked t ~min ~max

  let clamp t ~min ~max =
    if min > max then
      Or_error.error_s
        (Sexp.message "clamp requires [min <= max]"
           [ "min", T.sexp_of_t min
           ; "max", T.sexp_of_t max
           ])
    else
      Ok (clamp_unchecked t ~min ~max)
end

include Replace_polymorphic_compare
