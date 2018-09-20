open! Import

module Array = Array0

module T = struct
  type t = bytes [@@deriving_inline sexp]
  let t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t = bytes_of_sexp
  let sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t = sexp_of_bytes
  [@@@end]
  include Bytes0

  let module_name = "Base.Bytes"

  let pp fmt t = Caml.Format.fprintf fmt "%S" (to_string t)
end

include T

module To_bytes =
  Blit.Make
    (struct
      include T
      let create ~len = create len
    end)
include To_bytes

include Comparator.Make(T)
include Comparable.Validate(T)

include Pretty_printer.Register_pp(T)

(* Open replace_polymorphic_compare after including functor instantiations so they do not
   shadow its definitions. This is here so that efficient versions of the comparison
   functions are available within this module. *)
open! Bytes_replace_polymorphic_compare

module To_string = Blit.Make_to_string (T) (To_bytes)

module From_string = Blit.Make_distinct(struct
    type t = string
    let length = String.length
  end)
    (struct
      type nonrec t = t
      let create ~len = create len
      let length = length
      let unsafe_blit = unsafe_blit_string
    end)

let init n ~f =
  if Int_replace_polymorphic_compare.(<) n 0 then Printf.invalid_argf "Bytes.init %d" n ();
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
    if Int_replace_polymorphic_compare.(<) i 0
    then acc
    else loop t (i - 1) (unsafe_get t i :: acc)
  in
  loop t (length t - 1) []

let to_array t = Array.init (length t) ~f:(fun i -> (unsafe_get t i))

let fold t ~init ~f =
  let n = length t in
  let rec loop i ac = if Int.equal i n then ac else loop (i + 1) (f ac (unsafe_get t i)) in
  loop 0 init

let foldi t ~init ~f =
  let n = length t in
  let rec loop i ac = if Int.equal i n then ac else loop (i + 1) (f i ac (unsafe_get t i)) in
  loop 0 init

let tr ~target ~replacement s =
  for i = 0 to length s - 1 do
    if Char.equal (unsafe_get s i) target
    then unsafe_set s i replacement
  done

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

let contains ?pos ?len t char =
  let (pos, len) =
    Ordered_collection_common.get_pos_len_exn ?pos ?len ~length:(length t)
  in
  let last = pos + len in
  let rec loop i =
    Int_replace_polymorphic_compare.(<) i last
    && (Char.equal (get t i) char || loop (i + 1))
  in
  loop pos
;;

(* Include type-specific [Replace_polymorphic_compare] at the end, after
   including functor application that could shadow its definitions. This is
   here so that efficient versions of the comparison functions are exported by
   this module. *)
include Bytes_replace_polymorphic_compare
