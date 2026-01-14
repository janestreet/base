open! Import


module Constants =
  struct
    let pow10 : string =
      "\000\000\000\000\000\000\000\000\t\000\000\000\000\000\000\000c\000\000\000\000\000\000\000\231\003\000\000\000\000\000\000\015'\000\000\000\000\000\000\159\134\001\000\000\000\000\000?B\015\000\000\000\000\000\127\150\152\000\000\000\000\000\255\224\245\005\000\000\000\000\255\201\154;\000\000\000\000\255\227\011T\002\000\000\000\255\231vH\023\000\000\000\255\015\165\212\232\000\000\000\255\159rN\024\t\000\000\255?z\016\243Z\000\000\255\127\198\164~\141\003\000\255\255\192o\242\134#\000\255\255\137]xEc\001\255\255c\167\179\182\224\r\255\255\255\255\255\255\255\127"
    let digit_pairs =
      "00010203040506070809101112131415161718192021222324252627282930313233343536373839404142434445464748495051525354555657585960616263646566676869707172737475767778798081828384858687888990919293949596979899"
  end
[@@@warning "-incompatible-with-upstream"]
module Char = struct let unsafe_of_int = Stdlib.Char.unsafe_chr end
module String =
  struct
    external unsafe_get :
      ((string)[@local_opt ]) -> ((int)[@local_opt ]) -> char @@ stateless =
        "%string_unsafe_get"
  end
module Bytes =
  struct
    external unsafe_set :
      ((bytes)[@local_opt ]) ->
        ((int)[@local_opt ]) -> ((char)[@local_opt ]) -> unit @@ stateless =
        "%bytes_unsafe_set"
    let create = Stdlib.Bytes.create
    external unsafe_to_string :
      no_mutation_while_string_reachable:((bytes)[@local_opt ]) ->
        ((string)[@local_opt ]) @@ stateless = "%bytes_to_string"
  end
module Bool =
  struct external to_int : bool -> int @@ stateless = "%identity" end
module I8 = struct type t : bits8 end
module I16 = struct type t : bits16 end
module I64 =
  struct
    type t = int64
    external (+) :
      local_ t -> local_ t -> ((t)[@local_opt ]) @@ stateless = "%int64_add"
    external (-) :
      local_ t -> local_ t -> ((t)[@local_opt ]) @@ stateless = "%int64_sub"
    external ( * ) :
      local_ t -> local_ t -> ((t)[@local_opt ]) @@ stateless = "%int64_mul"
    external (/) :
      local_ t -> local_ t -> ((t)[@local_opt ]) @@ stateless = "%int64_div"
    external (~-) :
      local_ t -> ((t)[@local_opt ]) @@ stateless = "%int64_neg"
    external \#land :
      local_ t -> local_ t -> ((t)[@local_opt ]) @@ stateless = "%int64_and"
    include Int64_replace_polymorphic_compare
    let clz = Ocaml_intrinsics_kernel.Int64.count_leading_zeros
    external \#lsr :
      local_ t -> local_ int -> ((t)[@local_opt ]) @@ stateless =
        "%int64_lsr"
    external of_int :
      int -> ((t)[@local_opt ]) @@ stateless = "%int64_of_int"
    external to_int_trunc : local_ t -> int @@ stateless = "%int64_to_int"
    external of_int32 :
      local_ int32 -> ((t)[@local_opt ]) @@ stateless = "%int64_of_int32"
    external of_nativeint :
      local_ nativeint -> ((t)[@local_opt ]) @@ stateless =
        "%int64_of_nativeint"
  end
module I64u =
  struct
    type t = int64#
    external of_int64 :
      ((int64)[@local_opt ]) -> ((t)[@unboxed ]) @@ stateless =
        "%unbox_int64"
    external to_int64 :
      ((t)[@unboxed ]) -> ((int64)[@local_opt ]) @@ stateless = "%box_int64"
    let of_int32 x = of_int64 (I64.of_int32 x)[@@inline ]
    let of_nativeint x = of_int64 (I64.of_nativeint x)[@@inline ]
    let select b t1 t2 =
      of_int64
        (Ocaml_intrinsics_kernel.Conditional.select_int64 b (to_int64 t1)
           (to_int64 t2))[@@inline ]
    let (+) t1 t2 = of_int64 (I64.(+) (to_int64 t1) (to_int64 t2))[@@inline ]
    let (-) t1 t2 = of_int64 (I64.(-) (to_int64 t1) (to_int64 t2))[@@inline ]
    let ( * ) t1 t2 = of_int64 (I64.( * ) (to_int64 t1) (to_int64 t2))
      [@@inline ]
    let (/) t1 t2 = of_int64 (I64.(/) (to_int64 t1) (to_int64 t2))[@@inline ]
    let (~-) t = of_int64 (I64.(~-) (to_int64 t))[@@inline ]
    let clz t = (((I64.clz)[@inlined hint]) (to_int64 t)) |> of_int64
      [@@inline ]
    let \#lsr t x =
      of_int64 (((let open I64 in \#lsr)[@inlined hint]) (to_int64 t) x)
      [@@inline ]
    let \#land t1 t2 = of_int64 (I64.(land) (to_int64 t1) (to_int64 t2))
      [@@inline ]
    let of_int x = of_int64 (I64.of_int x)[@@inline ]
    let to_int_trunc t = I64.to_int_trunc (to_int64 t)[@@inline ]
    let of_bool x = of_int (Bool.to_int x)[@@inline ]
    let (=) t1 t2 = I64.(=) (to_int64 t1) (to_int64 t2)[@@inline ]
    let (>) t1 t2 = I64.(>) (to_int64 t1) (to_int64 t2)[@@inline ]
    let (<) t1 t2 = I64.(<) (to_int64 t1) (to_int64 t2)[@@inline ]
    let (<>) t1 t2 = I64.(<>) (to_int64 t1) (to_int64 t2)[@@inline ]
    external unsafe_get_int64 :
      ((string)[@local_opt ]) -> int64# -> int64# @@ stateless =
        "%caml_string_get64u#_indexed_by_int64#"
    external swap64 : int64 -> int64 @@ stateless = "%bswap_int64"
    let swap64 v = of_int64 (swap64 (to_int64 v))
    let get_pow10 i =
      let v = unsafe_get_int64 Constants.pow10 (i * #8L) in
      if Stdlib.Sys.big_endian then swap64 v else v
    external bytes_unsafe_set_i8 :
      bytes -> ((t)[@unboxed ]) -> ((I8.t)[@unboxed ]) -> unit @@ stateless =
        "%unsafe_set_idx"
    external string_unsafe_get_i16 :
      string -> t -> ((I16.t)[@unboxed ]) @@ stateless = "%unsafe_get_idx"
    external bytes_unsafe_set_i16 :
      bytes -> ((t)[@unboxed ]) -> ((I16.t)[@unboxed ]) -> unit @@ stateless
        = "%unsafe_set_idx"
    external i8_of_i64 :
      ((t)[@unboxed ]) -> ((I8.t)[@unboxed ]) @@ stateless =
        "%int8#_of_int64#"
    let num_digits_neg (x : t) =
      let v = - x in
      let bits = select (x = #0L) x ((#64L - (clz v)) - #1L) in
      let est = #1L + ((bits * #1233L) lsr 12) in
      let need_extra = of_bool (v > (get_pow10 est)) in est + need_extra
    let bytes_unsafe_set_i8 buf pos x =
      match Sys.backend_type with
      | Native -> bytes_unsafe_set_i8 buf pos (i8_of_i64 x)
      | Bytecode | Other _ ->
          Bytes.unsafe_set buf (to_int_trunc pos)
            (Char.unsafe_of_int (to_int_trunc x))
    let write_one_digit buf ~pos d =
      let a = d + #48L in bytes_unsafe_set_i8 buf pos a
    let write_2_digits buf ~pos d =
      let digit_idx = d * #2L in
      match Sys.backend_type with
      | Native ->
          let digits = string_unsafe_get_i16 Constants.digit_pairs digit_idx in
          bytes_unsafe_set_i16 buf (pos - #1L) digits
      | Bytecode | Other _ ->
          (Bytes.unsafe_set buf (to_int_trunc (pos - #1L))
             (String.unsafe_get Constants.digit_pairs
                (to_int_trunc digit_idx));
           Bytes.unsafe_set buf (to_int_trunc pos)
             (String.unsafe_get Constants.digit_pairs
                (to_int_trunc (digit_idx + #1L))))
    let write_negative_decimal_without_sign buf ~pos ~num_digits n =
      let rec loop ~n ~pos ~remaining_digit_pairs =
        match remaining_digit_pairs > #0L with
        | true ->
            let q = n / #100L in
            let r = (q * #100L) - n in
            (write_2_digits buf ~pos r;
             loop ~n:q ~pos:(pos - #2L)
               ~remaining_digit_pairs:(remaining_digit_pairs - #1L))
        | false ->
            if (num_digits land #1L) <> #0L
            then let d = - n in write_one_digit buf ~pos d in
      let remaining_digit_pairs = num_digits / #2L in
      loop ~n ~pos ~remaining_digit_pairs
    let to_string n =
      let is_neg = n < #0L in
      let always_negative_n = select is_neg n (- n) in
      let num_digits = num_digits_neg always_negative_n in
      let len = (of_bool is_neg) + num_digits in
      let buf = Bytes.create (to_int_trunc len) in
      bytes_unsafe_set_i8 buf #0L (select is_neg #45L #48L);
      write_negative_decimal_without_sign buf ~pos:(len - #1L) ~num_digits
        always_negative_n;
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf[@@inline
                                                                    never]
  end
let int64_u_to_string i = I64u.to_string i[@@inline ]
let int_to_string i = I64u.to_string (I64u.of_int i)[@@inline ]
let int64_to_string i = I64u.to_string (I64u.of_int64 i)[@@inline ]
let int32_to_string i = I64u.to_string (I64u.of_int32 i)[@@inline ]
let nativeint_to_string i = I64u.to_string (I64u.of_nativeint i)[@@inline ]
module Private = struct module Constants = Constants end

