val is_nan : float -> bool
val to_int64_preserve_order : float -> int64 option
val to_int64_preserve_order_exn : float -> int64
val of_int64_preserve_order : int64 -> float
val one_ulp : [ `Down | `Up ] -> float -> float
val upper_bound_for_int : int -> float
val is_x_minus_one_exact : float -> bool
val lower_bound_for_int : int -> float
val box : float -> float
