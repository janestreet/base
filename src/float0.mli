@@ portable

val is_nan : float @ local -> bool
val to_int64_preserve_order : float @ local -> int64 option
val to_int64_preserve_order_exn : float @ local -> int64
val of_int64_preserve_order : int64 @ local -> float
val one_ulp : [ `Down | `Up ] -> float @ local -> float
val upper_bound_for_int : int -> float
val is_x_minus_one_exact : float -> bool
val lower_bound_for_int : int -> float
val box : float @ local -> float
