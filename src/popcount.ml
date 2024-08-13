open! Import

let int_popcount = Ocaml_intrinsics_kernel.Int.count_set_bits
let int64_popcount = Ocaml_intrinsics_kernel.Int64.count_set_bits
let int32_popcount = Ocaml_intrinsics_kernel.Int32.count_set_bits
let nativeint_popcount = Ocaml_intrinsics_kernel.Nativeint.count_set_bits
