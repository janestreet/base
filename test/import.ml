include (Base : module type of struct include Base end
         with module Pervasives := Pervasives)
include Stdio
include Base_for_tests
include Expect_test_helpers_kernel.Std

let () = Base.Not_exposed_properly.Int_conversions.sexp_of_int_style := `Underscores

let ( land ) = Int.bit_and
let ( lor  ) = Int.bit_or
let ( lsl  ) = Int.shift_left
let ( lsr  ) = Int.shift_right_logical

let stage   = Staged.stage
let unstage = Staged.unstage

let ok_exn = Or_error.ok_exn
