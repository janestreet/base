open! Base

let%test_module _ = (module Hashtbl_tests.Make(struct
    include Hashtbl

    let create_poly ?size () = Poly.create ?size ()

    let of_alist_poly_exn l = Poly.of_alist_exn l
    let of_alist_poly_or_error l = Poly.of_alist_or_error l
  end))
