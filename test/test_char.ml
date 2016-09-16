open! Import
open! Char

let%test_module "int to char conversion" =
  (module struct

    let%test_unit "of_int bounds" =
      let bounds_check i =
        [%test_result: t option]
          (of_int i)
          ~expect:None
          ~message:(Int.to_string i)
      in
      for i = 1 to 100 do
        bounds_check (-i);
        bounds_check (255 + i);
      done

    let%test_unit "of_int_exn vs of_int" =
      for i = -100 to 300 do
        [%test_eq: t option]
          (of_int i)
          (Option.try_with (fun () -> of_int_exn i))
          ~message:(Int.to_string i)
      done

    let%test_unit "unsafe_of_int vs of_int_exn" =
      for i = 0 to 255 do
        [%test_eq: t]
          (unsafe_of_int i)
          (of_int_exn    i)
          ~message:(Int.to_string i)
      done

  end)
