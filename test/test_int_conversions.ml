open! Import
open! Base.Not_exposed_properly.Int_conversions

let%test_module "pretty" =
  (module struct

    let check input output =
      List.for_all [""; "+"; "-"] ~f:(fun prefix ->
        let input  = prefix ^ input  in
        let output = prefix ^ output in
        [%compare.equal: string] output (insert_underscores input))

    let%test _ = check          "1"             "1"
    let%test _ = check         "12"            "12"
    let%test _ = check        "123"           "123"
    let%test _ = check       "1234"         "1_234"
    let%test _ = check      "12345"        "12_345"
    let%test _ = check     "123456"       "123_456"
    let%test _ = check    "1234567"     "1_234_567"
    let%test _ = check   "12345678"    "12_345_678"
    let%test _ = check  "123456789"   "123_456_789"
    let%test _ = check "1234567890" "1_234_567_890"

  end)
