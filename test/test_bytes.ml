open! Import
open! Bytes

let%test_module "Blit" =
  (module Test_blit.Test
            (struct
              include Char

              let of_bool b = if b then 'a' else 'b'
            end)
            (struct
              include Bytes

              let create ~len = create len
            end)
            (Bytes))
;;

let%expect_test "local" =
  let bytes = Bytes.create_local 10 in
  printf "%d\n" (Bytes.length bytes);
  [%expect {| 10 |}];
  for i = 0 to 9 do
    Bytes.set bytes i (Int.to_string i).[0]
  done;
  let string = Bytes.unsafe_to_string ~no_mutation_while_string_reachable:bytes in
  for i = 0 to 9 do
    printf "%c" string.[i]
  done;
  [%expect {| 0123456789 |}];
  Expect_test_helpers_base.require_does_raise [%here] (fun () ->
    ignore (Bytes.create_local (Sys.max_string_length + 1) : Bytes.t));
  [%expect {| (Invalid_argument Bytes.create_local) |}]
;;

let%test_module "Unsafe primitives" =
  (module struct
    let%expect_test "16-bit primitives" =
      let buffer = create 10 in
      (* Ensure that writing the biggest possible 16-bit value works. *)
      Bytes.unsafe_set_int16 buffer 2 0xFFFF;
      printf "0x%04x" (Bytes.unsafe_get_int16 buffer 2);
      [%expect {| 0xffff |}];
      (* Ensure that [16-bit] operations are indeed 16-bit, meaning it doesn't affect
         anything other than x[pos] and x[pos + 1]. *)
      Bytes.unsafe_set_int16 buffer 4 0;
      Bytes.unsafe_set_int16 buffer 2 ((1 lsl 16) + 1);
      printf "0x%04x" (Bytes.unsafe_get_int16 buffer 2);
      [%expect {| 0x0001 |}];
      printf "0x%04x" (Bytes.unsafe_get_int16 buffer 4);
      [%expect {| 0x0000 |}]
    ;;

    let%expect_test "32-bit primitives" =
      let buffer = create 10 in
      Bytes.unsafe_set_int32 buffer 0 0xdeadbeefl;
      printf "%lx" (Bytes.unsafe_get_int32 buffer 0);
      [%expect {| deadbeef |}];
      (* Ensure that Bytes.get will retrieve the individual positions byte values as
         written by Bytes.unsafe_set_int32. *)
      for i = 0 to 3 do
        let chr = Bytes.get buffer i in
        printf "buffer[%d] = 0x%02x\n" i (Char.to_int chr)
      done;
      [%expect
        {|
        buffer[0] = 0xef
        buffer[1] = 0xbe
        buffer[2] = 0xad
        buffer[3] = 0xde
        |}];
      (* Ensure that 32-bit writes works on non-word-aligned positions. *)
      Bytes.unsafe_set_int32 buffer 1 178293l;
      printf "%ld" (Bytes.unsafe_get_int32 buffer 1);
      [%expect {| 178293 |}]
    ;;

    let%expect_test "64-bit primitives" =
      let buffer = create 10 in
      Bytes.unsafe_set_int64 buffer 0 0x12345678_deadbeefL;
      printf "%Lx" (Bytes.unsafe_get_int64 buffer 0);
      [%expect {| 12345678deadbeef |}];
      (* Ensure that Bytes.get will retrieve the individual positions byte values as
         written by Bytes.unsafe_set_int64. *)
      for i = 0 to 7 do
        let chr = Bytes.get buffer i in
        printf "buffer[%d] = 0x%02x\n" i (Char.to_int chr)
      done;
      [%expect
        {|
        buffer[0] = 0xef
        buffer[1] = 0xbe
        buffer[2] = 0xad
        buffer[3] = 0xde
        buffer[4] = 0x78
        buffer[5] = 0x56
        buffer[6] = 0x34
        buffer[7] = 0x12
        |}];
      (* Ensure that 64-bit writes works on non-word-aligned positions. *)
      Bytes.unsafe_set_int64 buffer 1 0x12345678_deadbeefL;
      printf "%Lx" (Bytes.unsafe_get_int64 buffer 1);
      [%expect {| 12345678deadbeef |}]
    ;;
  end)
;;
