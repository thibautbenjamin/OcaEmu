open Stdint

let print_string_fun = ref print_string
let print_string s = !print_string_fun s
let print_newline () = print_string "\n"
let print_endline s = print_string s; print_newline ()
let read_line_fun = ref read_line
let read_lin () = !read_line_fun ()

let string_of_byte a =
  Printf.sprintf "%02x" (Uint8.to_int a)

let printf e = Printf.ksprintf print_string e

let merge_bytes byte1 byte2 =
  Uint16.logor
    (Uint16.shift_left (Uint16.of_uint8 byte1) 8)
    (Uint16.of_uint8 byte2)

let split_two_bytes address =
  Uint8.of_uint16
    (Uint16.logand
       (Uint16.shift_right address 8) (Uint16.of_int 0xff)),
  Uint8.of_uint16 (Uint16.logand address (Uint16.of_int 0xff))
