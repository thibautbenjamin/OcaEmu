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
