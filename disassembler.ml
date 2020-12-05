open Common
open Registers
open Command
open Stdint

let rec disassemble_loop array n =
  match n with
  |n when n < Array.length array ->
    let (cmd,k) = disassemble_op array n in
    printf "%04x\t %s\n" n (string_of_cmd cmd);
    disassemble_loop array (n+k)
  |_ -> ()

let disassemble f  =
  let fi = open_in f in
  let flen = in_channel_length fi in
  let array = Array.init (flen) (fun _ -> Uint8.of_int (input_byte fi)) in
  let _ = disassemble_loop array 0 in
  close_in fi

let usage = "disassemble [file]"

let () =
  Printexc.record_backtrace true;
  let file_in = ref [] in
  Arg.parse
    []
    (fun s -> file_in := s::!file_in)
    usage;
  let _ = match !file_in with
    | [f] -> disassemble f
    | _ -> ()
  in ()


