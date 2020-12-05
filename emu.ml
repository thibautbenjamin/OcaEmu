open Common
open CpuState
open Cpu
open Invaders
(* open DiagInterface *)
open Tsdl

let usage = "emu [file]"

let () =
  Printexc.record_backtrace true;
  let file_in = ref [] in
  Arg.parse
    []
    (fun s -> file_in := s::!file_in)
    usage;
  let f = match !file_in with
    |[f] -> f
    |_ -> failwith "No ROM given"
  in
  let fi = open_in f in
  let inits = Sdl.Init.(video + events) in
  match Sdl.init inits with
  | Error (`Msg e) -> failwith (Printf.sprintf " SDL init: %s" e)
  | Ok () ->  let machine = init fi in
    close_in fi;
    flush_all();
    emulate machine
