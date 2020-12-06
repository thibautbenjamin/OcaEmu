module Cpu8080 (Interface : InOut.InOut8080) = struct

open CpuState
open CpuRun.CpuRun(Interface)
open Stdint

  let cpu = init()

  let load_file ?(offset=0) fi =
    CpuState.load_file ~offset:offset fi cpu

  let emulate_cmd () = emulate_cmd cpu

  let fire_interrupt i = fire_interrupt cpu i

  let read_video k =
    let address k = Uint16.add k (Uint16.of_int 0x2400) in
    read cpu (address (Uint16.of_int k))
end
