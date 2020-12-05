open Common
open Command
open CpuState
open Stdint


module DiagInterface : sig

  include Cpu.InOut
end =
struct
  let in_function port = Uint8.zero

  let out_function port value = ()

end

open Cpu.Cpu (DiagInterface)

type machine =
  {
    cpu : cpu8080;
  }

let init fi =
  let a = { cpu = load_file ~offset:0x100 fi }
  in a.cpu.pc <- Uint16.of_int 0x100;
  a.cpu.memory.(0x01ad) <- Uint8.of_int(0x7);
  a.cpu.memory.(0x059c) <- Uint8.of_int(0xc3);
  a.cpu.memory.(0x59d) <- Uint8.of_int(0xc2);
  a.cpu.memory.(0x59e) <- Uint8.of_int(0x05);
  a

let emulate machine =
  let rec emulate_loop cpu =
    begin
    if (Uint16.to_int cpu.pc == 0x689) then
      failwith "error reached here!"
    end;
    let cmd,_ = disassemble_op cpu.memory (Uint16.to_int cpu.pc) in
    begin
      match cmd with
      |CALL(None,_,_) -> if(cpu.registers_state.c == Uint8.of_int 9) then
          let offset = ref( Uint16.to_int (merge_bytes (cpu.registers_state.d) (cpu.registers_state.e))) in
          let c = ref (char_of_int (Uint8.to_int (cpu.memory.(!offset+3)))) in
          while !c != '$' do
            offset := !offset+1;
            c := char_of_int (Uint8.to_int (cpu.memory.(!offset+3)));
            printf "%c" !c;
          done;
          printf "\n%!"
      |NOP -> exit 0
      |_ -> ()
    end;
    let _ = emulate_cmd cpu in
    emulate_loop cpu
  in
  emulate_loop machine.cpu
