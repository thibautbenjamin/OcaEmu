open Common
open Registers
open Command
open CpuState
open Stdint

module type InOut
=sig
  val in_function : uint8 -> uint8
  val out_function : uint8 -> uint8 -> unit
end


module Cpu (Interface : InOut)
  :sig

    val emulate_cmd : cpu8080 -> int
    val fire_interrupt : cpu8080 -> int -> unit

  end
=struct

  let print_trace = false
  let print_state = false
  let step_by_step = false

  let taken = ref true


  let unimplemented_instruction cmd =
    failwith (Printf.sprintf "unimplemented_instruction %s" (string_of_cmd cmd))

  let conditionnal_op state cond f =
    match cond with
      |None -> taken := false; f ()
      |Some cond -> taken := true; test_cond state cond f

  let jmp state byte1 byte2 = fun () -> set_pc state byte1 byte2

  let call state byte1 byte2 = fun () ->
    let (pc1,pc2) = split_two_bytes state.pc in
    push state pc1 pc2;
    set_pc state byte1 byte2

  let ret state = fun() -> let (pc1,pc2) = pop state in set_pc state pc1 pc2


  let exec_cmd state cmd =
    match cmd with
    (* Data Transfer Group *)
    |MOV(r1,r2) -> set_rm state r1 (get_rmi state r2)
    |LXI(rp, byte1, byte2) -> set_rp_or_sp state rp byte1 byte2
    |LDA(byte1, byte2) -> set_register state A (read state (merge_bytes byte1 byte2))
    |STA(byte1,byte2) -> write state (get_register state A) (merge_bytes byte1 byte2)
    |LHLD(byte1,byte2) ->
      let address = merge_bytes byte1 byte2 in
      set_register_pair state HL (read state (Uint16.succ address)) (read state address)
    |SHLD(byte1, byte2) ->
      let address = merge_bytes byte1 byte2 in
      write state (get_register state H) (Uint16.succ address);
      write state (get_register state L) (address)
    |LDAX rp -> set_register state A (read state (address_at_rp state rp))
    |STAX rp -> write state (get_register state A) (address_at_rp state rp)
    |XCHG ->
      exchange_registers state D H; exchange_registers state E L
    (* Arithmetic Group *)
    |ADD r ->
      let v = get_rmi state r in
      let a = get_register state A in
      let res = Uint8.add a v in
      set_register state A res;
      set_zsp_flags state res;
      set_cy_flag state (Uint8.to_int v + Uint8.to_int a > Uint8.to_int Uint8.max_int)
    |ADC r ->
      let v = get_rmi state r in
      let a = get_register state A in
      let c = if state.condition_codes.cy then Uint8.one else Uint8.zero in
      let res = Uint8.add a (Uint8.add v c) in
      set_register state A res;
      set_zsp_flags state res;
      set_cy_flag state (Uint8.to_int v + Uint8.to_int a + Uint8.to_int c > Uint8.to_int Uint8.max_int)
    |SUB r ->
      let v = get_rmi state r in
      let a = get_register state A in
      let res = Uint8.sub a v in
      set_register state A res;
      set_zsp_flags state res;
      set_cy_flag state (compare v a > 0)
    |SBB r ->
      let v = get_rmi state r in
      let a = get_register state A in
      let c = if state.condition_codes.cy then Uint8.one else Uint8.zero in
      let res = Uint8.sub a (Uint8.add v c) in
      set_register state A res;
      set_zsp_flags state res;
      set_cy_flag state (compare (Uint8.add v c) a > 0)
    |INR r -> let newr = Uint8.succ (get_rm state r) in
      set_zsp_flags state newr;
      set_rm state r newr
    |DCR r -> let newr = Uint8.pred (get_rm state r) in
      set_zsp_flags state newr;
      set_rm state r newr
    |INX rp -> let new1,new2 = (split_two_bytes (Uint16.succ (get_rp_or_sp state rp))) in
      set_rp_or_sp state rp new1 new2
    |DCX rp -> let new1,new2 = (split_two_bytes (Uint16.pred (get_rp_or_sp state rp))) in
      set_rp_or_sp state rp new1 new2
    |DAD rp ->
      let hl = address_at_rp state HL in
      let rp = get_rp_or_sp state rp in
      let newh,newl = split_two_bytes (Uint16.add hl rp) in
      set_cy_flag state (compare (Uint16.sub Uint16.max_int hl) rp < 0);
      set_register_pair state HL newh newl;
    |DAA ->
      let a = get_register state A in
      let lsbs = Uint8.logand a (Uint8.of_int 0xf) in
      let a_inter = match compare lsbs (Uint8.of_int 9) > 0 || state.condition_codes.ac with
        |true -> Uint8.add a (Uint8.of_int 6)
        |false -> a
      in let msbs = Uint8.shift_right_logical (Uint8.logand a_inter (Uint8.of_int 0xf0)) 4 in
      let newa = match compare msbs (Uint8.of_int 9) > 0 || state.condition_codes.cy with
        |true ->
          let summand = (Uint8.shift_left (Uint8.of_int 6) 4) in
          set_cy_flag state (compare (Uint8.sub Uint8.max_int a_inter) summand < 0);
          Uint8.add a_inter (Uint8.shift_left (Uint8.of_int 6) 4)
        |false -> a_inter
      in set_zsp_flags state newa;
      set_register state A newa
    (* Logical Group *)
    |ANA r ->
      let v = get_rmi state r in
      let a = get_register state A in
      let res = Uint8.logand a v in
      set_register state A res;
      set_zsp_flags state res;
      set_cy_flag state false
    |XRA r ->
      let v = get_rmi state r in
      let a = get_register state A in
      let res = Uint8.logxor a v in
      set_register state A res;
      set_zsp_flags state res;
      set_cy_flag state false;
      set_ac_flag state false
    |ORA r ->
      let v = get_rmi state r in
      let a = get_register state A in
      let res = Uint8.logor a v in
      set_register state A res;
      set_zsp_flags state res;
      set_cy_flag state false;
      set_ac_flag state false
    |CMP r ->
      let v = get_rmi state r in
      let a = get_register state A in
      let res = Uint8.sub a v in
      set_zsp_flags state res;
      set_cy_flag state (compare v a > 0)
    |RRC ->
      let open Uint8 in
      let a = get_register state A in
      let a7 = logand a one in
      set_cy_flag state (a7 == one);
      set_register state A (logor (shift_right a 1) (shift_left a7 7))
    |RLC ->
      let open Uint8 in
      let a = get_register state A in
      let a0 = logand (shift_right a 7) one in
      set_cy_flag state (a0 == one);
      set_register state A (logor (shift_left a 1) a0)
    |RAL ->
      let open Uint8 in
      let a = get_register state A in
      let a0 = logand (shift_right a 7) one in
      set_register state A (logor (shift_left a 1) (if state.condition_codes.cy then one else zero));
      set_cy_flag state (a0 == one)
    |RAR ->
      let open Uint8 in
      let a = get_register state A in
      let a7 = logand a one in
      let carry = shift_left (if state.condition_codes.cy then one else zero) 7 in
      set_register state A (logor (shift_right a 1) carry);
      set_cy_flag state (a7 == one)
    |CMA -> set_register state A (Uint8.logxor (get_register state A) (Uint8.max_int))
    |CMC -> set_cy_flag state (not state.condition_codes.cy)
    |STC -> set_cy_flag state true
    (* Branch Group *)
    |JMP (cond, byte1, byte2) -> conditionnal_op state cond (jmp state byte1 byte2)
    |CALL (cond, byte1, byte2) -> conditionnal_op state cond (call state byte1 byte2)
    |RET cond -> conditionnal_op state cond (ret state)
    |RST _ -> unimplemented_instruction cmd
    |PCHL -> set_pc state (get_register state H) (get_register state L)
    (* Stack, I/O, Machine Group *)
    |PUSH rp -> let (v1,v2) = get_rp_or_psw state rp in push state v1 v2
    |POP rp -> let (v1,v2) = pop state in set_rp_or_psw state rp v1 v2
    |XTHL ->
      let newh = read state (Uint16.succ state.sp) in
      write state (get_register state H) (Uint16.succ state.sp);
      set_register state H newh;
      let newl = read state (state.sp) in
      write state (get_register state L) (state.sp);
      set_register state L newl
    |SPHL -> set_sp state (get_register state H) (get_register state L)
    |IN port -> set_register state A (Interface.in_function port)
    |OUT port -> Interface.out_function port (get_register state A)
    |EI -> set_int_enable state true
    |DI -> set_int_enable state false
    |HLT -> exit (0)
    |NOP -> ()

  let emulate_cmd state =
    begin
      match step_by_step with
      |true -> let _ = read_line () in ()
      |false -> ();
    end;
    let pc = (Uint16.to_int state.pc) in
    let (cmd,k) = disassemble_op (state.memory) pc
    in
    if print_trace
    then printf "%04x\t %s\n%!" pc (string_of_cmd cmd);
    state.pc <- Uint16.add state.pc (Uint16.of_int k);
    exec_cmd state cmd;
    if print_state
    then printf "%s\n" (string_of_state state);
    cycles cmd !taken

  let fire_interrupt state address =
    match state.int_enable with
    |true -> generate_interrupt state (Uint16.of_int address)
    |false -> ()


  (* let emulate state =
   *   let rec emulate_loop state =
   *     emulate_cmd state;
   *     flush_all ();
   *     begin
   *       if (timer_enabled)
   *       then
   *         if (Sys.time () -. state.last_interrupt > 1.0/.60.0 && state.int_enable)
   *         then generate_interrupt state (Uint16.succ (Uint16.one))
   *     end;
   *     emulate_loop state
   *   in emulate_loop state *)

end
