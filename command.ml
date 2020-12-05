open Common
open Registers
open Stdint

type condition =
  |NZ
  |Z
  |NC
  |C
  |PO
  |PE
  |P
  |M

type rp_or_sp =
  |RP of register_pair
  |SP

type rm =
  |R of register
  |M

type rmi =
  |R of register
  |M
  |I of uint8

type rp_or_psw =
  |RP of register_pair
  |PSW

(*
The none in register option indicates M
The none in register_pair option indicates SP
*)
type cmd =
  (* Data Transfer Group *)
  |MOV of (rm * rmi)
  |LXI of (rp_or_sp) * uint8 * uint8
  |LDA of uint8 * uint8
  |STA of uint8 * uint8
  |LHLD of uint8 * uint8
  |SHLD of uint8 * uint8
  |LDAX of register_pair
  |STAX of register_pair
  |XCHG
  (* Arithmetic Group *)
  |ADD of rmi
  |ADC of rmi
  |SUB of rmi
  |SBB of rmi
  |INR of rm
  |DCR of rm
  |INX of rp_or_sp
  |DCX of rp_or_sp
  |DAD of rp_or_sp
  |DAA
  (* Logical Group*)
  |ANA of rmi
  |ORA of rmi
  |XRA of rmi
  |CMP of rmi
  |RLC
  |RRC
  |RAR
  |RAL
  |CMA
  |CMC
  |STC
  (* Branch Group *)
  |JMP of condition option * uint8 * uint8
  |CALL of condition option * uint8 * uint8
  |RET of condition option
  |RST of uint8
  |PCHL
  (* Stack, I/O, Machine Group *)
  |POP of rp_or_psw
  |PUSH of rp_or_psw
  |OUT of uint8
  |IN of uint8
  |EI
  |DI
  |XTHL
  |SPHL
  |HLT
  |NOP

let string_of_reg_option r =
  match r with
  | None -> "M"
  | Some r -> string_of_register r

let string_of_rm (r:rm) =
  match r with
  |M -> "M"
  |R r -> string_of_register r

let string_of_rp_or_sp rp =
  match rp with
  |SP -> "SP"
  |RP rp -> string_of_register_pair rp

let string_of_rp_or_psw rp =
  match rp with
  |PSW -> "PSW"
  |RP rp -> string_of_register_pair rp

let string_of_cmd cmd=
  match cmd with
  |MOV(r1,r2) ->  begin match r2 with
      |R r2 -> Printf.sprintf "MOV %s %s" (string_of_rm r1) (string_of_register r2)
      |M -> Printf.sprintf "MOV %s M" (string_of_rm r1)
      |I byte -> Printf.sprintf "MVI %s #$%s" (string_of_rm r1) (string_of_byte byte)
    end
  |LXI(rp, byte1, byte2) -> Printf.sprintf "LXI %s #$%s%s" (string_of_rp_or_sp rp) (string_of_byte byte1) (string_of_byte byte2)
  |LDA(byte1, byte2) -> Printf.sprintf "LDA $%s%s" (string_of_byte byte1) (string_of_byte byte2)
  |STA(byte1, byte2) -> Printf.sprintf "STA $%s%s" (string_of_byte byte1) (string_of_byte byte2)
  |LHLD(byte1, byte2) -> Printf.sprintf "LHLD $%s%s" (string_of_byte byte1) (string_of_byte byte2)
  |SHLD(byte1, byte2) -> Printf.sprintf "SHLD $%s%s" (string_of_byte byte1) (string_of_byte byte2)
  |LDAX(rp) -> Printf.sprintf "LDAX %s" (string_of_register_pair rp)
  |STAX(rp) -> Printf.sprintf "STAX %s" (string_of_register_pair rp)
  |XCHG -> "XCHG"
  |ADD r ->
    begin
      match r with
      |R r -> Printf.sprintf "ADD %s" (string_of_register r)
      |M -> Printf.sprintf "ADD M"
      |I byte -> Printf.sprintf "ADI %s" (string_of_byte byte)
    end
  |ADC r ->
    begin
      match r with
      |R r -> Printf.sprintf "ADC %s" (string_of_register r)
      |M -> Printf.sprintf "ADC M"
      |I byte -> Printf.sprintf "ACI %s" (string_of_byte byte)
    end
  |SUB r ->
    begin
      match r with
      |R r -> Printf.sprintf "SUB %s" (string_of_register r)
      |M -> Printf.sprintf "SUB M"
      |I byte -> Printf.sprintf "SUI %s" (string_of_byte byte)
    end
  |SBB r ->
    begin
      match r with
      |R r -> Printf.sprintf "SBB %s" (string_of_register r)
      |M -> Printf.sprintf "SBB M"
      |I byte -> Printf.sprintf "SBI %s" (string_of_byte byte)
    end
  |INR r -> Printf.sprintf "INR %s" (string_of_rm r)
  |DCR r -> Printf.sprintf "DCR %s" (string_of_rm r)
  |INX rp -> Printf.sprintf "INX %s" (string_of_rp_or_sp rp)
  |DCX rp -> Printf.sprintf "DCX %s" (string_of_rp_or_sp rp)
  |DAD rp -> Printf.sprintf "DAD %s" (string_of_rp_or_sp rp)
  |DAA -> "DAA"
  |ANA r ->
    begin
      match r with
      |R r -> Printf.sprintf "ANA %s" (string_of_register r)
      |M -> Printf.sprintf "ANA M"
      |I byte -> Printf.sprintf "ANI %s" (string_of_byte byte)
    end
  |ORA r ->
    begin
      match r with
      |R r -> Printf.sprintf "ORA %s" (string_of_register r)
      |M -> Printf.sprintf "ORA M"
      |I byte -> Printf.sprintf "ORI %s" (string_of_byte byte)
    end
  |XRA r ->
    begin
      match r with
      |R r -> Printf.sprintf "XRA %s" (string_of_register r)
      |M -> Printf.sprintf "XRA M"
      |I byte -> Printf.sprintf "XRI %s" (string_of_byte byte)
    end
  |CMP r ->
    begin
      match r with
      |R r -> Printf.sprintf "CMP %s" (string_of_register r)
      |M -> Printf.sprintf "CMP M"
      |I byte -> Printf.sprintf "CPI %s" (string_of_byte byte)
    end
  |RLC -> "RLC"
  |RRC -> "RRC"
  |RAR -> "RAR"
  |RAL -> "RAL"
  |CMA -> "CMA"
  |CMC -> "CMC"
  |STC -> "STC"
  |JMP (cond, byte1, byte2) ->
    begin
      match cond with
      |None -> Printf.sprintf "JMP $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some NZ -> Printf.sprintf "JNZ $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some Z -> Printf.sprintf "JZ $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some NC -> Printf.sprintf "JNC $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some C -> Printf.sprintf "JC $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some PO -> Printf.sprintf "JPO $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some PE -> Printf.sprintf "JPE $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some P -> Printf.sprintf "JP $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some M -> Printf.sprintf "JM $%s%s" (string_of_byte byte1) (string_of_byte byte2)
    end
  |CALL (cond, byte1, byte2) ->
    begin
      match cond with
      |None -> Printf.sprintf "CALL $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some NZ -> Printf.sprintf "CNZ $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some Z -> Printf.sprintf "CZ $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some NC -> Printf.sprintf "CNC $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some C -> Printf.sprintf "CC $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some PO -> Printf.sprintf "CPO $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some PE -> Printf.sprintf "CPE $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some P -> Printf.sprintf "CP $%s%s" (string_of_byte byte1) (string_of_byte byte2)
      |Some M -> Printf.sprintf "CM $%s%s" (string_of_byte byte1) (string_of_byte byte2)
    end
  |RET cond ->
    begin
      match cond with
      |None -> Printf.sprintf "RET"
      |Some NZ -> Printf.sprintf "RNZ"
      |Some Z -> Printf.sprintf "RZ"
      |Some NC -> Printf.sprintf "RNC"
      |Some C -> Printf.sprintf "RC"
      |Some PO -> Printf.sprintf "RPO"
      |Some PE -> Printf.sprintf "RPE"
      |Some P -> Printf.sprintf "RP"
      |Some M -> Printf.sprintf "RM"
    end
  |RST i -> Printf.sprintf "RST %i" (Uint8.to_int i)
  |PCHL -> "PCHL"
  |POP rp -> Printf.sprintf "POP %s" (string_of_rp_or_psw rp)
  |PUSH rp -> Printf.sprintf "PUSH %s" (string_of_rp_or_psw rp)
  |OUT byte -> Printf.sprintf "OUT #$%s" (string_of_byte byte)
  |IN byte -> Printf.sprintf "IN #$%s" (string_of_byte byte)
  |EI -> "EI"
  |DI -> "DI"
  |XTHL -> "XTHL"
  |SPHL -> "SPHL"
  |HLT -> "HLT"
  |NOP -> "NOP"

let disassemble_op (array : uint8 array) n =
  let k = ref 0 in
  let read_next () =
    let res = Array.get array (n + !k) in
    k := !k + 1; res
  in
  let c = read_next () in
  let cmd =
  match Uint8.to_int c with
  |0x00 -> NOP
  |0x01 -> LXI (RP BC, read_next (), read_next ())
  |0x02 -> STAX BC
  |0x03 -> INX (RP BC)
  |0x04 -> INR (R B)
  |0x05 -> DCR (R B)
  |0x06 -> MOV (R B, I (read_next ()))
  |0x07 -> RLC
  |0x08 -> NOP
  |0x09 -> DAD (RP BC)
  |0x0a -> LDAX BC
  |0x0b -> DCX (RP BC)
  |0x0c -> INR (R C)
  |0x0d -> DCR (R C)
  |0x0e -> MOV (R C, I (read_next ()))
  |0x0f -> RRC
  |0x10 -> NOP
  |0x11 -> LXI (RP DE, read_next (), read_next ())
  |0x12 -> STAX DE
  |0x13 -> INX (RP DE)
  |0x14 -> INR (R D)
  |0x15 -> DCR (R D)
  |0x16 -> MOV (R D, I (read_next ()))
  |0x17 -> RAL
  |0x18 -> NOP
  |0x19 -> DAD (RP DE)
  |0x1a -> LDAX DE
  |0x1b -> DCX (RP DE)
  |0x1c -> INR (R E)
  |0x1d -> DCR (R E)
  |0x1e -> MOV (R E, I (read_next ()))
  |0x1f -> RAR
  |0x20 -> NOP
  |0x21 -> LXI (RP HL, read_next (), read_next ())
  |0x22 -> SHLD (read_next (), read_next ())
  |0x23 -> INX (RP HL)
  |0x24 -> INR (R H)
  |0x25 -> DCR (R H)
  |0x26 -> MOV (R H, I (read_next ()))
  |0x27 -> DAA
  |0x28 -> NOP
  |0x29 -> DAD (RP HL)
  |0x2a -> LHLD (read_next (), read_next ())
  |0x2b -> DCX (RP HL)
  |0x2c -> INR (R L)
  |0x2d -> DCR (R L)
  |0x2e -> MOV (R L, I (read_next ()))
  |0x2f -> CMA
  |0x30 -> NOP
  |0x31 -> LXI (SP, read_next (), read_next ())
  |0x32 -> STA (read_next (), read_next ())
  |0x33 -> INX SP
  |0x34 -> INR M
  |0x35 -> DCR M
  |0x36 -> MOV (M, I (read_next ()))
  |0x37 -> STC
  |0x38 -> NOP
  |0x39 -> DAD SP
  |0x3a -> LDA (read_next (), read_next ())
  |0x3b -> DCX SP
  |0x3c -> INR (R A)
  |0x3d -> DCR (R A)
  |0x3e -> MOV (R A, I (read_next ()))
  |0x3f -> CMC
  |0x40 -> MOV (R B, R B)
  |0x41 -> MOV (R B, R C)
  |0x42 -> MOV (R B, R D)
  |0x43 -> MOV (R B, R E)
  |0x44 -> MOV (R B, R H)
  |0x45 -> MOV (R B, R L)
  |0x46 -> MOV (R B, M)
  |0x47 -> MOV (R B, R A)
  |0x48 -> MOV (R C, R B)
  |0x49 -> MOV (R C, R C)
  |0x4a -> MOV (R C, R D)
  |0x4b -> MOV (R C, R E)
  |0x4c -> MOV (R C, R H)
  |0x4d -> MOV (R C, R L)
  |0x4e -> MOV (R C, M)
  |0x4f -> MOV (R C, R A)
  |0x50 -> MOV (R D, R B)
  |0x51 -> MOV (R D, R C)
  |0x52 -> MOV (R D, R D)
  |0x53 -> MOV (R D, R E)
  |0x54 -> MOV (R D, R H)
  |0x55 -> MOV (R D, R L)
  |0x56 -> MOV (R D, M)
  |0x57 -> MOV (R D, R A)
  |0x58 -> MOV (R E, R B)
  |0x59 -> MOV (R E, R C)
  |0x5a -> MOV (R E, R D)
  |0x5b -> MOV (R E, R E)
  |0x5c -> MOV (R E, R H)
  |0x5d -> MOV (R E, R L)
  |0x5e -> MOV (R E, M)
  |0x5f -> MOV (R E, R A)
  |0x60 -> MOV (R H, R B)
  |0x61 -> MOV (R H, R C)
  |0x62 -> MOV (R H, R D)
  |0x63 -> MOV (R H, R E)
  |0x64 -> MOV (R H, R H)
  |0x65 -> MOV (R H, R L)
  |0x66 -> MOV (R H, M)
  |0x67 -> MOV (R H, R A)
  |0x68 -> MOV (R L, R B)
  |0x69 -> MOV (R L, R C)
  |0x6a -> MOV (R L, R D)
  |0x6b -> MOV (R L, R E)
  |0x6c -> MOV (R L, R H)
  |0x6d -> MOV (R L, R L)
  |0x6e -> MOV (R L, M)
  |0x6f -> MOV (R L, R A)
  |0x70 -> MOV (M, R B)
  |0x71 -> MOV (M, R C)
  |0x72 -> MOV (M, R D)
  |0x73 -> MOV (M, R E)
  |0x74 -> MOV (M, R H)
  |0x75 -> MOV (M, R L)
  |0x76 -> HLT
  |0x77 -> MOV (M, R A)
  |0x78 -> MOV (R A, R B)
  |0x79 -> MOV (R A, R C)
  |0x7a -> MOV (R A, R D)
  |0x7b -> MOV (R A, R E)
  |0x7c -> MOV (R A, R H)
  |0x7d -> MOV (R A, R L)
  |0x7e -> MOV (R A, M)
  |0x7f -> MOV (R A, R A)
  |0x80 -> ADD (R B)
  |0x81 -> ADD (R C)
  |0x82 -> ADD (R D)
  |0x83 -> ADD (R E)
  |0x84 -> ADD (R H)
  |0x85 -> ADD (R L)
  |0x86 -> ADD M
  |0x87 -> ADD (R A)
  |0x88 -> ADC (R B)
  |0x89 -> ADC (R C)
  |0x8a -> ADC (R D)
  |0x8b -> ADC (R E)
  |0x8c -> ADC (R H)
  |0x8d -> ADC (R L)
  |0x8e -> ADC M
  |0x8f -> ADC (R A)
  |0x90 -> SUB (R B)
  |0x91 -> SUB (R C)
  |0x92 -> SUB (R D)
  |0x93 -> SUB (R E)
  |0x94 -> SUB (R H)
  |0x95 -> SUB (R L)
  |0x96 -> SUB M
  |0x97 -> SUB (R A)
  |0x98 -> SBB (R B)
  |0x99 -> SBB (R C)
  |0x9a -> SBB (R D)
  |0x9b -> SBB (R E)
  |0x9c -> SBB (R H)
  |0x9d -> SBB (R L)
  |0x9e -> SBB M
  |0x9f -> SBB (R A)
  |0xa0 -> ANA (R B)
  |0xa1 -> ANA (R C)
  |0xa2 -> ANA (R D)
  |0xa3 -> ANA (R E)
  |0xa4 -> ANA (R H)
  |0xa5 -> ANA (R L)
  |0xa6 -> ANA M
  |0xa7 -> ANA (R A)
  |0xa8 -> XRA (R B)
  |0xa9 -> XRA (R C)
  |0xaa -> XRA (R D)
  |0xab -> XRA (R E)
  |0xac -> XRA (R H)
  |0xad -> XRA (R L)
  |0xae -> XRA M
  |0xaf -> XRA (R A)
  |0xb0 -> ORA (R B)
  |0xb1 -> ORA (R C)
  |0xb2 -> ORA (R D)
  |0xb3 -> ORA (R E)
  |0xb4 -> ORA (R H)
  |0xb5 -> ORA (R L)
  |0xb6 -> ORA M
  |0xb7 -> ORA (R A)
  |0xb8 -> CMP (R B)
  |0xb9 -> CMP (R C)
  |0xba -> CMP (R D)
  |0xbb -> CMP (R E)
  |0xbc -> CMP (R H)
  |0xbd -> CMP (R L)
  |0xbe -> CMP M
  |0xbf -> CMP (R A)
  |0xc0 -> RET (Some NZ)
  |0xc1 -> POP (RP BC)
  |0xc2 -> JMP (Some NZ, read_next (), read_next ())
  |0xc3 -> JMP (None, read_next (), read_next ())
  |0xc4 -> CALL (Some NZ, read_next (), read_next ())
  |0xc5 -> PUSH (RP BC)
  |0xc6 -> ADD (I (read_next ()))
  |0xc7 -> RST Uint8.zero
  |0xc8 -> RET (Some Z)
  |0xc9 -> RET None
  |0xca -> JMP (Some Z, read_next (), read_next ())
  |0xcb -> NOP
  |0xcc -> CALL (Some Z, read_next (), read_next ())
  |0xcd -> CALL (None, read_next (), read_next ())
  |0xce -> ADC (I (read_next ()))
  |0xcf -> RST Uint8.one
  |0xd0 -> RET (Some NC)
  |0xd1 -> POP (RP DE)
  |0xd2 -> JMP (Some NC, read_next (), read_next ())
  |0xd3 -> OUT (read_next ())
  |0xd4 -> CALL (Some NC, read_next (), read_next ())
  |0xd5 -> PUSH (RP DE)
  |0xd6 -> SUB (I (read_next ()))
  |0xd7 -> RST (Uint8.of_int 2)
  |0xd8 -> RET (Some C)
  |0xd9 -> NOP
  |0xda -> JMP (Some C, read_next (), read_next ())
  |0xdb -> IN (read_next ())
  |0xdc -> CALL (Some C, read_next (), read_next ())
  |0xdd -> NOP
  |0xde -> SBB (I (read_next ()))
  |0xdf -> RST (Uint8.of_int 3)
  |0xe0 -> RET (Some PO)
  |0xe1 -> POP (RP HL)
  |0xe2 -> JMP (Some PO, read_next (), read_next ())
  |0xe3 -> XTHL
  |0xe4 -> CALL (Some PO, read_next (), read_next ())
  |0xe5 -> PUSH (RP HL)
  |0xe6 -> ANA (I (read_next ()))
  |0xe7 -> RST (Uint8.of_int 4)
  |0xe8 -> RET (Some PE)
  |0xe9 -> PCHL
  |0xea -> JMP (Some PE, read_next (), read_next ())
  |0xeb -> XCHG
  |0xec -> CALL (Some PE, read_next (), read_next ())
  |0xed -> NOP
  |0xee -> XRA (I (read_next ()))
  |0xef -> RST (Uint8.of_int 5)
  |0xf0 -> RET (Some P)
  |0xf1 -> POP PSW
  |0xf2 -> JMP (Some P, read_next (), read_next ())
  |0xf3 -> DI
  |0xf4 -> CALL (Some P,read_next (), read_next ())
  |0xf5 -> PUSH PSW
  |0xf6 -> ORA (I (read_next ()))
  |0xf7 -> RST (Uint8.of_int 6)
  |0xf8 -> RET (Some M)
  |0xf9 -> SPHL
  |0xfa -> JMP (Some M, read_next (), read_next ())
  |0xfb -> EI
  |0xfc -> CALL(Some M, read_next (), read_next ())
  |0xfd -> NOP
  |0xfe -> CMP (I (read_next ()))
  |0xff -> RST (Uint8.of_int 7)
  |_ -> failwith "Command not recognized"
  in cmd, !k


(*TODO : handle the various lengths for the conditionals *)
let cycles cmd taken =
  match cmd with
  |MOV (r1,r2) ->
    begin
      match r1,r2 with
      |R _,R _ -> 5
      |R _,M -> 7
      |M, R _ -> 7
      |M, M -> failwith "Command not recognized"
      |R _, I _ -> 7
      |M, I _ -> 10
    end
  |LXI _ -> 10
  |LDA _ -> 13
  |STA _ -> 13
  |LHLD _ -> 16
  |SHLD _ -> 16
  |LDAX _ -> 7
  |STAX _ -> 7
  |XCHG -> 4
  |ADD r ->
    begin
      match r with
      |R _ -> 4
      |M -> 7
      |I _ -> 7
    end
  |ADC r ->
    begin
      match r with
      |R _ -> 4
      |M -> 7
      |I _ -> 7
    end
  |SUB r ->
    begin
      match r with
      |R _ -> 4
      |M -> 7
      |I _ -> 7
    end
  |SBB r ->
    begin
      match r with
      |R _ -> 4
      |M -> 7
      |I _ -> 7
    end
  |INR r ->
    begin
      match r with
      |R _ -> 5
      |M -> 10
    end
  |DCR r ->
    begin
      match r with
      |R _ -> 5
      |M -> 10
    end
  |INX _ -> 5
  |DCX _ -> 5
  |DAD _ -> 10
  |DAA -> 4
  |ANA r ->
    begin
      match r with
      |R _ -> 4
      |M -> 7
      |I _ -> 7
    end
  |ORA r ->
    begin
      match r with
      |R _ -> 4
      |M -> 7
      |I _ -> 7
    end
  |XRA r ->
    begin
      match r with
      |R _ -> 4
      |M -> 7
      |I _ -> 7
    end
  |CMP r ->
    begin
      match r with
      |R _ -> 4
      |M -> 7
      |I _ -> 7
    end
  |RLC -> 4
  |RRC -> 4
  |RAR -> 4
  |RAL -> 4
  |CMA -> 4
  |CMC -> 4
  |STC -> 4
  |JMP _ -> 10
  |CALL _ -> if taken then 17 else 11
  |RET cond ->
    begin
      match cond with
      |None -> 10
      |Some _ -> if taken then 11 else 5
    end
  |RST _ -> 11
  |PCHL -> 5
  |POP _ -> 10
  |PUSH _ -> 11
  |OUT _ -> 10
  |IN _ -> 10
  |EI -> 4
  |DI -> 4
  |XTHL -> 18
  |SPHL -> 5
  |HLT -> 7
  |NOP -> 4
