open Common
open Registers
open Command
open Stdint

type cc = {
  mutable cy : bool;
  mutable p : bool;
  mutable ac : bool;
  mutable z : bool;
  mutable s : bool;
  mutable pad : int;
  mutable pad2 : int;
  mutable pad3 : int
}

type registers =
  {
    mutable a : uint8;
    mutable b : uint8;
    mutable c : uint8;
    mutable d : uint8;
    mutable e : uint8;
    mutable h : uint8;
    mutable l : uint8
  }

type cpu8080 = {
  registers_state : registers;
  mutable pc : uint16;
  mutable sp : uint16;
  mutable memory : uint8 array;
  mutable int_enable: bool;
  condition_codes : cc;
}

let string_of_registers registers =
  Printf.sprintf "A=%s B=%s C=%s D=%s E=%s H=%s L=%s"
    (string_of_byte registers.a)
    (string_of_byte registers.b)
    (string_of_byte registers.c)
    (string_of_byte registers.d)
    (string_of_byte registers.e)
    (string_of_byte registers.h)
    (string_of_byte registers.l)

let string_of_condition_codes cc =
  Printf.sprintf "z=%b s=%b p=%b cy=%b" cc.z cc.s cc.p cc.cy

let string_of_state state =
  Printf.sprintf "%s | pc=%04x sp=%04x | %s" (string_of_registers state.registers_state) (Uint16.to_int state.pc) (Uint16.to_int state.sp) (string_of_condition_codes state.condition_codes)

let init () =
  let o8 = Uint8.zero and o16 = Uint16.zero in
  {
    registers_state = {a = o8; b = o8; c = o8; d = o8; e = o8; h = o8; l = o8}; (*  *)
    pc = o16;
    sp = o16;
    memory = Array.make (16 * 0x1000) o8;
    int_enable = false;
    condition_codes = {cy = false; p = false; ac = false; z = false; s = false; pad = 0; pad2 = 0; pad3 = 0}
  }

let load_file ?(offset = 0) fi =
  let state = init () in
  let rec load_file_loop fi i =
    let read_char =
      try (Some (Array.set state.memory i (Uint8.of_int (input_byte fi))))
      with |End_of_file -> None
    in match read_char with
    |Some () -> (load_file_loop fi (i+1))
    |None -> ()
  in load_file_loop fi offset; state

let set_register state register value =
  match register with
  |A -> state.registers_state.a <- value
  |B -> state.registers_state.b <- value
  |C -> state.registers_state.c <- value
  |D -> state.registers_state.d <- value
  |E -> state.registers_state.e <- value
  |H -> state.registers_state.h <- value
  |L -> state.registers_state.l <- value

let get_register state register =
  match register with
  |A -> state.registers_state.a
  |B -> state.registers_state.b
  |C -> state.registers_state.c
  |D -> state.registers_state.d
  |E -> state.registers_state.e
  |H -> state.registers_state.h
  |L -> state.registers_state.l

let exchange_registers state r1 r2 =
  let newr1 = get_register state r2 in
  set_register state r2 (get_register state r1);
  set_register state r1 newr1

let set_register_pair state rp value1 value2 =
  set_register state (fst_register rp) value1;
  set_register state (snd_register rp) value2

let merge_bytes byte1 byte2 =
  Uint16.logor
    (Uint16.shift_left (Uint16.of_uint8 byte1) 8)
    (Uint16.of_uint8 byte2)

let split_two_bytes address =
  Uint8.of_uint16
  (Uint16.logand
     (Uint16.shift_right address 8) (Uint16.of_int 0xff)),
  Uint8.of_uint16 (Uint16.logand address (Uint16.of_int 0xff))

let set_pc state byte1 byte2 =
  state.pc <- merge_bytes byte1 byte2

let set_sp state byte1 byte2 =
  state.sp <- merge_bytes byte1 byte2

let set_int_enable state b =
  state.int_enable <- b

let write state byte address =
  let address = Uint16.to_int address in
  match address with
  (* |n when n < 0x2000 -> failwith "Trying to write into the ROM" *)
  |_ -> Array.set state.memory address byte

let push state byte1 byte2 =
  let push1 byte =
    state.sp <- Uint16.pred state.sp;
    write state byte state.sp
  in push1 byte1; push1 byte2

let read state address =
  Array.get state.memory (Uint16.to_int address)

let pop state =
  let pop1 () =
    let res = read state (state.sp) in
    state.sp <- Uint16.succ state.sp;
    res
  in (pop1(), pop1())


let address_at_rp state rp =
  let (r1,r2) = (fst_register rp, snd_register rp) in
  merge_bytes (get_register state r1) (get_register state r2)

let test_cond state cond f =
  match cond with
  |Z -> if state.condition_codes.z then f()
  |NZ -> if not state.condition_codes.z then f()
  |C -> if state.condition_codes.cy then f()
  |NC -> if not state.condition_codes.cy then f()
  |PO -> if not state.condition_codes.p then f()
  |PE -> if state.condition_codes.p then f()
  |P -> if not state.condition_codes.s then f()
  |M -> if state.condition_codes.s then f()


let set_zsp_flags state (x:uint8) =
  let open Uint8 in
  state.condition_codes.z <- (compare x zero == 0);
  state.condition_codes.s <- logand (shift_right x 7) one == one;
    let bit pos =  logand (shift_right x pos) one in
    let bits = ref [] in
    for i=0 to 7 do
      bits := (bit i) :: (!bits)
    done;
    state.condition_codes.p <- (List.fold_left logxor zero !bits == zero)


let set_cy_flag state b =
  state.condition_codes.cy <- b

let set_ac_flag state b =
  state.condition_codes.ac <- b

let set_rp_or_sp state rp byte1 byte2 =
  match rp with
  |SP -> set_sp state byte1 byte2
  |RP rp -> set_register_pair state rp byte1 byte2

let get_rp_or_sp state rp =
  match rp with
  |SP -> state.sp
  |RP rp -> merge_bytes (get_register state (fst_register rp)) (get_register state (snd_register rp))

let make_psw state =
  let open Uint8 in
  let byte_of b =
    match b with
    |true -> one
    |false -> zero
  in
  let w = ref []
  in
  w := byte_of (state.condition_codes.cy) :: !w;
  w := (shift_left one 1) :: !w;
  w := (shift_left (byte_of (state.condition_codes.p)) 2) :: !w;
  w := (shift_left zero 3) :: !w;
  w := (shift_left (byte_of (state.condition_codes.ac)) 4) :: !w;
  w := (shift_left zero 5) :: !w;
  w := (shift_left (byte_of (state.condition_codes.z)) 6) :: !w;
  w := (shift_left (byte_of (state.condition_codes.s)) 7) :: !w;
  List.fold_left logor zero !w


let read_psw state psw =
  let open Uint8 in
  let read_bit w pos =
    (logand psw (shift_left one pos) == shift_left one pos)
  in
  state.condition_codes.cy <- read_bit psw 0;
  state.condition_codes.p <- read_bit psw 2;
  state.condition_codes.ac <- read_bit psw 4;
  state.condition_codes.z <- read_bit psw 6;
  state.condition_codes.s <- read_bit psw 7


let generate_interrupt state address =
  let (pc1,pc2) = split_two_bytes state.pc in
  push state pc1 pc2;
  state.pc <- address;
  state.int_enable <- false

let get_rm state (rm : rm) =
  match rm with
  |M -> read state (address_at_rp state HL)
  |R r -> get_register state r

let get_rmi state rmi =
  match rmi with
  |M -> read state (address_at_rp state HL)
  |R r -> get_register state r
  |I byte -> byte

let set_rm state (rm:rm) value =
  match rm with
  |M -> write state value (address_at_rp state HL)
  |R r -> set_register state r value

let get_rp_or_psw state rp =
  match rp with
  |RP rp -> get_register state (fst_register rp), get_register state (snd_register rp)
  |PSW -> get_register state A, make_psw state

let set_rp_or_psw state rp v1 v2 =
  match rp with
  |RP rp -> set_register_pair state rp v1 v2
  |PSW -> set_register state A v1; read_psw state v2
