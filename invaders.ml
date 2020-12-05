open Unix
open Common
open Command
open CpuState
open Stdint
open Bigarray
open Tsdl


let interrupts = true
let cpu_frequency = 2.0e6
let interrupts_frequency = 120.


module InvadersInterface : sig

  type key =
    |Coin
    |P1_left
    |P1_right
    |P1_fire
    |P1_start

  val pressed : key option -> unit
  val released : key option -> unit

  include Cpu.InOut
end =
struct

  type key =
    |Coin
    |P1_left
    |P1_right
    |P1_fire
    |P1_start

  type t =
    {
    mutable  shift0 : uint8;
    mutable  shift1 : uint8;
    mutable  shift_offset : uint8
    }

  let shifts = {
    shift0 = Uint8.zero;
    shift1 = Uint8.zero;
    shift_offset = Uint8.zero
  }

  let in_port1 = ref (Uint8.of_int 0x08)

  let value_of_key key =
    let v = match key with
      |Coin -> 0x01
      |P1_left -> 0x20
      |P1_right -> 0x40
      |P1_fire -> 0x10
      |P1_start -> 0x04
    in Uint8.of_int v

  let port key =
    match key with
    |Coin -> in_port1
    |P1_left -> in_port1
    |P1_right -> in_port1
    |P1_fire -> in_port1
    |P1_start -> in_port1

  let pressed key =
    match key with
    |None -> ()
    |Some key -> let port = port key in
      port := Uint8.logor (!port) (value_of_key key)

  let released key =
    match key with
    |None -> ()
    |Some key -> let port = port key in
      port := Uint8.logand (!port) (Uint8.lognot (value_of_key key))

  let in_function port =
    match Uint8.to_int port with
    |0 -> Uint8.one
    |1 -> !in_port1
    |3 ->
      let v = Uint16.to_int (merge_bytes shifts.shift1 shifts.shift0)
      in Uint8.of_int ((v lsr (8 - Uint8.to_int (shifts.shift_offset))) land 0xff)
    |_ -> Uint8.zero


  let out_function port value =
    match Uint8.to_int port with
    |2 -> shifts.shift_offset <- Uint8.logand value (Uint8.of_int 0x7)
    |4 -> shifts.shift0 <- shifts.shift1; shifts.shift1 <- value
    |_ -> ()

end

open Cpu.Cpu (InvadersInterface)

type machine =
  {cpu : cpu8080;

   mutable vblank : bool;

   mutable last_cmd_time : float;
   mutable cycles_since_last_interrupt : int;

   renderer : Sdl.renderer;
   texture : Sdl.texture;
   event: Sdl.event
  }

let keymap (k:Sdl.scancode) =
  let open InvadersInterface in
  match (Sdl.Scancode.enum k)  with
  |`C -> Some Coin
  |`Left -> Some P1_left
  |`Right -> Some P1_right
  |`Space -> Some P1_fire
  |`Return -> Some P1_start
  |_ -> None


let init ?(offset = 0) fi =
  let flags = Sdl.Window.(shown + mouse_focus + resizable) in
  match Sdl.create_window ~w:224 ~h:256 "Invaders" flags with
  | Error (`Msg e) -> failwith (Printf.sprintf "Create window: %s" e)
  | Ok w ->
    let flags = Sdl.Renderer.(software) in
    match Sdl.create_renderer w ~flags:flags with
    |Error (`Msg e) -> failwith (Printf.sprintf "Error while creating renderer %s" e)
    |Ok(renderer) ->
      match Sdl.create_texture renderer Sdl.Pixel.format_rgb888 Sdl.Texture.access_streaming ~w:224 ~h:256 with
      |Error (`Msg e) -> failwith ""
      |Ok(texture) ->
      {
        cpu = load_file ~offset:offset fi;
        vblank = false;
        last_cmd_time = Sys.time();
        cycles_since_last_interrupt = 0;
        renderer = renderer;
        texture = texture;
        event = Sdl.Event.create ();
      }

let create_bitmap machine bitmap =
  let address k = Uint16.add k (Uint16.of_int 0x2400) in
  for i = 0 to 223 do
    for j = 0 to 31 do
      let pix = read machine.cpu (address (Uint16.of_int (i * 32 + j))) in
      for p = 0 to 7 do
        let color =
          match pix with
          |pix when (Uint8.logand pix (Uint8.shift_left Uint8.one p) != Uint8.zero) -> 0xffffff
          |_ -> 0x000000
        in
        bitmap.{ (255 - (p + 8*j))*224 + i } <- Int32.of_int color
      done
    done
  done


let print_graphics machine =
  match Sdl.lock_texture machine.texture None Int32 with
  |Error (`Msg e) -> failwith ""
  |Ok (bitmap,_) ->
    create_bitmap machine bitmap;
    Sdl.unlock_texture (machine.texture);
      match Sdl.render_copy machine.renderer machine.texture with
      |Error (`Msg e) -> failwith ""
      |Ok () -> Sdl.render_present machine.renderer

let emulate machine =
  let rec emulate_loop cpu =
    machine.last_cmd_time <- Sys.time();
    let n_cycles = emulate_cmd cpu in
    machine.cycles_since_last_interrupt <- machine.cycles_since_last_interrupt + n_cycles;
    begin
      if (machine.cycles_since_last_interrupt >= int_of_float (cpu_frequency /. interrupts_frequency))
      then begin
        let address =
          match machine.vblank with
          |false -> 0x08
          |true -> 0x10
        in
        machine.vblank <- not machine.vblank;
        fire_interrupt machine.cpu address;
        begin
          match machine.vblank with
          |false -> print_graphics machine
          |true -> ()
        end;
        machine.cycles_since_last_interrupt <- 0;
      end
    end;
    while Sdl.poll_event(Some machine.event) do
      match Sdl.Event.(get machine.event typ |> Sdl.Event.enum) with
      |`Key_down -> InvadersInterface.pressed (keymap (Sdl.Event.(get machine.event keyboard_scancode)))
      |`Key_up -> InvadersInterface.released (keymap (Sdl.Event.(get machine.event keyboard_scancode)))
      |`Quit -> Sdl.quit(); exit 0
      |_ -> ()
    done;
    emulate_loop cpu
  in
  emulate_loop machine.cpu
