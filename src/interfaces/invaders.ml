open Common
open Stdint
open Tsdl

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

  let event = Sdl.Event.create ()

  let keymap (k:Sdl.scancode) =
    match (Sdl.Scancode.enum k)  with
    |`C -> Some Coin
    |`Left -> Some P1_left
    |`Right -> Some P1_right
    |`Space -> Some P1_fire
    |`Return -> Some P1_start
    |_ -> None


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

  let poll() =
    while Sdl.poll_event(Some event) do
      match Sdl.Event.(get event typ |> Sdl.Event.enum) with
      |`Key_down -> pressed (keymap (Sdl.Event.(get event keyboard_scancode)))
      |`Key_up -> released (keymap (Sdl.Event.(get event keyboard_scancode)))
      |`Quit -> Sdl.quit(); exit 0
      |_ -> ()
    done

  let in_function port =
    poll();
    match Uint8.to_int port with
    |0 -> Uint8.one
    |1 -> !in_port1
    |3 ->
      let v = Uint16.to_int (merge_bytes shifts.shift1 shifts.shift0)
      in Uint8.of_int ((v lsr (8 - Uint8.to_int (shifts.shift_offset))) land 0xff)
    |_ -> Uint8.zero

  let out_function port value =
    poll();
    match Uint8.to_int port with
    |2 -> shifts.shift_offset <- Uint8.logand value (Uint8.of_int 0x7)
    |4 -> shifts.shift0 <- shifts.shift1; shifts.shift1 <- value
    |_ -> ()
