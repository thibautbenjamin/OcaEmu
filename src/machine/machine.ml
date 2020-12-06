open Stdint
open Bigarray
open InOut
open Tsdl
open Cpu8080

module Machine (Interface : InOut8080) = struct

  let interrupts = true
  let cpu_frequency = 2.0e6
  let interrupts_frequency = 120.

  open Cpu8080(Interface)

  type machine =
    {
     mutable vblank : bool;

     mutable last_cmd_time : float;
     mutable cycles_since_last_interrupt : int;

     renderer : Sdl.renderer;
     texture : Sdl.texture;
    }


  let init ?(offset = 0) fi =
    load_file ~offset:offset fi;
    let flags = Sdl.Window.(shown + mouse_focus + resizable) in
    match Sdl.create_window ~w:224 ~h:256 "Invaders" flags with
    | Error (`Msg e) -> failwith (Printf.sprintf "Create window: %s" e)
    | Ok w ->
      let flags = Sdl.Renderer.(software) in
      match Sdl.create_renderer w ~flags:flags with
      |Error (`Msg e) -> failwith (Printf.sprintf "Error while creating renderer %s" e)
      |Ok(renderer) ->
        match Sdl.create_texture renderer Sdl.Pixel.format_rgb888 Sdl.Texture.access_streaming ~w:224 ~h:256 with
        |Error (`Msg e) -> failwith (Printf.sprintf "failed to create the texture %s" e)
        |Ok(texture) ->
          {
            vblank = false;
            last_cmd_time = Sys.time();
            cycles_since_last_interrupt = 0;
            renderer = renderer;
            texture = texture;
          }

  let create_bitmap bitmap =
    for i = 0 to 223 do
      for j = 0 to 31 do
        let pix = read_video (i * 32 + j) in
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
    |Error (`Msg e) -> failwith (Printf.sprintf "Failed to lock texture %s" e)
    |Ok (bitmap,_) ->
      create_bitmap bitmap;
      Sdl.unlock_texture (machine.texture);
      match Sdl.render_copy machine.renderer machine.texture with
      |Error (`Msg e) -> failwith (Printf.sprintf "failed to render copy %s" e)
      |Ok () -> Sdl.render_present machine.renderer

  let emulate machine =
    let rec emulate_loop () =
      machine.last_cmd_time <- Sys.time();
      let n_cycles = emulate_cmd () in
      machine.cycles_since_last_interrupt <- machine.cycles_since_last_interrupt + n_cycles;
      begin
        if (machine.cycles_since_last_interrupt >= int_of_float (cpu_frequency /. interrupts_frequency))
        then
          begin
            let address =
              match machine.vblank with
              |false -> 0x08
              |true -> 0x10
            in
            machine.vblank <- not machine.vblank;
            fire_interrupt address;
            begin
              match machine.vblank with
              |false -> print_graphics machine
              |true -> ()
            end;
            machine.cycles_since_last_interrupt <- 0;
          end;
      end;
      Unix.sleepf (machine.last_cmd_time +. (float n_cycles) /. cpu_frequency -. Sys.time());
      emulate_loop ()
    in
    emulate_loop ()
end
