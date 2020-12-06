module Cpu8080 (Interface : InOut.InOut8080) : sig


  val load_file : ?offset:int -> in_channel -> unit
  val emulate_cmd : unit -> int
  val fire_interrupt : int -> unit
  val read_video : int -> Stdint.uint8

end
