module CpuRun (Interface : InOut.InOut8080) : sig
  val emulate_cmd : CpuState.cpu8080 -> int
  val fire_interrupt : CpuState.cpu8080 -> int -> unit
end
