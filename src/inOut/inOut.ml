open Stdint

module type InOut8080
= sig
  val in_function : uint8 -> uint8
  val out_function : uint8 -> uint8 -> unit
end
