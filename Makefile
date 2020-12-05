# PACKS = stdint
# SOURCES =  common.ml registers.ml command.ml disassembler.ml
# RESULT = disassemble
# ANNOTATE = true

# PACKS = unix stdint graphics
# SOURCES =  common.ml registers.ml command.ml cpuState.ml cpu.ml diagInterface.ml emu.ml
# RESULT = emu
# ANNOTATE = true

PACKS = unix stdint graphics tsdl
SOURCES =  common.ml registers.ml command.ml cpuState.ml cpu.ml invaders.ml emu.ml
RESULT = emu
ANNOTATE = true

all: nc

include OCamlMakefile
