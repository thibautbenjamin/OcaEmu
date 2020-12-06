export OCAMLMAKEFILE = OCamlMakefile
export PACKS = unix stdint tsdl
export ANNOTATE = true

define PROJ_disassembler
  SOURCES = common.ml cpu_i8080/registers.ml cpu_i8080/command.ml cpu_i8080/disassembler.ml
	TARGET = cpi_i8080/disassemble
endef
export PROJ_disassembler

define PROJ_cpu
  SOURCES = common.ml cpu_i8080/registers.ml cpu_i8080/command.ml cpu_i8080/cpuState.ml cpu_i8080/cpu.ml cpu_i8080/diagnostics/diagInterface.ml cpu_i8080/diagnostics/runDiagnostics.ml
  RESULT = cpu_i8080/diagnostics/diagnostics
endef
export PROJ_cpu

define PROJ_machine
  SOURCES =  common.ml cpu_i8080/registers.ml cpu_i8080/command.ml cpu_i8080/cpuState.ml cpu_i8080/cpu.ml machine/invaders.ml emu.ml
  RESULT = emu
endef
export PROJ_machine

ifndef SUBPROJS
  export SUBPROJS = cpu machine
endif

all: nc

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

