# Project Settings
TOP  = top
RTL  = top.sv
TB   = sim.cpp
CORES = $(shell sysctl -n hw.ncpu)

.PHONY: all clean view lint

# Default target: Lint, Build, and Run
all: lint
	verilator --cc --binary -Wall --trace-fst -j $(CORES) $(RTL) $(TB) --top-module $(TOP)
	./obj_dir/V$(TOP)

lint:
	verible-verilog-lint $(RTL)

view:
	surfer trace.fst

clean:
	rm -rf obj_dir trace.fst
