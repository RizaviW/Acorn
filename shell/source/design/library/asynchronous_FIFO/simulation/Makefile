MODE           ?= sv
CORES          := $(shell sysctl -n hw.ncpu)
REPO_ROOT      := $(shell git rev-parse --show-toplevel)

export SRC_DIR := $(REPO_ROOT)/shell/source
RTL            := -f ../filelist.f

ifeq ($(MODE),cpp)
    TOP    := application
    SRCS   := $(RTL) application_testbench.cpp
    VFLAGS := --binary
else ifeq ($(MODE),sv)
    TOP    := asynchronous_FIFO_testbench
    SRCS   := $(RTL) asynchronous_FIFO_testbench.sv
    VFLAGS := --binary --timing
else
    TOP    := application_testbench
    SRCS   := $(RTL) application_testbench.sv golden_model.cpp
    VFLAGS := --binary --timing
endif

.PHONY: all view clean

all:
    verilator $(VFLAGS) -Wall --trace-fst -j $(CORES) $(SRCS) --top-module $(TOP)
    ./obj_dir/V$(TOP)

view:
    surfer trace.fst

clean:
    rm -rf obj_dir trace.fst
