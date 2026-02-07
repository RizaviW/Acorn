// ============================================================
// Application Registers
// ============================================================
//
// This file defines the complete CSR contract between hardware and software
//
// Rules:
//   - Indexes are defined once and are authoritative
//   - Fields must not overlap
//   - Software tools parse @reg and @field annotations
//
// ============================================================

// REGMAP-BEGIN

// ------------------------------------------------------------
// CONTROL Register
// ------------------------------------------------------------

typedef struct packed {
  logic [27:0] reserved;       // @field CONTROL RESERVED 31:4
  logic [1:0]  mode;           // @field CONTROL MODE 3:2
  logic        reset;          // @field CONTROL RESET 1
  logic        start;          // @field CONTROL START 0
} control_register_t;

localparam logic [31:0] CONTROL_INDEX = 32'h0000_0000; // @reg CONTROL

// ------------------------------------------------------------
// STATUS Register
// ------------------------------------------------------------

typedef struct packed {
  logic [28:0] reserved;       // @field STATUS RESERVED 31:3
  logic        error;          // @field STATUS ERROR 2
  logic        done;           // @field STATUS DONE 1
  logic        busy;           // @field STATUS BUSY 0
} status_register_t;

localparam logic [31:0] STATUS_INDEX = 32'h0000_0001; // @reg STATUS

// REGMAP-END
