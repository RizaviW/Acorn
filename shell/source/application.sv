module application #(
  parameter integer                     AXIS_DATA_WIDTH   = 64,
  parameter integer                     CSR_DATA_WIDTH    = 32,
  parameter integer                     CSR_ADDRESS_WIDTH = 8
)(
  // System Signals
  input  logic                          clock,
  input  logic                          reset_n,

  // Streaming Interface - Card-to-Host (C2H)
  output logic [AXIS_DATA_WIDTH-1:0]    AXIS_C2H_tdata,
  output logic [AXIS_DATA_WIDTH/8-1:0]  AXIS_C2H_tkeep,
  output logic                          AXIS_C2H_tlast,
  input  logic                          AXIS_C2H_tready,
  output logic                          AXIS_C2H_tvalid,

  // Streaming Interface - Host-to-Card (H2C)
  input  logic [AXIS_DATA_WIDTH-1:0]    AXIS_H2C_tdata,
  input  logic [AXIS_DATA_WIDTH/8-1:0]  AXIS_H2C_tkeep,
  input  logic                          AXIS_H2C_tlast,
  output logic                          AXIS_H2C_tready,
  input  logic                          AXIS_H2C_tvalid,

  // Configuration Interface - RAM
  output logic                          CSR_RAM_valid,
  output logic                          CSR_RAM_write_enable,
  output logic [CSR_ADDRESS_WIDTH-1:0]  CSR_RAM_address,
  output logic [CSR_DATA_WIDTH-1:0]     CSR_RAM_write_data,
  input  logic [CSR_DATA_WIDTH-1:0]     CSR_RAM_read_data,

  // Configuration Interface - Registers
  input  logic                          CSR_FF_valid,
  input  logic                          CSR_FF_write_enable,
  input  logic [CSR_ADDRESS_WIDTH-1:0]  CSR_FF_address,
  input  logic [CSR_DATA_WIDTH-1:0]     CSR_FF_write_data,
  output logic [CSR_DATA_WIDTH-1:0]     CSR_FF_read_data
);

  assign AXIS_C2H_tdata       = AXIS_H2C_tdata;
  assign AXIS_C2H_tkeep       = AXIS_H2C_tkeep;
  assign AXIS_C2H_tlast       = AXIS_H2C_tlast;
  assign AXIS_C2H_tvalid      = AXIS_H2C_tvalid;
  assign AXIS_H2C_tready      = AXIS_C2H_tready;
  assign CSR_RAM_valid        = '0;
  assign CSR_RAM_write_enable = '0;
  assign CSR_RAM_address      = '0;
  assign CSR_RAM_write_data   = '0;
  assign CSR_FF_read_data     = '0;

endmodule
