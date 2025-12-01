module reference #(
)(
  input  logic system_clock_p,
  input  logic system_clock_n,
  input  logic system_reset_n,
  input  logic PCIe_RX_n,
  input  logic PCIe_RX_p,
  output logic PCIe_TX_n,
  output logic PCIe_TX_p
);

  logic       system_clock;
  logic       system_reset;

  IBUFDS_GTE2 clock_buffer (.O(system_clock), .I(system_clock_p), .IB(system_clock_n), .CEB(1'b0), .ODIV2());
  IBUF        reset_buffer (.O(system_reset), .I(system_reset_n));

  system #(
  ) system (
    .sys_clk_0      (system_clock),
    .sys_rst_n_0    (system_reset),
    .pcie_mgt_0_rxn (PCIe_RX_n),
    .pcie_mgt_0_rxp (PCIe_RX_p),
    .pcie_mgt_0_txn (PCIe_TX_n),
    .pcie_mgt_0_txp (PCIe_TX_p)
  );

endmodule
