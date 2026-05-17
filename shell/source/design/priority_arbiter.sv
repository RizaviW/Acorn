module priority_arbiter #(
  parameter int BITS = 4
)(
  // Inputs
  input  logic [BITS-1:0] request,
  // Outputs
  output logic [BITS-1:0] grant
);
  // Logic
  assign grant = request & -request;
endmodule
