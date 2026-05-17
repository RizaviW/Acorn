module round_robin_arbiter #(
  parameter int BITS = 4
)(
  // Inputs
  input  logic            clock,
  input  logic            reset,
  input  logic [BITS-1:0] request,
  // Outputs
  output logic [BITS-1:0] grant
);
  // Internal signals
  logic [2*BITS-1:0] double_request;
  logic [2*BITS-1:0] double_grant;
  logic   [BITS-1:0] priority;
  logic [2*BITS-1:0] next_priority;
  // Logic
  always_comb begin
    double_request = {request, request};
    double_grant = double_request & ~(double_request - priority);
    next_priority = double_grant << 1;
    grant = double_grant[2*BITS-1:BITS] | double_grant[BITS-1:0];
  end
  always_ff @(posedge clock) begin
    if (reset) begin
      priority <= 'b1;
    end else begin
      priority <= request ? next_priority[2*BITS-1:BITS] | next_priority[BITS-1:0] : priority;
    end
  end
endmodule
