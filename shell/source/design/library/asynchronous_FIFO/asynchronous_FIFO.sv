module asynchronous_FIFO #(
  parameter int                 DATA_WIDTH = 32,
  parameter int                 DEPTH = 32 // Power of 2
)(
  input  logic                  write_clock,
  input  logic                  read_clock,
  input  logic                  write_reset_n,
  input  logic                  read_reset_n,
  input  logic                  write_enable,
  input  logic [DATA_WIDTH-1:0] write_data,
  input  logic                  read_enable,
  output logic [DATA_WIDTH-1:0] read_data,
  output logic                  full,
  output logic                  empty
);
  localparam int POINTER_WIDTH = $clog2(DEPTH);

  logic [DATA_WIDTH-1:0]  buffer [DEPTH];
  logic [POINTER_WIDTH:0] write_pointer, read_pointer;
  logic [POINTER_WIDTH:0] gray_write_pointer, gray_read_pointer;
  logic [POINTER_WIDTH:0] pre_gray_write_pointer, pre_gray_read_pointer;
  logic [POINTER_WIDTH:0] post_gray_write_pointer, post_gray_read_pointer;
  logic [POINTER_WIDTH:0] crossed_gray_write_pointer, crossed_gray_read_pointer;

  always_ff @(posedge write_clock) begin
    if (~write_reset_n) begin
      write_pointer <= '0;
      pre_gray_write_pointer <= '0;
      post_gray_read_pointer <= '0;
      crossed_gray_read_pointer <= '0;
    end else begin
      if (write_enable && ~full) begin
        buffer[write_pointer[POINTER_WIDTH-1:0]] <= write_data;
        write_pointer <= write_pointer + 'b1;
      end
      pre_gray_write_pointer <= gray_write_pointer;
      post_gray_read_pointer <= pre_gray_read_pointer;
      crossed_gray_read_pointer <= post_gray_read_pointer;
    end
  end

  always_comb begin
    gray_write_pointer = write_pointer ^ (write_pointer >> 1);
    full = gray_write_pointer[POINTER_WIDTH] != crossed_gray_read_pointer[POINTER_WIDTH] &&
           gray_write_pointer[POINTER_WIDTH-1] != crossed_gray_read_pointer[POINTER_WIDTH-1] &&
           gray_write_pointer[POINTER_WIDTH-2:0] == crossed_gray_read_pointer[POINTER_WIDTH-2:0];
  end

  always_ff @(posedge read_clock) begin
    if (~read_reset_n) begin
      read_pointer <= '0;
      pre_gray_read_pointer <= '0;
      post_gray_write_pointer <= '0;
      crossed_gray_write_pointer <= '0;
    end else begin
      if (read_enable && ~empty) begin
        read_pointer <= read_pointer + 'b1;
      end
      pre_gray_read_pointer <= gray_read_pointer;
      post_gray_write_pointer <= pre_gray_write_pointer;
      crossed_gray_write_pointer <= post_gray_write_pointer;
    end
  end

  always_comb begin
    read_data = buffer[read_pointer[POINTER_WIDTH-1:0]];
    gray_read_pointer = read_pointer ^ (read_pointer >> 1);
    empty = gray_read_pointer == crossed_gray_write_pointer;
  end
endmodule
