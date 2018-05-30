/*module to test T type Flip Flop*/

module T_FF_TESTBENCH(q0, q1, q2, q3, clk, reset);

input  q0, q1, q2, q3;
output clk;
output reset;
reg clk, reset;

initial
   clk = 1'b0;

always
   #5 clk = ~clk;

initial
   begin
      $dumpfile("test.vcd");
      $dumpvars(0,ripple);
      reset = 1'b1;
      #10 reset = 1'b0;
      #180 reset = 1'b1;
      #3 reset = 1'b0;
      #30 $finish;
   end

endmodule
