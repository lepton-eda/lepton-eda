//dummy module for test bench


module dummy;

wire q0,q1,q2,q3,clk,reset;

   RIPPLE_COUNT ripple(.q0(q0),.q1(q1),.q2(q2),.q3(q3),
   .clock(clk),.reset(reset));
   T_FF_TESTBENCH TB1(.q0(q0),.q1(q1),.q2(q2),.q3(q3),
   .clk(clk),.reset(reset));

endmodule
