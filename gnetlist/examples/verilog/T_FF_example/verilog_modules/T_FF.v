/* module for T-Flip Flop */


module T_FF(q_, clock_, reset_);


output q_;
input clock_, reset_;
wire d;

D_FF dff0(q_, d, clock_, reset_);
	not n1(d, q_); //not primitive

endmodule
