/^T.*/ N
s/\(^T.*\)\
\(pin[0-9]*=IN[0-9]*\)/&\
\1\
type=IN/ 
/^T.*/ N
s/\(^T.*\)\
\(pin[0-9]*=OUT\)/&0\
\1\
type=OUT/ 
/VERILOG_PORTS=POSITIONAL/ d
