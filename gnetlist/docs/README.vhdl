
The VHDL backend

Written by Magnus Danielson and improved by Thomas Heidel 


A few things you have to care about:

1. In order to generate valid component declarations, you
   have to add an additional attribute to each pin.
   "type=IN" or "type=OUT" or "type=INOUT"

2. The "device" attribute must be unique to a symbol!
   The verilog symbols of the same type for example, have all
   the same device attribute and will therefore not work.

3. Make sure your component-library picks up the vhdl symbols instead
   of the verilog symbols  Library paths that show up last are searched
   first!


