LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
-- Entity declaration -- 

ENTITY RESISTOR IS
	 GENERIC (	r : REAL := 10000.0 );
	 PORT (		terminal 	LT 	:  	electrical;
			terminal 	RT 	:  	electrical );
END ENTITY RESISTOR; 

