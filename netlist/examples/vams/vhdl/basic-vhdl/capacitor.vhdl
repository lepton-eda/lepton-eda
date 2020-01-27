LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
-- Entity declaration -- 

ENTITY CAPACITOR IS
	 GENERIC (	v_init : REAL := 0.0;
			c : REAL := 10.0e-12 );
	 PORT (		terminal RT : 	electrical;
			terminal LT : 	electrical );
END ENTITY CAPACITOR;

