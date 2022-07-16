LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
-- Entity declaration -- 

ENTITY CURRENT_SOURCE IS
	 GENERIC (	N : REAL := 1.0;
			VT : REAL := 25.85e-6;
			ISS : REAL := 10.0e-14 );
	 PORT (		terminal RT : 	electrical;
			terminal LT : 	electrical );
END ENTITY CURRENT_SOURCE;

