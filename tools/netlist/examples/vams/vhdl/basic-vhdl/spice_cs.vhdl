LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
-- Entity declaration -- 

ENTITY SPICE_cs IS
	 GENERIC (	N : REAL := 10.0;
			VT : REAL := 25.85e-6;
			ISS : REAL := 10.0e-14 );
	 PORT (		terminal llt : 	electrical;
			terminal ult : 	electrical;
			terminal lrt : 	electrical;
			terminal urt : 	electrical );
END ENTITY SPICE_cs;

