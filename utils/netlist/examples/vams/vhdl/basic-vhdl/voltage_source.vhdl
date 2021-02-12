LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
-- Entity declaration -- 

ENTITY VOLTAGE_SOURCE IS
	 GENERIC (	amplitude : REAL := 2.0;
			offset : REAL := 1.2;
			width : REAL := 0.002;
			period : REAL := 0.005;
			k : REAL := 100.0 );
	 PORT (		terminal 	RT 	:  	electrical;
			terminal 	LT 	:  	electrical );
END ENTITY VOLTAGE_SOURCE; 

