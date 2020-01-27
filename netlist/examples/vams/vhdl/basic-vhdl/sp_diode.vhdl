LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
-- Entity declaration -- 

ENTITY SP_DIODE IS
	 GENERIC (	RS : REAL := 1.0;
			VT : REAL := 25.85e-3;
			AF : REAL := 1.0;
			KF : REAL := 0.0;
			PT : REAL := 3.0;
			EG : REAL := 1.11;
			M : REAL := 0.5;
			PB : REAL := 1.0;
			TT : REAL := 0.0;
			CJ0 : REAL := 0.0;
			N : REAL := 1.0;
			ISS : REAL := 10.0e-14 );
	 PORT (		terminal KATHODE : 	electrical;
			terminal ANODE : 	electrical );
END ENTITY SP_DIODE;

