LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
-- Entity declaration -- 

ENTITY VOLTAGE_DEPENDEND_CAPACITOR IS
	 GENERIC (	PB : REAL := 1.0;
			M : REAL := 0.5;
			VT : REAL := 25.85e-6;
			ISS : REAL := 1.0e-15;
			TT : REAL := 4.0e-9;
			CJ0 : REAL := 2.5e-12;
			v_init : REAL := 0.0;
                        N :REAL := 1.0);
	 PORT (		terminal RT : 	electrical;
			terminal LT : 	electrical );
END ENTITY VOLTAGE_DEPENDEND_CAPACITOR;

