LIBRARY ieee,disciplines;
USE ieee.math_real.all;
USE ieee.math_real.all;
USE work.electrical_system.all;
USE work.all;
-- Entity declaration -- 

ENTITY BJT_transistor_simple IS
	 GENERIC (	VT : REAL := 25.85e-3;
			AF : REAL := 1.0;
			KF : REAL := 0.0;
			PT : REAL := 3.0;
			EG : REAL := 1.11;
			MC : REAL := 0.5;
			PC : REAL := 1.0;
			CJC : REAL := 2.5e-12;
			ME : REAL := 0.5;
			PE : REAL := 1.0;
			CJE : REAL := 2.5e-12;
			CCS : REAL := 2.5e-12;
			TR : REAL := 4.0e-9;
			TF : REAL := 4.0e-9;
			NCL : REAL := 2.0;
			C4 : REAL := 0.0;
			NEL : REAL := 2.0;
			C2 : REAL := 0.0;
			RS : REAL := 1.0;
			RE : REAL := 1.0;
			RC : REAL := 1.0;
			RB : REAL := 1.0;
			ISS : REAL := 10.0e-14;
			BR : REAL := 1.0;
			BF : REAL := 100.0 );
	 PORT (		terminal 	Emitter 	:  	electrical;
			terminal 	Collector 	:  	electrical;
			terminal 	Base 	:  	electrical );
END ENTITY BJT_transistor_simple; 

