ARCHITECTURE sinusodial OF voltage_source IS
  QUANTITY v ACROSS i THROUGH lt TO rt;
BEGIN
  v == (amplitude * sin(k * MATH_2_PI * now)) + offset;
END architecture sinusodial;

ARCHITECTURE pulse OF voltage_source IS
  QUANTITY v ACROSS i THROUGH lt TO rt;
  SIGNAL source_sig: real := 0.0;
BEGIN
  p:PROCESS
  BEGIN
    source_sig <= (amplitude/2.0) + offset;
    WAIT FOR width;
    source_sig <= - (amplitude/2.0) + offset;
    WAIT FOR period - width;
  END process p;
  -- BREAK v => 0.0;
  
  v == source_sig;
  
END architecture pulse;
