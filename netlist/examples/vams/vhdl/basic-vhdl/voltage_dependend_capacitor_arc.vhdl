ARCHITECTURE spice_beh OF voltage_dependend_capacitor IS 
  QUANTITY v ACROSS i THROUGH lt TO rt; 
  QUANTITY c : real;
BEGIN
--  c ==  ((TT * ISS)/(N * VT)) * exp(v/(N*VT)) + CJ0 * (always_positive(1.0 - v/PB))**(-M);

c ==  ((TT * ISS)/(N * VT)) * exp(v/(N*VT)) + CJ0 * (1.0 - v/PB)**(-M);
  v'dot ==  i / always_positive(c);      
END ARCHITECTURE spice_beh;
