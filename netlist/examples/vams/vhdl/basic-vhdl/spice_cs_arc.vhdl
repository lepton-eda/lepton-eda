ARCHITECTURE current_controlled OF spice_cs IS
  QUANTITY v ACROSS i THROUGH urt TO lrt;
  QUANTITY vc ACROSS ic THROUGH ult TO llt;
BEGIN
  vc == 0.0;
  i == N * ic;
--  i == ISS * (exp(v/(N * VT)) - 1.0);
END ARCHITECTURE current_controlled;
