ARCHITECTURE voltage_dependend OF current_source IS
  QUANTITY v ACROSS i THROUGH lt TO rt;
BEGIN
    i == ISS * (exp(v/(N * VT)) - 1.0);
END ARCHITECTURE voltage_dependend;
