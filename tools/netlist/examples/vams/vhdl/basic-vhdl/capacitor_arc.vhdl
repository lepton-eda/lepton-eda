ARCHITECTURE beh OF capacitor IS 
  QUANTITY v ACROSS i THROUGH lt TO rt;
BEGIN
  IF c > epsi USE 
    v'dot ==  i / c;
  ELSE
    i ==  0.0;
  END use;
END ARCHITECTURE beh;       
