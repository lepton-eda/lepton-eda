.control
ac dec 20 3meg 40meg
write db(v(1)/1u) frg_band.raw
.endc


notes:
ac analysis results are complex so
vectors must be show as mag(vec) or db(vec) which is equivalent to 
db(mag(vec)). 

-
After writing file open file with gaw, set Abscissa (X) to Log scale and display.


