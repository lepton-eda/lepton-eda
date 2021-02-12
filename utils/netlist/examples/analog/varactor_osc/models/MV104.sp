* mv104 varactor model(internet)spice3
* Model generated on Jun  5, 01
* MODEL FORMAT: SPICE3
.SUBCKT mv104 11 22
Dmod 11 22 dmodel
.MODEL dmodel d
+(IS=6.62255e-15 RS=0.09585 N=1.02933 EG=1.19084
+XTI=3.00208 BV=32 IBV=0.000999997 TT=2.01517e-08
+KF=0 AF=0.1)
b1 2 0 v=-4.00888*(v(11)-v(22)-0.191433)
rdc1 2 0 1e12
d1 3 0 d1mod
vsense1 4 3 0
e1 4 0 2 0 1
.model d1mod d (is=1, n=38.75969, rs=0.01)
b2 0 5 i=i(vsense1)+1
d2 5 0 d1mod
b3 6 0 v=0.191433-0.249446*v(5)
rdc2 6 0 1e12
vicj 6 7 0
dcj 7 0 dcjmod
.model dcjmod d (is=1e-25 n=10
+ cjo=1.14726e-10 vj=0.4 m=0.47927 fc=0.5)
bicj 11 22 i=i(vicj)
.ends mv104
