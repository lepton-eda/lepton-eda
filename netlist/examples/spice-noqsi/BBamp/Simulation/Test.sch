v 20130925 2
C 40000 40000 0 0 0 EMBEDDEDNoqsi-title-B.sym
[
B 40000 40000 17000 11000 15 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
T 54400 41500 5 10 0 0 0 0 1
graphical=1
L 52900 40600 52900 40000 15 0 0 0 -1 -1
B 49400 40000 7600 1400 15 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
L 49400 40700 57000 40700 15 0 0 0 -1 -1
T 50000 40500 9 10 0 1 0 0 1
date=$Date: 2010-02-10 22:01:20 $
T 53900 40500 9 10 0 1 0 0 1
rev=$Revision: 1.2 $
T 55400 40200 9 10 0 1 0 0 1
auth=$Author: jpd $
T 50200 40800 9 8 0 1 0 0 1
fname=$Source: /cvs/Osaka/SXI/Components/Symbols/Noqsi-title-B.sym,v $
T 53200 41200 9 14 0 1 0 4 1
title=TITLE
T 49500 40800 15 8 1 0 0 0 1
FILE:
T 53000 40500 15 8 1 0 0 0 1
REVISION:
T 53000 40200 15 8 1 0 0 0 1
DRAWN BY: 
T 49500 40200 15 8 1 0 0 0 1
PAGE
T 51200 40200 15 8 1 0 0 0 1
OF
T 49500 41200 15 8 1 0 0 0 1
TITLE
T 49500 40500 15 8 1 0 0 0 1
DATE
B 49400 49600 7600 1400 15 0 0 0 -1 -1 0 -1 -1 -1 -1 -1
T 50200 50400 9 30 1 0 0 0 1
Noqsi Aerospace, Ltd.
T 50600 50100 9 10 1 0 0 0 1
2822 South Nova Road, Pine, Colorado, USA 80470
T 51300 49800 9 10 1 0 0 0 1
+1-303-816-2756    jpd@noqsi.com
]
{
T 50000 40500 15 10 1 1 0 0 1
date=20130901
T 53900 40500 15 10 1 1 0 0 1
rev=1.0
T 55400 40200 15 10 1 1 0 0 1
auth=jpd@noqsi.com
T 50200 40800 15 8 1 1 0 0 1
fname=Test.sch
T 53200 41200 15 14 1 1 0 4 1
title=Test Circuit
}
C 46500 45400 1 0 0 BBamp.sym
{
T 46600 46700 5 10 1 1 0 0 1
refdes=Amp1
}
C 47200 45100 1 0 0 gnd-1.sym
C 44700 44800 1 0 0 vac-1.sym
{
T 45400 45450 5 10 1 1 0 0 1
refdes=Vin
T 45500 44650 5 10 1 1 0 0 3
value=dc 0
+ac 1
+pulse 0 0.1 10n 200p 200p 10n
}
C 44700 47200 1 0 0 vdc-1.sym
{
T 45400 47850 5 10 1 1 0 0 1
refdes=Vdc
T 45400 47650 5 10 1 1 0 0 1
value=DC 8V
}
N 47300 46600 47300 48400 4
N 47300 48400 45000 48400 4
N 45300 46000 45000 46000 4
C 44900 46900 1 0 0 gnd-1.sym
C 44900 44500 1 0 0 gnd-1.sym
N 49200 46000 49600 46000 4
{
T 49500 46100 5 10 1 1 0 0 1
netname=out
}
T 50300 40200 9 10 1 0 0 0 1
1
T 51900 40200 9 10 1 0 0 0 1
1
T 43100 42700 9 10 1 1 0 0 11
spice-epilog=.control
ac dec 100 10k 1g
plot db(out)-db(in)
tran 25p 25n
plot in out xamp1.e
noise v(out) vin dec 10 10k 1G
setplot noise1
let noise_figure=db(inoise_spectrum)/2-db(2*sqrt(boltz*290*50))
plot noise_figure
.endc

C 49400 45100 1 90 0 resistor-1.sym
{
T 49700 45800 5 10 1 1 180 0 1
refdes=R0
T 49500 45300 5 10 1 1 0 0 1
value=50
}
C 49200 44800 1 0 0 gnd-1.sym
C 48300 45800 1 0 0 capacitor-1.sym
{
T 48500 46300 5 10 1 1 0 0 1
refdes=C0
T 48500 46700 5 10 0 0 0 0 1
symversion=0.1
T 48600 45600 5 10 1 1 0 0 1
value=1uF
}
N 48300 46000 48100 46000 4
C 45300 45900 1 0 0 resistor-1.sym
{
T 45500 46200 5 10 1 1 0 0 1
refdes=Rg
T 45600 45700 5 10 1 1 0 0 1
value=50
}
N 46200 46000 46500 46000 4
{
T 46300 46200 5 10 1 1 0 0 1
netname=in
}
