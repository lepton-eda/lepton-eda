v 20150930 2
N 6700 4500 5900 4500 4
{
T 5900 4700 5 10 1 1 0 0 1
netname=middleA
}
N 8300 4500 9600 4500 4
{
T 8900 4700 5 10 1 1 0 0 1
netname=middleB
}
C 5300 4400 1 0 0 in-1.sym
{
T 5300 4700 5 10 1 1 0 0 1
refdes=A
}
C 9600 4400 1 0 0 out-1.sym
{
T 9900 4700 5 10 1 1 0 0 1
refdes=B
}
C 6700 3900 1 0 0 h3-bottom.sym
{
T 7000 5000 5 10 1 1 0 0 1
refdes=Umiddle
T 7500 5400 5 10 1 1 0 0 1
source=h3-bottom-1.sch,h3-bottom-2.sch
T 7500 5200 5 10 1 1 0 0 1
source=h3-bottom-3.sch
}
