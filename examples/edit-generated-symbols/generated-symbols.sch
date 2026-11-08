v 20220529 2
T 60000 57600 9 10 1 0 0 4 1
Edit generated symbols
C 59200 55500 1 0 0 dummy.sym
{
T 59200 56100 5 6 1 0 0 0 1
refdes=dummy
}
C 59900 54900 1 0 0 my-gnd.sym
C 59900 56200 1 0 0 my-vss.sym
C 60100 55300 1 90 0 resistor-2.sym
{
T 59750 55700 5 10 0 0 90 0 1
device=RESISTOR
T 60200 55700 5 10 1 1 0 0 1
refdes=R?
}
N 60000 56200 60000 56300 4
N 60000 55200 60000 55300 4
T 60000 57000 9 10 1 0 0 4 5
Descend into any of the generated symbols (power symbols
or the dummy square symbol) using 'H s', edit it and save into
the "./sym" directory under another name.  Then open the
symbol library, hit onto the "Update" button, and insert the
new symbol.
