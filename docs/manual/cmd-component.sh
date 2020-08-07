#!/usr/bin/env sh
if [ -z "$1" ] ; then
   echo "Usage: $0 symname.sym"
   echo "List symbols: $0 -l"
   echo "Get symbol: $0 symname.sym"
   exit 255
fi

if test "$1" = "-l"; then
   echo "symname1.sym"
   echo "symname2.sym"
else
   if test "$1" = "symname1.sym"; then
       # symname1.sym
       echo "v 20200604 2"
       echo "L 100 100 200 200 3 0 0 0 -1 -1"
   else
       # symname2.sym
       echo "v 20200604 2"
       echo "P 300 300 0 300 1 0 1"
   fi
fi
