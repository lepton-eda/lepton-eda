#!/bin/sh
# ha ha

if [ -z "$1" ] ; then
    echo "usage $0 geda-bom  # output on stdout"
    exit -1
fi    
cat $1 | sort -k1,1 |\
/usr/bin/awk '!/device/{printf("%-5s %-20s %-20s %-20s\n", toupper($1), toupper($2), toupper($3), toupper($4));} /device/{}'

