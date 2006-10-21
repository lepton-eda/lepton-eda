#!/bin/sh

diff -r -q wip wiki | grep Files | sed "s/Files/cp -f/" | sed "s/ and / /" | sed "s/ differ//" > tmp$$.sh

sh ./tmp$$.sh

rm -f tmp$$.sh

