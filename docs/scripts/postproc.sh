#!/bin/sh

files=`find . -name \*.html -print`

for i in $files
do
	echo $i
	mv -f $i $i.old
	cat $i.old | sed '/\<script type/d' > $i

	mv -f $i $i.old2
	cat $i.old2 | sed 's%media="screen" type="text/css" href="http://geda.seul.org/wiki/lib/exe/css.php"%media="screen" type="text/css" href="lib/exe/css"%g' | sed 's%media="print" type="text/css" href="http://geda.seul.org/wiki/lib/exe/css.php?print=1"%media="print" type="text/css" href="lib/exe/001css"%g' > $i
done


