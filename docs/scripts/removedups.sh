#!/bin/sh

prefix=$1

cd _media
files=`ls -1 ${prefix}*`

cd ../_detail

for i in $files
do
	oldname=$i
	newname=`echo $i | sed "s/$prefix//g"`
	htmlfiles=`grep -l $oldname *.html`
	for j in $htmlfiles
	do
		echo Patching $j : $oldname to $newname
		mv $j $j.orig
		cat $j.orig | sed "s/$oldname/$newname/g" > $j
	done
done

