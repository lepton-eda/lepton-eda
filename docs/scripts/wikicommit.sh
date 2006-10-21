#!/bin/sh

files=`find . -name \* -print | grep -v CVS | grep -v Makefile | sed "s%^\./%%" | sed "s%^\.%%"`

for i in $files
do
	image=`file $i | grep -i image`

	if [ "$image" != "" ]
	then
		echo Adding image: $i
		cvs add -kb $i
	else
		echo Adding regular file: $i
		cvs add $i
	fi
done
