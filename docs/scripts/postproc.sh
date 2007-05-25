#!/bin/sh

if [ ! -e "geda_faq.html" ]
then
	echo "You must start this program in the toplevel wiki directory"
	echo "wherever geda_faq.html lives."
	exit 1
fi

if [ ! -e "../scripts/removedups.sh" ]
then
	echo "../scripts/removedups.sh must exist"
	exit 1
fi

files=`find . -name \*.html -print`

for i in $files
do
	echo $i
	mv -f $i $i.old
	cat $i.old | sed '/\<script type/d' > $i

	rm -f $i.old

	mv -f $i $i.old2
	isdetail=`echo $i | grep _detail`
	if [ "$isdetail" = "" ] 
	then
		cat $i.old2 | \
	sed 's%media="all" type="text/css" href="http://geda.seul.org/wiki/lib/exe/css.php?s=all"%media="all" type="text/css" href="lib/exe/css"%g' | \
	sed 's%media="screen" type="text/css" href="http://geda.seul.org/wiki/lib/exe/css.php"%media="screen" type="text/css" href="lib/exe/001css"%g' | \
	sed 's%media="print" type="text/css" href="http://geda.seul.org/wiki/lib/exe/css.php?s=print"%media="print" type="text/css" href="lib/exe/002css"%g' \
		> $i
	else
		cat $i.old2 | \
	 sed 's%media="all" type="text/css" href="http://geda.seul.org/wiki/lib/exe/css.php?s=all"%media="all" type="text/css" href="../lib/exe/css"%g' | \
	sed 's%media="screen" type="text/css" href="http://geda.seul.org/wiki/lib/exe/css.php"%media="screen" type="text/css" href="../lib/exe/001css"%g' | \
	sed 's%media="print" type="text/css" href="http://geda.seul.org/wiki/lib/exe/css.php?s=print"%media="print" type="text/css" href="../lib/exe/002css"%g' \
		> $i
	fi


	rm -f $i.old2

done

# Remove duplicate images from _detail
../scripts/removedups.sh 001
../scripts/removedups.sh 002
rm -f _media/001*.png _media/001*.jpg _media/002*.png _media/002*.jpg

