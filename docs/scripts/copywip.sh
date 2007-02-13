#!/bin/sh

# Handle changed files
diff -r -q wip wiki | grep Files | sed "s/Files/cp -f/" | sed "s/ and / /" | sed "s/ differ//" > tmp$$.sh
sh ./tmp$$.sh
rm -f tmp$$.sh


# Handle new files (still buggy but works for at least two levels)
diff -r -q wip wiki | grep "Only in wip" | sed "s/Only in//" | sed "s/: /:/" > filelist.$$

for i in `cat filelist.$$`
do
	directory=`echo $i | awk -F: '{print $1}'`
	file=`echo $i | awk -F: '{print $2}'`
	basedir=`basename $directory`

	echo Processing: $directory $file
	if [ "$basedir" = "wip" ] 
	then
		cp -f $basedir/$file wiki/
	else
		mkdir -p wiki/$basedir > /dev/null 2>&1
		cp -f wip/$basedir/$file wiki/$basedir
	fi
done

rm -f filelist.$$
