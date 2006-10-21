#!/bin/sh

# $1 is the name of the directory
do_work()
{
	if [ "$2" = "." ]
	then
		thisdir=
	else
		if [ "$2" != "" ]
		then
			thisdir=/$1/$2
		else
			thisdir=/$1
		fi
	fi

	thisdir=`echo $thisdir | sed "s%//%/%g"`

	mv -f Makefile.am Makefile.am.old
	echo "" > Makefile.am

	echo "" >> Makefile.am
	echo "docname=wiki" >> Makefile.am
	echo 'docsdir = @GEDADOCDIR@/$(docname)'$thisdir >> Makefile.am
	echo "" >> Makefile.am

	dirs=`find . -maxdepth 1 -type d -print | grep -v CVS | sed "s%^./%%" | sed "s%^\.%%"`

	if [ "$dirs" != "" ]
	then
		echo -n "SUBDIRS = " >> Makefile.am
		for i in $dirs
		do
			echo -n "$i " >> Makefile.am
		done
		echo "" >> Makefile.am
		echo "" >> Makefile.am
	fi

	files=`find . -maxdepth 1 -type f -print | grep -v Makefile | grep -v CVS | grep -v .cvsignore  | sed "s%^./%%" | sed "s%^\.%%"`
	files=`echo $files`
	echo files: _${files}_

	if [ "$files" != "" ]
	then
		echo -n "docs_DATA = " >> Makefile.am
		for i in $files
		do
			if [ ! -d $i ]
			then
				echo -n "$i " >> Makefile.am
			fi
		done
		echo "" >> Makefile.am
		echo "" >> Makefile.am

		echo 'EXTRA_DIST = $(docs_DATA)' >> Makefile.am
	
	fi
	echo "" >> Makefile.am
}

pwd=`pwd`
thisprogram=/home/ahvezda/2gaf/docs/scripts/populatemkfiles.sh
basedir=/home/ahvezda/2gaf/docs/wiki

subdir=`echo $pwd | awk -F$basedir '{print $2}'`

do_work $1 $2

subdirs=`find . -maxdepth 1 -type d -print | grep -v CVS | sed "s%^./%%" | sed "s%^\.%%"`
subdirs=`echo $subdirs`

for i in $subdirs
do
	currentpwd=`pwd`
	cd $i
	echo Handling: $subdir $i
	$thisprogram $subdir $i
	cd $currentpwd
done

