#!/bin/sh

usage()
{
	echo usage: $0 dotted_version date libgeda_sharedlib_version
	echo Exmaple:
	echo "   $0 1.0.1 20070626 29:1:0"
}

new_dottedversion=$1
new_date=$2
new_sharedlibversion=$3 
if [ "$new_dottedversion" = "" ]
then
	echo Missing dotted version
	echo ""
	usage
	exit 1
fi

if [ "$new_date" = "" ]
then
	echo Missing date 
	echo ""
	usage
	exit 1
fi

if [ "$new_sharedlibversion" = "" ]
then
	echo Missing libgeda shared library version
	echo ""
	usage
	exit 1
fi


old_dottedversion=`grep ^DOTTED_VERSION= Makefile | \
	awk -F= '{print $2}'`

old_date=`grep ^DATE_VERSION= Makefile | \
	awk -F= '{print $2}'`

old_sharedlibversion=`grep ^SHARED_LIBRARY_VERSION libgeda/configure.ac.in | \
	awk -F= '{print $2}'`

echo Existing version info: $old_dottedversion $old_date $old_sharedlibversion
echo ""

# Update dates and dotted version in the configure scripts
tbd_files="docs/configure.ac examples/configure.ac gattrib/configure.ac gnetlist/configure.ac gsymcheck/configure.ac libgeda/configure.ac.in symbols/configure.ac utils/configure.ac gschem/configure.ac.in Makefile"

for i in $tbd_files
do
	echo Updating $old_date / $old_dottedversion to $new_date / $new_dottedversion in $i
	mv -f $i $i.orig
	cat $i.orig | \
	    sed "s/^DATE_VERSION=$old_date/DATE_VERSION=$new_date/" | \
	    sed "s/^DOTTED_VERSION=$old_dottedversion/DOTTED_VERSION=$new_dottedversion/" > $i
	rm -f $i.orig
done

# Update shared library version
libgeda_conf=libgeda/configure.ac.in
echo Updating $old_sharedlibversion to $new_sharedlibversion in $libgeda_conf
mv -f $libgeda_conf $libgeda_conf.orig2
cat $libgeda_conf.orig2 | \
	sed "s/^SHARED_LIBRARY_VERSION=$old_sharedlibversion/SHARED_LIBRARY_VERSION=$new_sharedlibversion/" > $libgeda_conf
rm -f $libgeda_conf.orig2

echo ""

# Verification step needed here.
for i in $tbd_files
do
	echo Verify $i
	grep ^DOTTED_VERSION= $i
	grep ^DATE_VERSION= $i
done
