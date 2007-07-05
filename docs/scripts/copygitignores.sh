#!/bin/sh

if [ ! -d "wiki" -a ! -d "wip" ]
then
        echo "You must start this program in the toplevel docs directory"
        echo "wherever wiki and wip live."
        exit 1
fi

files=`find wiki -name .gitignore -print`

for i in $files
do
	src=$i
	dest=`echo $i | sed "s/^wiki/wip/"`	
	echo Copying $src to $dest
	cp -f $src $dest
done

