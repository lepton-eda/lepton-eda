#!/bin/sh

if [ ! -d "wiki" -a ! -d "wikiold" ]
then
        echo "You must start this program in the toplevel docs directory"
        echo "wherever wiki and wikiold live."
        exit 1
fi

files=`find wikiold -name .gitignore -print`

for i in $files
do
	src=$i
	dest=`echo $i | sed "s/^wikiold/wiki/"`	
	echo Copying $src to $dest
	cp -f $src $dest
done

