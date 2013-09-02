#!/bin/sh
dir=`gnetlist -l scheme-path.scm -g geda /dev/null`
test -d ${dir} || echo "Cannot find gEDA Scheme directory."
test -d ${dir} || exit 1
cp -a gnet-spice-noqsi.scm $dir && echo 'Success!'
