#!/bin/sh

# gEDA: GPL Electronic Design Automation
# pstoimage (converts a postscript file to a gif)
# This script is placed under the GNU Public License (GPL) version 2.0 

# This scripts creates gifs from ps files using gs and ppmtogif
# (both programs are REQUIRED)

# It's a fairly simple script designed to give an idea on how to take
# the output from gschem and create image files

# First parameter is the input filename (must be a postscript file)
# output filename is called `basename $1`.gif

# This script requires free diskspace when it runs due to ppm files being
# so large (don't forget to remove old ppms)

# I have found that ppmtogif does a poor job sometimes, so it in that case
# try using xv to read the image and then save it as a gif


if [ "$1" = "" ] 
then 
	echo usage: pstoimage filename.ps
	exit 0
fi

input_filename=$1
basename=`basename $input_filename .ps`
output_filename=${basename}.gif
#resolution=100


# Added a -r${resolution} if you want to control the resolution
#

# Uncomment the following line if you have Aladdin Ghostscript 
# and want anti-aliasing enabled (replace this line with the other gs line
#gs -r200 -q \
gs -q -dTextAlphaBits=4 -dGraphicsAlphaBits=4 \
	-dNOPAUSE -sOutputFile=$basename.ppm \
	-sDEVICE=ppm \
	$input_filename quit.ps

# ppm to gif
ppmtogif $basename.ppm > $output_filename 

#
# Use ImageMagic convert if you want crop the image
# This will create fairly big gif files
#convert -crop 0x0 $output_filename ${basename}_crop.gif

