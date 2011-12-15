#! /bin/sh

usage ()
{
  cat <<EOF
Usage: schdiff old new

View a graphical diff of gEDA schematics using gschem and ImageMagick.

Usage with git:
  git difftool -x schdiff ...

Usage with Mercurial:
  Add the following to .hgrc:

    [extensions]
    hgext.extdiff =
    [extdiff]
    cmd.schdiff = /path/to/schdiff

  Then use: hg schdiff ...

Usage with Subversion:
  svn diff --diff-cmd schdiff

Report bugs to <http://bugs.launchpad.net/geda>
gEDA/gaf homepage <http://gpleda.org/>
EOF

}

for PROG in gschem composite display
do
  if which $PROG > /dev/null
  then 
    true
  else 
    echo "$PROG is not found.  Either it is not installed, or not in your PATH"
    exit 1
  fi
done

if test $# -lt 2
  then usage; exit 1
fi

#In case the script was invoked with extra option arguments, throw them away
shift `expr $# - 2`

if test -d $1 -o -d $2
  then echo "ERROR: schdiff cannot diff entire directories"
  exit 1
fi

LEFTFILE=$1
RIGHTFILE=$2

GEDASCHEMEDIR=share/gEDA/scheme
SCHEMEFILE=`dirname $0`/../${GEDASCHEMEDIR}/schdiff-image.scm
LEFTPNG=`mktemp --tmpdir schdiff.XXXXXXXXXX`
RIGHTPNG=`mktemp --tmpdir schdiff.XXXXXXXXXX`
DIFFPNG=`mktemp --tmpdir schdiff.XXXXXXXXXX`

gschem -p -o $LEFTPNG -q -c '(image-size 1344 1008) (image-color "disabled") (gschem-use-rc-values) (gschem-image "dummyfilename") (gschem-exit)' $LEFTFILE && \
gschem -p -o $RIGHTPNG -q -c '(image-size 1344 1008) (image-color "disabled") (gschem-use-rc-values) (gschem-image "dummyfilename") (gschem-exit)' $RIGHTFILE && \
composite -stereo 0 $LEFTPNG $RIGHTPNG $DIFFPNG && \
display $DIFFPNG
rm $LEFTPNG
rm $RIGHTPNG
rm $DIFFPNG
