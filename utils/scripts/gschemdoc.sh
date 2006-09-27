#!/bin/sh
# $Id$
#
# NOTE: built from gschemdoc.sh
#
# Present as relevant documentation as possible wrt a component
# The strategy is built into separate cases so that may be re-arranged easily
#
# arguments are:
#    1: "documentation" attribute, which can be a filename or an URL
#    2: "device" attribute
#    3: "value" attribute
#    4: symbol basename
#    5: symbol directory (currently not used)
#
# alternative action:
# present gschem user's manual
#
# options:
#    "-m"       display user's manual instead
#
# deBUG:
# echo "gschemdoc args are: <$0> <$1> <$2> <$3> <$4> <$5>"

#
# these may be changed to suit local preferences
#
CANDIDATE_BROWSER="galeon mozilla phoenix netscape netscape-navigator opera firefox konqueror iexplore"
CANDIDATE_PDFREADER="xpdf acroread ggv gv"
CANDIDATE_LOCATE="slocate locate"

#
#  make symbol filename into something more akin to a device name
#
symbolbase=`echo "$4" | sed s/-[0-9]\\.sym//`

#
#  display file vith PDF viewer if file exists, it is a pdf file and we have a reader
#
view_file_if_pdf()
{
	# If we are on Cygwin, translate the path
	# so native applications can find the file
	if test "${cygpath}" != "no" ; then
		file=`cygpath -w $1`
	else
		file=$1
	fi

	if test -f "$1" && (file "$1" | grep -q "PDF") ; then
		if test "${pdfreader}" != "no"; then
			echo "Found ${pdfreader}"
			echo "Using PDF viewer and file: $file"
			# NOTE: Acrobat Reader on Windows does not seem to support
			#       -- on the command line
			${pdfreader} "$file"
			exit
		else
			echo "Did not find a PDF viewer application."
			exit
		fi
	fi
}

#
#  display file vith browser if available
#
view_file_browser()
{
	# If we are on Cygwin, translate the path
	# so native applications can find the file
	if test "${cygpath}" != "no" ; then
		file=`cygpath -w $1`
	else
		file=$1
	fi
	
	if test "${browser}" != "no" ; then
		echo "Found ${browser}"
		echo "Using browser and file: $file"
		# NOTE: Mozilla and Netscape does not seem to support
		#       -- on the command line
		${browser} "file:$file"
		exit
	else
		echo "Did not find a browser application."
		exit
	fi
}

#
#  go look for things globally
#
go_look_for()
{
	if test "${browser}" != "no" ; then
		echo "Go look for: $1"
		${browser} "http://www.google.com/search?q=$1%20filetype:pdf"
		exit
	else
		echo "Did not find a browser application."
		exit
	fi
}

#
#  display a manual of some kind
#
lookup_manual()
{
	if test -f "${DOCDIR}/$1" ; then
		view_file_browser "${DOCDIR}/$1"
	fi
	if test "${locate}" != "no"; then
		b=`${locate} -- "/$1"`
		if test `echo "$b" | wc -w` -ge 1; then
			view_file_browser "`echo "$b" | head -n1`"
		fi
	fi
	echo "Sorry, cannot show manual $1"
	exit 1
}

#
#  establish what software we have
#
browser="no"
for a in ${CANDIDATE_BROWSER}; do
	b=`which $a 2>/dev/null` && browser=$b && break
done

pdfreader="no"
for a in ${CANDIDATE_PDFREADER}; do
	b=`which $a 2>/dev/null` && pdfreader=$b && break
done

locate="no"
for a in ${CANDIDATE_LOCATE}; do
	b=`which $a 2>/dev/null` && locate=$b && break
done

cygpath="no"
b=`which cygpath 2>/dev/null` && cygpath=$b

#
#  documentation case first
#  NOTE: this is too crude..
#
if test "$1" = "-m"; then
	lookup_manual "gedadocs.html"
fi

#
#  i.
#  if there is a file in the documentation directory, we
#  will of course use that first and foremost
#
if test "$1" != "" && test -f "${DOCDIR}/$1" ; then
	view_file_if_pdf "${DOCDIR}/$1"
	view_file_browser "${DOCDIR}/$1"
fi

#
#  ii.
#  if doc is an URL, we'll invoke the brower with that URL
#
if test "${browser}" != "no"; then
	for s in http ftp file; do
		if echo "$1" | grep -q "^$s:"; then
			echo "Found ${browser}"
			echo "Using browser and URL: $1"
			${browser} "$1"
			exit
		fi
	done
fi

#
#  iii.
#  if documentation is a filename, we'll invoke locate to see if we can
#  find it locally
#
if test "$1" != "" && test "${locate}" != "no"; then
	b=`${locate} -- "/$1"`
	if test `echo "$b" | wc -w` -ge 1; then
		n="`echo "$b" | head -n1`"
		view_file_if_pdf "$n"
		view_file_browser "$n"
	fi
fi

#
#  iv.
#  if there is a documentation at all, go look for it on the web
#
if test "$1" != ""; then
	go_look_for "$1"
fi

#
#  v.
#  if there is a device and a value, add .pdf or .PDF and look for local files
#
if test "$2" != "" && test "$3" != ""; then
	for s in .pdf .PDF; do
		view_file_if_pdf "${DOCDIR}/$2-$3$s"
	done
fi

#
#  vi.
#  if there is a device, add .pdf or .PDF and look for local files
#
if test "$2" != ""; then
	for s in .pdf .PDF; do
		view_file_if_pdf "${DOCDIR}/$2$s"
	done
fi

#
#  vii.
#  if there is a device and a value, go look for both on the web
#
if test "$2" != "" && test "$3" != ""; then
	go_look_for "$2%20$3"
fi

#
#  viii.
#  if there is a device only, go look for it on the web
#
if test "$2" != ""; then
	go_look_for "$2"
fi

#
#  ix.
#  if there is a value only, go look for it on the web
#
if test "$3" != "" && test "${browser}" != "no" ; then
	go_look_for "$3"
fi

#
#  x.
#  there is just a symbol filename - try to find a pdf locally
#
if test "${symbolbase}" != "" && test "${pdfreader}" != "no"; then
	for s in .pdf .PDF; do
		view_file_if_pdf "${DOCDIR}/${symbolbase}$s"
	done
fi

#
#  xi.
#  there is a device, go look for it on the web
#
if test "${symbolbase}" != ""; then
	go_look_for "${symbolbase}"
fi

#
#  none of the above
#
echo "Sorry, cannot help you"
