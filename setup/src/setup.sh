#!/bin/sh

# modify these versions if necessary
# (Gtk+ 1.2.4 used as default to be compatible with glibc 2.1)

GTK_VERSION=1.2.4

# this should be derrived from Makefile
VERSION=20030514
CD_VERSION=-${VERSION}
DIR_PREFIX=geda-

##############################################################################

GLIB_LIB=libglib-1.2.so.0
GLIB_FILE=glib-${GTK_VERSION}.tar.gz
GLIB_DIR=glib-${GTK_VERSION}

GTK_LIB=libgtk-1.2.so.0
GTK_FILE=gtk+-${GTK_VERSION}.tar.gz
GTK_DIR=gtk+-${GTK_VERSION}

PREFIX=${PWD}
PATH=${PREFIX}:${PREFIX}/bin:${PATH}
LD_LIBRARY_PATH=${PREFIX}:${PREFIX}/lib:${LD_LIBRARY_PATH}

echo
echo gEDA Suite setup ${VERSION}
echo "(c) 2001-2003 Piotr Miarecki"
echo

echo Checking for tar ...
if ! (which tar >/dev/null 2>/dev/null) ; then
	echo ERROR ! Cannot find tar ! >&2
	exit 1
fi

echo Checking for gcc ...
if ! (which gcc >/dev/null 2>/dev/null) ; then
	echo ERROR ! Cannot find gcc ! >&2
	exit 1
fi

echo Checking for wget ...
if ! (which wget >/dev/null 2>/dev/null) ; then
	echo WARNING ! Cannot find wget ! >&2
	exit 1
fi

echo Checking for libglib ...
if ! test -x /lib/${GLIB_LIB} && ! test -x /usr/lib/${GLIB_LIB} && ! test -x ./${GLIB_LIB}; then
	echo WARNING ! Library ${GLIB_LIB} not found, building ...
	if ! test -f ${GLIB_FILE} ; then
		echo WARNING ! Cannot find ${GLIB_FILE}, downloading using wget ... >&2
		wget -c -t0 ftp://ftp.gtk.org/pub/gtk/v1.2/${GLIB_FILE} >/dev/null 2>/dev/null
		if ! test -f ${GLIB_FILE} ; then
			echo ERROR ! Cannot download ${GLIB_FILE}, please copy it to ${PREFIX}, then restart setup >&2
			exit
		fi
	fi
	tar -xzf ${GLIB_FILE}
	cd ${GLIB_DIR}
	./configure --prefix=${PREFIX}
	make
	make install
	cd ..
	if ! test -x ${PREFIX}/lib/${GLIB_LIB} ; then
		echo Cannot build ${GLIB_LIB}, sorry >&2
		exit
	fi
fi

echo Checking for libgtk ...
if ! test -x /lib/${GTK_LIB} && ! test -x /usr/lib/${GTK_LIB} && ! test -x ./${GTK_LIB}; then
	echo WARNING ! Library ${GTK_LIB} not found, building ... >&2
	if ! test -f ${GTK_FILE} ; then
		echo WARNING ! Cannot find ${GTK_FILE}, downloading using wget ...
		wget -c -t0 ftp://ftp.gtk.org/pub/gtk/v1.2/${GTK_FILE} >/dev/null 2>/dev/null
		if ! test -f ${GTK_FILE} ; then
			echo ERROR ! Cannot download ${GTK_FILE}, please copy it to ${PREFIX}, then restart setup >&2
			exit
		fi
	fi
	tar -xzf ${GTK_FILE}
	cd ${GTK_DIR}
	./configure --prefix=${PREFIX}
	make
	make install
	cd ..
	if ! test -x ${PREFIX}/lib/${GTK_LIB} ; then
		echo ERROR ! Cannot build ${GTK_LIB}, sorry >&2
		exit
	fi
fi

echo Checking for ${DIR_PREFIX}setup ...
if ! test -x ./${DIR_PREFIX}setup ; then
	echo WARNING ! Setup not found, building ... >&2
	if ! test -f ${DIR_PREFIX}setup${CD_VERSION}.tar.gz ; then
		echo WARNING ! Cannot find ${DIR_PREFIX}setup${CD_VERSION}.tar.gz, downloading using wget ... >&2
		wget -c -t0 ftp://ftp.seul.org/geda/devel/${VERSION}/${DIR_PREFIX}setup${CD_VERSION}.tar.gz >/dev/null 2>/dev/null
		if ! test -f ${DIR_PREFIX}setup${CD_VERSION}.tar.gz ; then
			echo ERROR ! Cannot download ${DIR_PREFIX}setup${CD_VERSION}.tar.gz, please copy it to ${PREFIX}, then restart setup >&2
			exit
		fi
	fi
	tar -xzf ${DIR_PREFIX}setup${CD_VERSION}.tar.gz
	cd ${DIR_PREFIX}setup${CD_VERSION}
	./configure
	make
	mv src/setup ../${DIR_PREFIX}setup
	cd ..
fi

echo Starting ${DIR_PREFIX}setup ...
./${DIR_PREFIX}setup
