#!/bin/sh

# modify these versions if necessary
# (Gtk+ 1.2.4 used as default to be compatible with glibc 2.1)

GTK_VERSION=1.2.4

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

echo "gEDA Suite setup"
echo "(c) 2001-2003 Piotr Miarecki"
echo ""

if test -z $1; then
	echo "Usage:"
	echo "        setup.sh <VERSION> [<DIR_PREFIX> [<CD_VERSION>]]"
	echo ""
	echo "Examples:"
	echo "        setup.sh 20030901"
	echo "        setup.sh 20030901 geda-"
	echo "        setup.sh 20030901 geda- -20030901"
	exit 1
fi

GAF_VERSION=$1

if test -n $2; then
	DIR_PREFIX=$2
	if test -n $3; then
		CD_VERSION=$3
	else
		CD_VERSION=
	fi
else
	DIR_PREFIX=
	CD_VERSION=
fi

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

echo "Checking for geda-setup ..."
if test -d ${DIR_PREFIX}setup${CD_VERSION}; then
	echo "Using CVS version of geda-setup and setup.cfg ..."
	if ! test -x ${DIR_PREFIX}setup${CD_VERSION}/src/setup || ! test -f ${DIR_PREFIX}setup${CD_VERSION}/src/setup.cfg; then
		echo "WARNING ! Setup not found, building ..."
		make setup
	fi
	cp ${DIR_PREFIX}setup${CD_VERSION}/src/setup geda-setup 
	sed -e s/\${CD_VERSION}/-${GAF_VERSION}/g -e s/\${GAF_VERSION}/${GAF_VERSION}/g -e s/\${DIR_PREFIX}/${DIR_PREFIX}/g ${DIR_PREFIX}setup${CD_VERSION}/src/setup.cfg > setup.cfg
else
	if test -f ${DIR_PREFIX}setup${CD_VERSION}.tar.gz ; then
		echo "Using distributed version of geda-setup and setup.cfg (from existing ${DIR_PREFIX}setup${CD_VERSION}.tar.gz) ..."
	else
		echo "File ${DIR_PREFIX}setup${CD_VERSION}.tar.gz not found in current directory, trying to download ..."
		wget -c -t0 ftp://ftp.seul.org/geda/devel/${GAF_VERSION}/${DIR_PREFIX}setup${CD_VERSION}.tar.gz >/dev/null 2>/dev/null
		if test -f ${DIR_PREFIX}setup${CD_VERSION}.tar.gz ; then
			echo "Using distributed version of geda-setup and setup.cfg (from downloaded ${DIR_PREFIX}setup${CD_VERSION}.tar.gz) ..."
		fi
	fi
	if test -f ${DIR_PREFIX}setup${CD_VERSION}.tar.gz ; then
		echo "Forced setup building ..."
		tar -xzf ${DIR_PREFIX}setup${CD_VERSION}.tar.gz
		cd ${DIR_PREFIX}setup${CD_VERSION}
		echo "   configuring ..."
		./configure > /dev/null
		echo "   compiling ..."
		make > /dev/null
		if ! test ${DIR_PREFIX}setup${CD_VERSION}/src/setup; then
			echo "ERROR ! Cannot build setup, installation cannot be continued !" >&2
			exit 0
		fi
		cd ..
		cp ${DIR_PREFIX}setup${CD_VERSION}/src/setup geda-setup
		sed -e s/\${CD_VERSION}/-${GAF_VERSION}/g -e s/\${GAF_VERSION}/${GAF_VERSION}/g -e s/\${DIR_PREFIX}/${DIR_PREFIX}/g ${DIR_PREFIX}setup${CD_VERSION}/src/setup.cfg > setup.cfg
		rm -Rf ${DIR_PREFIX}setup${CD_VERSION}
	elif test -x geda-setup && test -f setup.cfg ; then
		echo "Using geda-setup and setup.cfg existing in current directory ..."
	else
		echo "ERROR ! Cannot find geda-setup and setup.cfg, installation cannnot be continued !" >&2
		exit
	fi
fi

echo "Starting ${DIR_PREFIX}setup ..."
./geda-setup
