#!/bin/sh

export INSTALLDIR=$HOME/geda

# Set these to point to GNU make
export MAKE=make
export GNUMAKE=make

# Nothing below configurable
########################################################################

# Check for bzip2 as some tarballs come as .tar.bz2 files
whichbzip2=`which bzip2`
if [ "$whichbzip2" = "" ]
then
	echo You must have bzip2 installed
	exit 1
fi

# Create a dummy lynx if it doesn't exist
whichlynx=`which lynx`
if [ "$whichlynx" = "" ]
then
	touch $INSTALLDIR/bin/lynx
	chmod 755 $INSTALLDIR/bin/lynx
	CREATEDLYNX=1
fi

# Create a somewhat dumb docbook2pdf if it doesn't exist
whichdocbook2pdf=`which docbook2pdf`
if [ "$whichdocbook2pdf" = "" ]
then

cat << EOF > $INSTALLDIR/bin/docbook2pdf
#!/bin/sh

newfilename=\`echo \$1 | sed "s/sgml/pdf/"\`
touch \$newfilename
EOF

	chmod 755 $INSTALLDIR/bin/docbook2pdf
	CREATEDDOCBOOK2PDF=1
fi


export PKG_CONFIG_PATH=$INSTALLDIR/lib/pkgconfig:$PKG_CONFIG_PATH
export PATH=$INSTALLDIR/bin:$PATH
export LD_LIBRARY_PATH=$INSTALLDIR/lib:$LD_LIBRARY_PATH
export CFLAGS=-I$INSTALLDIR/include
export LDFLAGS=-L$INSTALLDIR/lib

EXTRA_CONFIGURE_FLAGS=

# $1 name of package and created subdirectory
configure_build_install()
{
	filename=$1.tar.gz
	tarflags=xvfz
	if [ ! -e $filename ] 
	then
		filename=$1.tar.bz2
		if [ ! -e $filename ] 
		then
			echo Could not find $1.tar.gz or $1.tar.bz2
			exit 2
		else
			bzip2 -d $filename
			filename=$1.tar
			tarflags=xvf
		fi
	fi

	echo Untarring: $filename with tar $tarflags
	tar $tarflags $filename
	status=$?
	if [ $status != 0 ]
	then
		echo $1: failed to untar
		exit 3
	fi
	
	cd $1
	status=$?
	if [ $status != 0 ]
	then
		echo $1: failed to cd into $1
		exit 4
	fi

	echo Configuring: $1 $EXTRA_CONFIGURE_FLAGS
	./configure --prefix=$INSTALLDIR $EXTRA_CONFIGURE_FLAGS
	status=$?
	if [ $status != 0 ]
	then
		echo $1: failed to configure
		exit 5
	fi

	echo Building: $1
	$MAKE
	status=$?
	if [ $status != 0 ]
	then
		echo $1: failed to $MAKE
		exit 6
	fi

	echo Installing: $1
	$MAKE install
	status=$?
	if [ $status != 0 ]
	then
		echo $1: failed to $MAKE install
		exit 7
	fi

	cd ..
}

########################################################################
#
# Build the actual dependencies now 
#
########################################################################

configure_build_install pkgconfig-0.15.0 

export EXTRA_CONFIGURE_FLAGS=--disable-csharp 
configure_build_install gettext-0.14.3 
export EXTRA_CONFIGURE_FLAGS=

configure_build_install freetype-2.1.9 
configure_build_install render-0.8 
configure_build_install libXrender-0.8.4 
configure_build_install expat-1.95.8 
configure_build_install fontconfig-2.3.1 
configure_build_install libXft-2.1.6 
configure_build_install xextensions-1.0.1 
configure_build_install fixesext-2.0.1 
configure_build_install libXfixes-2.0.1 
configure_build_install libXcursor-1.1.2 
configure_build_install zlib-1.2.2 
configure_build_install libpng-1.2.8-config 
configure_build_install libiconv-1.9.2
configure_build_install glib-2.6.3 
configure_build_install atk-1.9.0 
configure_build_install pango-1.8.1 

export EXTRA_CONFIGURE_FLAGS=-"-without-libtiff --without-libjpeg"
configure_build_install gtk+-2.6.4 
export EXTRA_CONFIGURE_FLAGS=

configure_build_install guile-1.6.7 
configure_build_install libstroke-0.5.1
configure_build_install libgdgeda-2.0.15 

########################################################################
#
# Now build gEDA/gaf
#
########################################################################
$MAKE open
$MAKE install

########################################################################
#
# Cleanup 
#
########################################################################
if [ "$CREATEDLYNX" != "" ]
then
	rm -f $INSTALLDIR/bin/lynx
fi

if [ "$CREATEDDOCBOOK2PDF" != "" ]
then
	rm -f $INSTALLDIR/bin/docbook2pdf
fi
