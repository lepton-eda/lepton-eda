## Process this file with automake to produce Makefile.in

## Created by Anjuta - will be overwritten
## If you don't want it to overwrite it,
## 	Please disable it in the Anjuta project configuration

EXTRA_DIST=setup.cfg\
	setup.sh

INCLUDES = \
	`gtk-config --cflags`

AM_CFLAGS =\
	 -Wall -Wimplicit -Wreturn-type -Wunused -Wswitch -Wcomment -Wuninitialized -Wparentheses -Wpointer-arith -Wmissing-prototypes\
	 -O1\
	 -g

bin_PROGRAMS = setup

setup_SOURCES = \
	support.h\
	support.c\
	main.c\
	interface.h\
	interface.c\
	callbacks.h\
	callbacks.c\
	comps.c\
	config.c\
	dirs.c\
	file.c\
	install.c\
	license.c\
	log.c\
	msgbox.c\
	package.c\
	setup.c\
	start.c\
	status.c\
	summary.c\
	xpm.c\
	comps.h\
	config.h\
	dirs.h\
	file.h\
	install.h\
	license.h\
	log.h\
	global.h\
	msgbox.h\
	package.h\
	setup.h\
	start.h\
	status.h\
	summary.h\
	xpm.h

setup_LDFLAGS = 

setup_LDADD =  \
	$(GTK_LIBS)

MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *~ Makefile.in configure

