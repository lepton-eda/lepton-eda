
AM_CFLAGS = -g -O2 

SUBDIRS = src include lib design docs

EXTRA_DIST = BUGS NOTES README README.gEDA ChangeLog ChangeLog-1.0 \
	AUTHORS COPYING INSTALL autogen.sh ToDos

distclean-local:
	-rm -rf autom4te*.cache

maintainer-clean-local:
	-rm -rf autom4te*.cache

MOSTLYCLEANFILES = *.log core FILE *~ #*#
CLEANFILES = *.log core FILE *~ #*#
DISTCLEANFILES = *.log core FILE *~ prototype.bak #*#
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
                       config.status aclocal.m4 #*#
