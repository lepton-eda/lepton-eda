
AM_CFLAGS = -g -O2 

SUBDIRS = src include lib design

EXTRA_DIST = BUGS NOTES AlesCorrespondance ProgramArchitecture.gnumeric \
		NotesAndToDos.abw ChangeLog AUTHORS COPYING INSTALL

distclean-local:
	-rm -rf autom4te*.cache

maintainer-clean-local:
	-rm -rf autom4te*.cache

MOSTLYCLEANFILES = *.log core FILE *~ #*#
CLEANFILES = *.log core FILE *~ #*#
DISTCLEANFILES = *.log core FILE *~ prototype.bak #*#
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
                       config.status aclocal.m4 #*#
