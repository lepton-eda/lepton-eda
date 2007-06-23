
SUBDIRS = toplevel wiki scripts

EXTRA_DIST = ChangeLog-1.0 autogen.sh

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.h.in \
	               config.status aclocal.m4 install-sh missing \
		       mkinstalldirs
