
SUBDIRS = src include lib scheme examples docs tests

EXTRA_DIST = BUGS ChangeLog-1.0 autogen.sh

proto:
	(cd src ; ./create_proto)

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.h.in \
		       config.status aclocal.m4 config.guess config.sub \
		       depcomp install-sh missing mkinstalldirs
