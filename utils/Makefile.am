
SUBDIRS = src gschlas scripts lib include examples docs man tests

EXTRA_DIST = ChangeLog.tragesym-1.0 ChangeLog.gsch2pcb-1.0 ChangeLog-1.0 \
	autogen.sh

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.h.in \
		       config.status aclocal.m4 config.guess config.sub \
		       depcomp install-sh  missing mkinstalldirs compile
