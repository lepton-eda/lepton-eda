
SUBDIRS = src gschlas scripts lib include examples docs

EXTRA_DIST = ChangeLog.tragesym ChangeLog.gsch2pcb autogen.sh

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
		       config.status aclocal.m4 config.guess config.sub \
		       depcomp install-sh  missing mkinstalldirs compile
