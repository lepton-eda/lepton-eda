
SUBDIRS = src gschlas scripts lib include examples 

EXTRA_DIST = README.olib ChangeLog.tragesym README.tragesym \
	     README.gsch2pcb ChangeLog.gsch2pcb

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
		       config.status aclocal.m4
