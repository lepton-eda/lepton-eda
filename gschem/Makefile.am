
SUBDIRS = src include lib scheme scripts examples noweb docs 

EXTRA_DIST = VOCABULARY BUGS guile.m4

#proto:
#	(cd src ; ./create_proto)

dist-clean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.status
