
SUBDIRS = src include noweb scripts docs

bin_SCRIPTS = libgeda-config

EXTRA_DIST = guile.m4 HACKING BUGS

#proto:
#	(cd src ; ./create_proto)

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure config.status
