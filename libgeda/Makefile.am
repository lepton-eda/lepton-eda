

SUBDIRS = src include docs noweb scripts

bin_SCRIPTS = libgeda-config

EXTRA_DIST = guile.m4 HACKING BUGS

proto:
	(cd src ; ./create_proto)

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure
