
SUBDIRS = attributes symbols fileformats hooks keymapping netattrib \
	  gschem gnetlist switcap spice-sdb toplevel templates \
	  pads_backannotate

EXTRA_DIST = autogen.sh

distclean-local:
	-rm -rf autom4te.cache

maintainer-clean-local:
	-rm -rf autom4te.cache

MOSTLYCLEANFILES = *.log core FILE *~
CLEANFILES = *.log core FILE *~
DISTCLEANFILES = *.log core FILE *~ prototype.bak 
MAINTAINERCLEANFILES = *.log core FILE *~ Makefile.in configure \
	               config.status aclocal.m4 install-sh missing \
		       mkinstalldirs
