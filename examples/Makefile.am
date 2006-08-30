
SUBDIRS = gTAG lightning_detector RF_Amp TwoStageAmp

exampledir = @GEDADOCDIR@/examples/

example_DATA = README 

EXTRA_DIST = autogen.sh $(example_DATA)

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

