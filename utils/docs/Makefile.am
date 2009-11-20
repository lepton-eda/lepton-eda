
EXTRA_DIST = README.gsch2pcb README.olib README.tragesym README.grenum \
	     TODO.grenum why.grenum logo.png utils-main.txt \
	     what-is-geda.html what-is-utils.html utils.dox

docsreadmedir = $(docdir)/readmes
docsreadme_DATA = README.gsch2pcb README.olib README.tragesym README.grenum

doxygen: utils.dox
	@echo 'Creating doxygen documentation for utils...'
if ENABLE_DOXYGEN
	$(DOXYGEN) utils.dox
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif


MOSTLYCLEANFILES = *.log *~
CLEANFILES = *.log *~ 
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *~ Makefile.in

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
