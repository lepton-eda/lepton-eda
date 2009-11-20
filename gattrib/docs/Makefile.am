SUFFIXES = 		.html

EXTRA_DIST = gattrib.dox what-is-gattrib.html what-is-geda.html \
	     gattrib-main.txt logo.png

all: 
	@echo 'Type: make doxygen  to create doxygen documentation for gattrib'

doxygen: gattrib.dox
	@echo 'Creating doxygen documentation for gattrib...'
if ENABLE_DOXYGEN
	$(DOXYGEN) gattrib.dox
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

MOSTLYCLEANFILES = 	*.log *~
CLEANFILES = 		*.log *~
DISTCLEANFILES =        *.log
MAINTAINERCLEANFILES = 	*.log *~ Makefile.in

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
