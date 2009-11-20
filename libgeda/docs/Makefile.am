SUBDIRS = images

EXTRA_DIST = 		libgeda-main.txt what-is-geda.html \
			what-is-libgeda.html logo.png libgeda.dox

SUFFIXES = 		.html .tex .pdf

all: 
	@echo 'Type: make doxygen  to create doxygen documentation for libgeda'

doxygen: libgeda.dox
	@echo 'Creating doxygen documentation for libgeda...'
if ENABLE_DOXYGEN
	-cd images && $(MAKE) images
	-cd ..
	$(DOXYGEN) libgeda.dox
	-cd latex && $(MAKE)
	-cd ..
else
	@echo 'Doxygen is not installed on your system.'
	@echo 'The documentation can not be built from the sources.'
endif

MOSTLYCLEANFILES = 	*.log *~
CLEANFILES = 		*.log *~
DISTCLEANFILES =        *.log
MAINTAINERCLEANFILES = 	*.log *~ Makefile.in

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
