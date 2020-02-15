SUBDIRS = images

EXTRA_DIST =		libgeda-main.txt what-is-geda.html \
			what-is-libgeda.html logo.png liblepton.dox

SUFFIXES =		.html .tex .pdf

all:
	@echo 'Type: make doxygen  to create doxygen documentation for libgeda'

doxygen: liblepton.dox
	@echo 'Creating doxygen documentation for liblepton...'
if ENABLE_DOXYGEN
	-(cd images && $(MAKE) images)
	$(DOXYGEN) liblepton.dox
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif

MAINTAINERCLEANFILES = Makefile.in

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
