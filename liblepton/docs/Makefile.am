SUBDIRS = images

EXTRA_DIST = \
	geda-logo.png \
	liblepton.dox \
	README.libleptonrenderer \
	HACKING

SUFFIXES = .html .tex .pdf

all:
	@echo 'Type: "make doxygen" to create doxygen documentation for liblepton'

doxygen: liblepton.dox
	@echo 'Creating doxygen documentation for liblepton...'
if ENABLE_DOXYGEN
	-(cd images && $(MAKE) images)
	$(DOXYGEN) liblepton.dox
else
	@echo 'configure was not run with the --enable-doxygen switch.'
	@echo 'The documentation cannot be built from the sources.'
endif


distclean-local:
	-rm -rf html

maintainer-clean-local:
	-rm -rf html
