man_MANS = 		gschem.1

EXTRA_DIST = 		gschem-main.txt what-is-geda.html what-is-gschem.html\
			gschem.1 logo.png

all:
	@echo 'Type: make doxygen to create doxygen documentation for gschem'

doxygen: gschem.dox
	@echo 'Creating doxygen documentation for gschem...'
if BUILDDOC
	$(DOXYGEN) gschem.dox
	-cd latex && $(MAKE)
	-cd ..
else
	@echo 'Doxygen is not installed on your system.' >> $@
	@echo 'The documentation can not be built from the sources.' >> $@
endif

MOSTLYCLEANFILES = 	*.log *~
CLEANFILES = 		*.log *~
DISTCLEANFILES = 	*.log core FILE *~ prototype.bak \
			*.dvi *.info *.info-* *.aux *.fn \
			*.fns *.cp *.ky *.pg *.toc *.tp *.vrs *.vr 
MAINTAINERCLEANFILES = 	*.log *~ Makefile.in configure

distclean-local:
	-rm -rf html latex

maintainer-clean-local:
	-rm -rf html latex
