## $Id$
##

docname=pads_backannotate
docsdir = @GEDADOCDIR@/$(docname)
docs_DATA = $(docname).dvi $(docname).ps $(docname).pdf $(docname)/index.html

BUILT_SOURCES = $(docname).dvi $(docname).ps $(docname).pdf \
                $(docname)

EXTRA_DIST = $(docname).tex $(BUILT_SOURCES)

$(docname).dvi: $(docname).tex
	latex $(docname)
	latex $(docname)

$(docname).ps: $(docname).dvi
	if which dvips >/dev/null 2>/dev/null; then             \
		dvips -f < $(docname).dvi > $(docname).ps     ; \
	else                                                    \
		dvi2ps $(docname) > $(docname).ps             ; \
	fi

$(docname).pdf: $(docname).ps
	ps2pdf $(docname).ps 

$(docname)/index.html: $(docname).tex
	latex2html -local_icons $(docname)

$(docname): $(docname)/index.html

install-data-hook: $(docname)/index.html
	( $(INSTALL) -d $(DESTDIR)$(docsdir); \
          for i in $(docname)/*; do \
		$(INSTALL_DATA) $$i $(DESTDIR)$(docsdir); \
	  done )

uninstall-hook:
	( cd $(docname) ; $(INSTALL) -d $(DESTDIR)$(docsdir); \
          for i in *; do \
		rm -f $(DESTDIR)$(docsdir)/$$i; \
	  done; \
	  rmdir $(DESTDIR)$(docsdir) )

tgz: $(docname).tex $(docname).dvi $(docname).ps $(docname).pdf \
     $(docname)/index.html
	tar cvfz $(docname).tgz $(docname).tex $(docname).ps $(docname).pdf \
	         $(docname).dvi $(docname)

clean-local:
	rm -f $(docname).aux $(docname).dvi $(docname).log \
	      $(docname).pdf $(docname).ps *~ $(docname).toc $(docname).tgz \
	      $(docname).out 
	rm -rf $(docname)

maintainer-clean-local:
	rm -f $(docname).aux $(docname).dvi $(docname).log \
	      $(docname).pdf $(docname).ps *~ $(docname).toc $(docname).tgz \
              $(docname).out Makefile.in Makefile
	rm -rf $(docname)
dist-clean-local:
	rm -f $(docname).aux $(docname).dvi $(docname).log \
	      $(docname).pdf $(docname).ps *~ $(docname).toc $(docname).tgz \
	      $(docname).out Makefile
	rm -rf $(docname)

