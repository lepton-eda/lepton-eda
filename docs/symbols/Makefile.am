docname=symbols
docsdir = @GEDADATADIR@/documentation/$(docname)
docs_DATA = $(docname).dvi $(docname).ps $(docname).pdf $(docname)/index.html

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

install-data-hook: $(docname)/index.html
	( cd $(docname) ; mkdir -p $(docsdir); \
          cp -rp * $(docsdir) )

tgz: $(docname).tex $(docname).dvi $(docname).ps $(docname).pdf \
     $(docname)/index.html
	tar cvfz $(docname).tgz $(docname).tex $(docname).ps $(docname).pdf \
	         $(docname).dvi $(docname)

maintainer-clean-local:
dist-clean-local:
clean-local:
	rm -f $(docname).aux $(docname).dvi $(docname).log \
	      $(docname).pdf $(docname).ps *~ $(docname).toc $(docname).tgz \
              Makefile.in
	rm -rf $(docname)
