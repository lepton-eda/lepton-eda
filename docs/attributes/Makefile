
docname=attributes

all: dvi

dvi: $(docname).tex
	latex $(docname)
	latex $(docname)

ps: dvi
	if which dvips >/dev/null 2>/dev/null; then             \
		dvips -f < $(docname).dvi > $(docname).ps     ; \
	else                                                    \
		dvi2ps $(docname) > $(docname).ps             ; \
       fi

pdf: ps
	ps2pdf $(docname).ps 

html: $(docname).tex
	latex2html -local_icons $(docname)

tgz: dvi ps pdf html
	tar cvfz $(docname).tgz $(docname).tex $(docname).ps $(docname).pdf \
	         $(docname).dvi $(docname)

clean:
	rm -f $(docname).aux $(docname).dvi $(docname).log \
	      $(docname).pdf $(docname).ps *~ $(docname).toc $(docname).tgz
	rm -rf $(docname)
