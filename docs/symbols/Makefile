
docname=symbols

all: dvi

dvi: $(docname).tex
	latex $(docname)
	latex $(docname)

ps: dvi
	dvi2ps $(docname) > $(docname).ps

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
