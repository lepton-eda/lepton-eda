info_TEXINFOS = lepton-scheme.texi

AM_MAKEINFOHTMLFLAGS = --css-ref=lepton-scheme.css

EXTRA_DIST = lepton-scheme.css

html-local:
	$(MKDIR_P) $(builddir)/lepton-scheme.html/
	cp -fv $(srcdir)/lepton-scheme.css $(builddir)/lepton-scheme.html/

all-local: html

install-data-local: install-html
