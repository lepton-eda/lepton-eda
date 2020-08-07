info_TEXINFOS = lepton-manual.texi

AM_MAKEINFOHTMLFLAGS = --css-ref=lepton-manual.css

EXTRA_DIST = lepton-manual.css scm-component.scm cmd-component.sh

all: all-am html
	$(MKDIR_P) $(builddir)/lepton-manual.html/
	cp -fv $(srcdir)/lepton-manual.css $(builddir)/lepton-manual.html/

install-data-local: install-html
