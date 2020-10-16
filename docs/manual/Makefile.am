info_TEXINFOS = lepton-manual.texi
lepton_manual_TEXINFOS = \
	geda-file-format-spec.texi \
	lepton-schematic.texi

AM_MAKEINFOHTMLFLAGS = --css-ref=lepton-manual.css

PICTURES = \
	coordinate-space.eps \
	coordinate-space.png \
	coordinate-space.sch \
	text-layout.eps \
	text-layout.png \
	text-layout.sch

# lepton-cli export --no-color -k 0,5cm -o coordinate-space.png coordinate-space.sch
# lepton-cli export --no-color -o coordinate-space.pdf coordinate-space.sch && pdftops -eps coordinate-space.pdf coordinate-space.eps
noinst_DATA = $(PICTURES)

EXTRA_DIST = \
	$(PICTURES) \
	lepton-manual.css \
	scm-component.scm \
	cmd-component.sh

all: all-am html
	$(MKDIR_P) $(builddir)/lepton-manual.html/
	cp -fv $(srcdir)/lepton-manual.css $(builddir)/lepton-manual.html/

install-data-local: install-html
