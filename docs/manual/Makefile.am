info_TEXINFOS = lepton-manual.texi
lepton_manual_TEXINFOS = \
	communication.texi \
	configuration.texi \
	development.texi \
	geda-file-format-spec.texi \
	installation.texi \
	lepton-netlist.texi \
	lepton-schematic.texi

AM_MAKEINFOHTMLFLAGS = --css-ref=lepton-manual.css

PICTURES = \
	coordinate-space.eps \
	coordinate-space.png \
	coordinate-space.sch \
	path-example.eps \
	path-example.png \
	path-example.sch \
	text-layout.eps \
	text-layout.png \
	text-layout.sch

# lepton-cli export --no-color -k 0,5cm -o coordinate-space.png coordinate-space.sch
# lepton-cli export --no-color -o coordinate-space.pdf coordinate-space.sch && pdftops -eps coordinate-space.pdf coordinate-space.eps
# lepton-cli export -k 1cm -o text-layout.png text-layout.sch
# lepton-cli export -m 0,1cm -o text-layout.pdf text-layout.sch && pdftops -eps text-layout.pdf text-layout.eps
# lepton-cli export -k 1cm -o path-example.png path-example.sch
# lepton-cli export -o path-example.pdf path-example.sch && pdftops -eps path-example.pdf path-example.eps
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
