info_TEXINFOS = lepton-manual.texi
lepton_manual_TEXINFOS = \
	attribute-net.texi \
	attributes.texi \
	communication.texi \
	configuration.texi \
	development.texi \
	geda-file-format-spec.texi \
	index.texi \
	installation.texi \
	lepton-attrib.texi \
	lepton-cli-config.texi \
	lepton-cli-export.texi \
	lepton-cli-shell.texi \
	lepton-cli.texi \
	lepton-eda-fonts.texi \
	lepton-netlist.texi \
	lepton-netlist-allegro.texi \
	lepton-netlist-bom.texi \
	lepton-schematic.texi \
	logging.texi \
	no-connect-symbol.texi \
	overview.texi \
	placeholder.texi \
	symbol-creation.texi \
	symbol-editing.texi \
	symbol-version-attribute.texi \
	symbols.texi \
	title-block-symbol.texi

AM_MAKEINFOHTMLFLAGS = --css-ref=lepton-manual.css

schematics = \
	coordinate-space.sch \
	path-example.sch \
	text-layout.sch

schematics_eps = ${schematics:.sch=.eps}
schematics_png = ${schematics:.sch=.png}

PICTURES = $(schematics) $(schematics_eps) $(schematics_png)

infoimagesdir = $(infodir)
infoimages_DATA = $(schematics_png)

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
	for file in $(schematics_png); do cp $(srcdir)/$$file ${builddir}/lepton-manual.html/; done

install-data-local: install-html
