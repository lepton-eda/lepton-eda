#
# NOTE: Don't forget that in the libtool distribution, files in this
# directory are distributed by the demo_distfiles variable in the top
# level Makefile.
AUTOMAKE_OPTIONS = foreign

NOTANGLE=       @NOTANGLE@
NOWEAVE=        @NOWEAVE@

NOTANGLEFLAGS=  
NOWEAVEFLAGS=   -n -index
TEXIBACKEND=    -backend ../scripts/geda_totexi
TEXIROOT=       ../docs/libgeda.texi


MAKEINFO=       @MAKEINFO@
TEXI2DVI=       @TEXI2DVI@
TEXI2HTML=      @TEXI2HTML@
INDENTBIN=      @INDENT@
INDENTFLAG=     -kr -i2


VPATH = ../noweb:../docs:src


# Build a libtool library, libhello.la for installation in libdir.
lib_LTLIBRARIES = libgeda.la

BUILT_SOURCES = \
	a_basic.nw f_basic.nw f_print.nw f_image.nw g_basic.nw m_basic.nw \
	o_arc_basic.nw o_attrib.nw o_basic.nw o_box_basic.nw \
	o_circle_basic.nw o_complex_basic.nw o_line_basic.nw o_list.nw \
	o_net_basic.nw o_selection.nw o_bus_basic.nw o_text_basic.nw \
	o_pin_basic.nw o_image.nw u_basic.nw s_attrib.nw s_basic.nw \
	s_clib.nw s_hierarchy.nw s_papersizes.nw s_stretch.nw s_log.nw \
	s_page.nw s_scratch.nw s_slib.nw s_color.nw s_undo.nw s_conn.nw \
	s_cue.nw s_tile.nw g_smob.nw libgeda.nw

libgeda_la_SOURCES = $(BUILT_SOURCES:.nw=.c)

libgeda_la_LDFLAGS = -version-info 13:0:0

include_HEADERS = 

INCLUDES = -I$(top_srcdir)/include @GUILE_CFLAGS@ @X_CFLAGS@  

EXTRA_DIST = create_proto 

TEXIFILES = $(BUILT_SOURCES:.nw=.texi)

src: $(libgeda_la_SOURCES)
	@echo Finished building the C source

info: $(TEXIFILES) $(TEXIROOT)
	$(MAKEINFO) $(TEXIROOT)

dvi: $(TEXIFILES) $(TEXIROOT)
	$(TEXI2DVI) $(TEXIROOT)

html: $(TEXIFILES) $(TEXIROOT)
	$(TEXI2HTML) -split_chapter $(TEXIROOT)


SUFFIXES = .nw .texi

.nw.c:
	$(NOTANGLE) $(NOTANGLEFLAGS) -R'$@ : *' $< > ../src/$@
	$(INDENTBIN) $(INDENTFLAG) ../src/$@
	rm -f ../src/$@~

.nw.texi:
	$(NOWEAVE) $(NOWEAVEFLAGS) $(TEXIBACKEND) $< > $@



MOSTLYCLEANFILES = *.log core FILE *~ prototype.bak
CLEANFILES = *.log core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ prototype.bak prototype.h \
			Makefile.in configure \
			*.c *.html *.dvi *.texi *.info *.info-* *.aux *.fn \
			*.fns *.cp *.ky *.pg *.toc *.tp *.vr 

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
