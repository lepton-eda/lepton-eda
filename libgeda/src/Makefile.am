#
# NOTE: Don't forget that in the libtool distribution, files in this
# directory are distributed by the demo_distfiles variable in the top
# level Makefile.
AUTOMAKE_OPTIONS = foreign

NOTANGLE=       notangle
NOTANGLEFLAGS=  
NOWEAVE=        noweave
NOWEAVEFLAGS=   -n -index
TEXIBACKEND=    -backend ../scripts/geda_totexi
MAKEINFO=       makeinfo
TEXI2DVI=       texi2dvi
TEXI2HTML=      texi2html
INDENTBIN=      indent
INDENTFLAG=     -kr -i2


VPATH = ../noweb:src


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

libgeda_la_SOURCES = \
	a_basic.c f_basic.c f_print.c f_image.c g_basic.c m_basic.c \
	o_arc_basic.c o_attrib.c o_basic.c o_box_basic.c \
	o_circle_basic.c o_complex_basic.c o_line_basic.c o_list.c \
	o_net_basic.c o_selection.c o_bus_basic.c o_text_basic.c \
	o_pin_basic.c o_image.c u_basic.c s_attrib.c s_basic.c \
	s_clib.c s_hierarchy.c s_papersizes.c s_stretch.c s_log.c \
	s_page.c s_scratch.c s_slib.c s_color.c s_undo.c s_conn.c \
	s_cue.c s_tile.c g_smob.c libgeda.c

libgeda_la_LDFLAGS = -version-info 12:0:0

include_HEADERS = 

INCLUDES = -I$(top_srcdir)/include @GUILE_CFLAGS@ @X_CFLAGS@  

EXTRA_DIST = create_proto 

SUFFIXES = .nw

.nw.c:
	$(NOTANGLE) $(NOTANGLEFLAGS) -R'$@ : *' $< > ../src/$@
	$(INDENTBIN) $(INDENTFLAG) ../src/$@
	rm -f ../src/$@~


MOSTLYCLEANFILES = *.log core FILE *~ prototype.bak
CLEANFILES = *.log core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak Makefile.in configure
MAINTAINERCLEANFILES = *.log core FILE *~ prototype.bak prototype.h Makefile.in configure *.c

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
