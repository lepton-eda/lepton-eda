# $Id$
#
# NOTE: Don't forget that in the libtool distribution, files in this
# directory are distributed by the demo_distfiles variable in the top
# level Makefile.
AUTOMAKE_OPTIONS = foreign

NOTANGLE=       @NOTANGLE@
NOTANGLEROOT=	@NOTANGLEROOT@
NOTANGLEFLAGS=  @NOTANGLEFLAGS@

INDENTBIN=      @INDENT@
INDENTFLAG=     -kr -i2

VPATH = ../noweb:../include:src

if CCISGCC
AM_CFLAGS = -Wall
endif

# Build a libtool library, libhello.la for installation in libdir.
lib_LTLIBRARIES = libgeda.la

BUILT_SOURCES = \
	a_basic.nw f_basic.nw f_print.nw f_image.nw g_basic.nw m_basic.nw \
	o_arc_basic.nw o_attrib.nw o_basic.nw o_box_basic.nw \
	o_circle_basic.nw o_complex_basic.nw o_line_basic.nw o_list.nw \
	o_net_basic.nw o_selection.nw o_bus_basic.nw o_text_basic.nw \
	o_pin_basic.nw o_image.nw u_basic.nw s_attrib.nw s_basic.nw \
	s_clib.nw s_hierarchy.nw s_papersizes.nw s_stretch.nw s_log.nw \
	s_page.nw s_slib.nw s_color.nw s_undo.nw s_conn.nw \
	s_cue.nw s_tile.nw s_menu.nw s_toplevel.nw g_smob.nw libgeda.nw \
	g_register.nw g_rc.nw i_vars.nw o_picture.nw gdk-pixbuf-hacks.nw

libgeda_la_SOURCES = $(BUILT_SOURCES:.nw=.c)

INCLUDES = -I$(top_srcdir)/include @LIBGEDA_CFLAGS@
libgeda_la_LDFLAGS = @LIBTOOL_FLAGS@ @LIBGEDA_LDFLAGS@


EXTRA_DIST = create_proto 

TEXIFILES = $(BUILT_SOURCES:.nw=.texi)

src: $(libgeda_la_SOURCES)
	@echo Finished building the C source

SUFFIXES = .nw

.nw.c:
	$(NOTANGLE) $(NOTANGLEFLAGS) $(NOTANGLEROOT)'$@ : *' $< > ../src/$@
	$(INDENTBIN) $(INDENTFLAG) ../src/$@
	rm -f ../src/$@~

CPROTO =	cproto
CPROTOCFLAGS = 	$(INCLUDES) -I. -I.. -I../include
ALES_HACK =	-D__GNUC__

proto: prototype.h

prototype.h: $(libgeda_la_SOURCES)
	$(CPROTO) $(ALES_HACK) $(CPROTOCFLAGS) *.c > new_proto
	mv new_proto ../include/prototype.h


MOSTLYCLEANFILES = *.log core FILE *~ prototype.bak
CLEANFILES = *.log core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ prototype.bak prototype.h \
			Makefile.in configure \
			*.c

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
