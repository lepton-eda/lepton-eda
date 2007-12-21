# $Id$
#
# NOTE: Don't forget that in the libtool distribution, files in this
# directory are distributed by the demo_distfiles variable in the top
# level Makefile.
AUTOMAKE_OPTIONS = foreign

if CCISGCC
AM_CFLAGS = -Wall
endif

# Build a libtool library, libhello.la for installation in libdir.
lib_LTLIBRARIES = libgeda.la

libgeda_la_SOURCES = \
	a_basic.c \
	f_basic.c \
	f_print.c \
	g_basic.c \
	g_rc.c \
	g_register.c \
	g_smob.c \
	gdk-pixbuf-hacks.c \
	geda_list.c \
	i_vars.c \
	libgeda.c \
	m_basic.c \
	o_arc_basic.c \
	o_attrib.c \
	o_basic.c \
	o_box_basic.c \
	o_bus_basic.c \
	o_circle_basic.c \
	o_complex_basic.c \
	o_embed.c \
	o_line_basic.c \
	o_list.c \
	o_net_basic.c \
	o_picture.c \
	o_pin_basic.c \
	o_selection.c \
	o_text_basic.c \
	s_attrib.c \
	s_basic.c \
	s_clib.c \
	s_color.c \
	s_conn.c \
	s_cue.c \
	s_encoding.c \
	s_hierarchy.c \
	s_log.c \
	s_menu.c \
	s_page.c \
	s_papersizes.c \
	s_slib.c \
	s_stretch.c \
	s_textbuffer.c \
	s_tile.c \
	s_toplevel.c \
	s_undo.c \
	u_basic.c

INCLUDES = -I$(top_srcdir)/include @LIBGEDA_CFLAGS@
libgeda_la_LDFLAGS = @LIBTOOL_FLAGS@ @LIBGEDA_LDFLAGS@
LIBTOOL=@LIBTOOL@ --silent

EXTRA_DIST = create_proto 

src: $(libgeda_la_SOURCES)
	@echo Finished building the C source

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
			Makefile.in configure

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`
