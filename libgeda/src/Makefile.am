#
# NOTE: Don't forget that in the libtool distribution, files in this
# directory are distributed by the demo_distfiles variable in the top
# level Makefile.
AUTOMAKE_OPTIONS = foreign

EXTRA_DIST = 


# Build a libtool library, libhello.la for installation in libdir.
lib_LTLIBRARIES = libgeda.la
libgeda_la_SOURCES = \
	a_basic.c f_basic.c f_print.c f_image.c g_basic.c m_basic.c \
	o_arc_basic.c o_attrib.c o_basic.c o_box_basic.c o_circle_basic.c \
	o_complex_basic.c o_line_basic.c o_list.c o_net_basic.c o_selection.c \
	o_bus_basic.c o_text_basic.c o_pin_basic.c o_image.c u_basic.c \
	s_attrib.c s_basic.c s_clib.c s_hierarchy.c s_papersizes.c \
	s_log.c s_page.c s_scratch.c s_slib.c s_stroke.c s_color.c s_undo.c \
	o_conn.c s_nethash.c funcs.h globals.h 

libgeda_la_LDFLAGS = -version-info 9:0:0


include_HEADERS = 

INCLUDES = -I../include @GUILE_INCLUDE@ @X_CFLAGS@  

EXTRA_DIST = BUGS TODO create_proto 


MOSTLYCLEANFILES = *.log core FILE *~ prototype.bak
CLEANFILES = *.log core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ prototype.bak prototype.h

# Unfortunately, in order to test libtool thoroughly, we need access
# to its private directory.
objdir = `sed -n -e 's/^objdir=\"\(.*\)\"$$/\1/p' ./libtool`

