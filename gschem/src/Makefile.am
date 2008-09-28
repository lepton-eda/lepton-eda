#
# NOTE: Don't forget that in the libtool distribution, files in this
# directory are distributed by the demo_distfiles variable in the top
# level Makefile.
#

bin_PROGRAMS = gschem

gschem_SOURCES = \
	a_pan.c \
	a_zoom.c \
	g_funcs.c \
	g_hook.c \
	g_keys.c \
	g_rc.c \
	g_register.c \
	globals.c \
	gschem.c \
	gschem_dialog.c \
	gschem_toplevel.c \
	i_basic.c \
	i_callbacks.c \
	i_vars.c \
	o_arc.c \
	o_attrib.c \
	o_basic.c \
	o_box.c \
	o_buffer.c \
	o_bus.c \
	o_circle.c \
	o_complex.c \
	o_copy.c \
	o_cue.c \
	o_delete.c \
	o_find.c \
	o_grips.c \
	o_line.c \
	o_misc.c \
	o_move.c \
	o_net.c \
	o_path.c \
	o_picture.c \
	o_pin.c \
	o_place.c \
	o_select.c \
	o_slot.c \
	o_text.c \
	o_undo.c \
	parsecmd.c \
	x_attribedit.c \
	x_autonumber.c \
	x_basic.c \
	x_color.c \
	x_compselect.c \
	x_dialog.c \
	x_event.c \
	x_fileselect.c \
	x_grid.c \
	x_image.c \
	x_log.c \
	x_menus.c \
	x_multiattrib.c \
	x_pagesel.c \
	x_preview.c \
	x_print.c \
	x_script.c \
	x_stroke.c \
	x_window.c

CAIRO_SRCS =

EXTRA_gschem_SOURCES = \
	${CAIRO_SRCS}

if ENABLE_CAIRO
gschem_SOURCES += ${CAIRO_SRCS}
endif

if CCISGCC
AM_CFLAGS = -Wall
endif

INCLUDES = -I$(prefix)/include -I$(top_srcdir)/intl -I$(top_srcdir)/include @GSCHEM_CFLAGS@
gschem_LDADD = -L$(prefix)/lib @GSCHEM_LDFLAGS@ @LIBINTL@

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

#man_MANS = gschem.1
EXTRA_DIST = create_proto rcstrings.c
#gschem.1

src: $(gschem_SOURCES)
	@echo Finished building the C source

CPROTO =	cproto
CPROTOCFLAGS = 	$(INCLUDES) -I. -I.. -I../include
ALES_HACK =	-D__GNUC__

proto: prototype.h

prototype.h: $(gschem_SOURCES)
	$(CPROTO) $(ALES_HACK) $(CPROTOCFLAGS) *.c > new_proto
	mv new_proto ../include/prototype.h

potfiles:
	rm -f po/$(PACKAGE).pot
	@mv -f po/POTFILES.in po/POTFILES.in~ ; true
	grep '\([^A-Za-z0-9_]_(\)\|\(^_(\)\|\(gettext[[:blank:]]*(\)' \
	*.c | cut -d: -f1 | sort -u >po/POTFILES.in
	$(MAKE)

MOSTLYCLEANFILES = *.log core FILE *~ prototype.bak
CLEANFILES = *.log core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ prototype.bak prototype.h \
			Makefile.in configure 
