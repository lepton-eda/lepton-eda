#
# NOTE: Don't forget that in the libtool distribution, files in this
# directory are distributed by the demo_distfiles variable in the top
# level Makefile.
#

NOTANGLE=       @NOTANGLE@
NOTANGLEROOT=	@NOTANGLEROOT@
NOTANGLEFLAGS=  @NOTANGLEFLAGS@

INDENTBIN=      @INDENT@
INDENTFLAG=     -kr -i2

VPATH = ../noweb:../include:src

bin_PROGRAMS = gschem

BUILT_SOURCES = \
	a_pan.nw a_zoom.nw \
	g_register.nw g_hook.nw\
	g_rc.nw g_keys.nw g_funcs.nw \
	globals.nw gschem.nw \
	i_basic.nw i_callbacks.nw i_vars.nw \
	o_basic.nw o_attrib.nw o_move.nw o_copy.nw o_delete.nw o_misc.nw \
	o_slot.nw o_grips.nw \
	o_arc.nw o_box.nw o_circle.nw o_complex.nw o_select.nw o_find.nw \
	o_line.nw o_net.nw o_text.nw o_pin.nw o_bus.nw o_buffer.nw o_undo.nw \
	o_picture.nw \
	x_basic.nw x_dialog.nw \
	x_event.nw x_grid.nw x_log.nw x_menus.nw x_script.nw \
	x_pagesel.nw x_print.nw x_window.nw x_stroke.nw x_image.nw x_color.nw \
	x_fileselect.nw x_preview.nw x_attribedit.nw \
	x_multiattrib.nw \
	parsecmd.nw o_cue.nw

gschem_SOURCES = $(BUILT_SOURCES:.nw=.c)

INCLUDES = -I$(top_srcdir)/intl -I$(top_srcdir)/include @GSCHEM_CFLAGS@
gschem_LDADD = @GSCHEM_LDFLAGS@ @LIBINTL@

localedir = @datadir@/locale
DEFS = -DLOCALEDIR=\"$(localedir)\" @DEFS@

#man_MANS = gschem.1
EXTRA_DIST = create_proto 
#gschem.1

src: $(gschem_SOURCES)
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

prototype.h: $(gschem_SOURCES)
	$(CPROTO) $(ALES_HACK) $(CPROTOCFLAGS) *.c > new_proto
	mv new_proto ../include/prototype.h

potfiles:
	rm -f po/$(PACKAGE).pot
	@mv -f po/POTFILES.in po/POTFILES.in~ ; true
	grep '\([^A-Za-z0-9_]_(\)\|\(^_(\)\|\(gettext[[:blank:]]*(\)' \
	noweb/*.nw | cut -d: -f1 | sort -u >po/POTFILES.in
	$(MAKE)

MOSTLYCLEANFILES = *.log core FILE *~ prototype.bak
CLEANFILES = *.log core FILE *~ prototype.bak
DISTCLEANFILES = *.log core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log core FILE *~ prototype.bak prototype.h \
			Makefile.in configure \
			*.c 

