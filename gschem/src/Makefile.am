## Process this file with automake to produce Makefile.in

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

bin_PROGRAMS = gschem


## scmdatadir = @gedadatadir@/scheme
## scmdata_DATA = gschem.scm


BUILT_SOURCES = 
	a_pan.nw a_zoom.nw \
	g_register.nw g_hook.nw\
	g_rc.nw g_key.nw g_funcs.nw \
	globals.nw gschem.nw \
	i_basic.nw i_callbacks.nw i_vars.nw \
	o_basic.nw o_attrib.nw o_move.nw o_copy.nw o_delete.nw o_misc.nw \
	o_slot.nw o_grips.nw \
	o_arc.nw o_box.nw o_circle.nw o_complex.nw o_select.nw o_find.nw \
	o_line.nw o_net.nw o_text.nw o_pin.nw o_bus.nw o_buffer.nw o_undo.nw \
	x_attrsel.nw x_basic.nw x_dialog.nw \
	x_event.nw x_grid.nw x_log.nw x_menus.nw x_script.nw \
	x_pagesel.nw x_print.nw x_window.nw x_stroke.nw x_image.nw x_color.nw \
	x_fileselect.nw x_preview.nw \
	x_multimulti.nw \
	parsecmd.nw o_cue.nw


gschem_SOURCES = \
	a_pan.c a_zoom.c \
	g_register.c g_hook.c\
	g_rc.c g_key.c g_funcs.c \
	globals.c gschem.c \
	i_basic.c i_callbacks.c i_vars.c \
	o_basic.c o_attrib.c o_move.c o_copy.c o_delete.c o_misc.c \
	o_slot.c o_grips.c \
	o_arc.c o_box.c o_circle.c o_complex.c o_select.c o_find.c \
	o_line.c o_net.c o_text.c o_pin.c o_bus.c o_buffer.c o_undo.c \
	x_attrsel.c x_basic.c x_dialog.c \
	x_event.c x_grid.c x_log.c x_menus.c x_script.c \
	x_pagesel.c x_print.c x_window.c x_stroke.c x_image.c x_color.c \
	x_fileselect.c x_preview.c \
	x_multimulti.c \
	parsecmd.c o_cue.c

#INCLUDES = -I../include @GUILE_INCLUDE@ @X_CFLAGS@  

INCLUDES = @GUILE_CFLAGS@ @X_CFLAGS@  
gschem_LDADD = @STROKE_LIB@ @GUILE_LDFLAGS@ @GTK@ @GDK@ @GLIB@ @X_LDFLAGS@ 

#man_MANS = gschem.1

EXTRA_DIST = create_proto 

#gschem.1


SUFFIXES = .nw

.nw.c:
	$(NOTANGLE) $(NOTANGLEFLAGS) -R'$@ : *' $< > ../src/$@
	$(INDENTBIN) $(INDENTFLAG) ../src/$@
	rm -f ../src/$@~


MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log *.ps core FILE *~ prototype.bak Makefile.in configure
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h Makefile.in configure *.c

