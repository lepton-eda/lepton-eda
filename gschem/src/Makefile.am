## Process this file with automake to produce Makefile.in

bin_PROGRAMS = gschem

## scmdatadir = @gedadatadir@/scheme
## scmdata_DATA = gschem.scm

# don't forget all *.h files */
gschem_SOURCES = \
	a_pan.c a_zoom.c \
	g_register.c \
	g_rc.c g_key.c g_funcs.c \
	globals.c gschem.c \
	i_basic.c i_callbacks.c i_sbox.c i_vars.c \
	o_basic.c o_attrib.c o_move.c o_copy.c o_delete.c o_misc.c \
	o_slot.c o_stretch.c \
	o_arc.c o_box.c o_circle.c o_complex.c \
	o_line.c o_net.c o_text.c o_pin.c o_bus.c \
	x_attrsel.c x_basic.c x_dialog.c \
	x_event.c x_grid.c x_log.c x_menus.c x_script.c \
	x_pagesel.c x_print.c x_window.c x_stroke.c x_image.c x_color.c \
	x_fs.c x_preview.c \
	prototype1.0.h prototype1.2.h \
	parsecmd.c globals.h i_vars.h o_conn.c

#INCLUDES = -I../include @GUILE_INCLUDE@ @X_CFLAGS@  

INCLUDES = @GUILE_INCLUDE@ @X_CFLAGS@  
gschem_LDADD = @STROKE_LIB@ @GUILE_LIB@ @GTK@ @GDK@ @GLIB@ @X_LDFLAGS@ 

man_MANS = gschem.1

EXTRA_DIST = create_proto gschem.1

MOSTLYCLEANFILES = *.log *.ps core FILE *~ prototype.bak
CLEANFILES = *.log *.ps core FILE *~ prototype.bak
DISTCLEANFILES = *.log *.ps core FILE *~ prototype.bak
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ prototype.bak prototype.h

