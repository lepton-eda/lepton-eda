#
# NOTE: Don't forget that in the libtool distribution, files in this
# directory are distributed by the demo_distfiles variable in the top
# level Makefile.
#

NOTANGLE=       @NOTANGLE@
NOWEAVE=        @NOWEAVE@

NOTANGLEFLAGS=  
NOWEAVEFLAGS=   -n -index
TEXIBACKEND=    -backend ../scripts/geda_totexi
TEXIROOT=       ../docs/gschemdoc.texi


MAKEINFO=       @MAKEINFO@
TEXI2DVI=       @TEXI2DVI@
TEXI2HTML=      @TEXI2HTML@
INDENTBIN=      @INDENT@
INDENTFLAG=     -kr -i2

VPATH = ../noweb:../docs:src

bin_PROGRAMS = gschem

BUILT_SOURCES = \
	a_pan.nw a_zoom.nw \
	g_key.nw g_rc.nw g_funcs.nw g_hook.nw g_register.nw \
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

gschem_SOURCES = $(BUILT_SOURCES:.nw=.c)

INCLUDES = @GUILE_CFLAGS@ @X_CFLAGS@
gschem_LDADD = @STROKE_LIB@ @GTK@ @GDK@ @GLIB@ @X_LDFLAGS@ @GUILE_LDFLAGS@

#man_MANS = gschem.1
EXTRA_DIST = create_proto 
#gschem.1

TEXIFILES = $(BUILT_SOURCES:.nw=.texi)

src: $(gschem_SOURCES)
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
			*.fns *.cp *.ky *.pg *.toc *.tp *.vrs *.vr

