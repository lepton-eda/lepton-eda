#-----------------------------------------------------------------------------
#
# Makefile pour répertoire gschem/noweb
#
#-----------------------------------------------------------------------------

# Pour documentation : noweb
NOTANGLE=	notangle
NOTANGLEFLAGS=	
NOWEAVE=	noweave
NOWEAVEFLAGS=	-n -index
TEXIBACKEND=	-backend geda_totexi

# piur génération doc
MAKEINFO=	makeinfo
TEXI2DVI=	texi2dvi
TEXI2HTML=	texi2html

# Pour réindentation : indent
INDENTBIN=	indent
INDENTFLAG=	-kr -i2


NWFILES=	a_pan.nw a_zoom.nw					\
		g_funcs.nw g_hook.nw g_key.nw g_rc.nw g_register.nw	\
		globals.nw gschem.nw					\
		i_basic.nw i_callbacks.nw i_vars.nw			\
		o_arc.nw o_attrib.nw o_basic.nw o_box.nw o_buffer.nw 	\
		o_bus.nw o_circle.nw o_complex.nw o_copy.nw o_cue.nw 	\
		o_delete.nw o_find.nw o_grips.nw o_line.nw o_misc.nw 	\
		o_move.nw o_net.nw o_pin.nw o_select.nw o_slot.nw 	\
		o_text.nw o_undo.nw					\
		parsecmd.nw						\
		x_attrsel.nw x_basic.nw x_color.nw x_dialog.nw		\
		x_event.nw x_fileselect.nw x_grid.nw x_image.nw		\
		x_log.nw x_menus.nw x_multimulti.nw x_pagesel.nw	\
		x_preview.nw x_print.nw x_script.nw x_stroke.nw		\
		x_window.nw

SRCFILES=	a_pan.c a_zoom.c					\
		g_funcs.c g_hook.c g_key.c g_rc.c g_register.c		\
		globals.c gschem.c					\
		i_basic.c i_callbacks.c i_vars.c			\
		o_arc.c o_attrib.c o_basic.c o_box.c o_buffer.c 	\
		o_bus.c o_circle.c o_complex.c o_copy.c o_cue.c 	\
		o_delete.c o_find.c o_grips.c o_line.c o_misc.c 	\
		o_move.c o_net.c o_pin.c o_select.c o_slot.c 		\
		o_text.c o_undo.c					\
		parsecmd.c						\
		x_attrsel.c x_basic.c x_color.c x_dialog.c		\
		x_event.c x_fileselect.c x_grid.c x_image.c		\
		x_log.c x_menus.c x_multimulti.c x_pagesel.c		\
		x_preview.c x_print.c x_script.c x_stroke.c		\
		x_window.c

TEXIFILES=	a_pan.texi a_zoom.texi					\
		g_funcs.texi g_hook.texi g_key.texi g_rc.texi 		\
		g_register.texi globals.texi gschem.texi		\
		i_basic.texi i_callbacks.texi i_vars.texi		\
		o_arc.texi o_attrib.texi o_basic.texi o_box.texi 	\
		o_buffer.texi o_bus.texi o_circle.texi o_complex.texi	\
		o_copy.texi o_cue.texi o_delete.texi o_find.texi 	\
		o_grips.texi o_line.texi o_misc.texi o_move.texi 	\
		o_net.texi o_pin.texi o_select.texi o_slot.texi 	\
		o_text.texi o_undo.texi					\
		parsecmd.texi						\
		x_attrsel.texi x_basic.texi x_color.texi x_dialog.texi	\
		x_event.texi x_fileselect.texi x_grid.texi x_image.texi	\
		x_log.texi x_menus.texi x_multimulti.texi x_pagesel.texi\
		x_preview.texi x_print.texi x_script.texi x_stroke.texi	\
		x_window.texi

TEXIROOT=	gschemdoc.texi


all: $(SRCFILES)

install: all 
	cp -f $(SRCFILES) ../src/

indent:
	@for file in $(SRCFILES); do \
		echo Indenting file $$file ...; \
		$(INDENTBIN) $(INDENTFLAG) $$file; \
	done

info: $(TEXIFILES) $(TEXIROOT)
	$(MAKEINFO) $(TEXIROOT)

dvi: $(TEXIFILES) $(TEXIROOT)
	$(TEXI2DVI) $(TEXIROOT)

html: $(TEXIFILES) $(TEXIROOT)
	$(TEXI2HTML) -split_chapter $(TEXIROOT)


.SUFFIXES: .c .texi .nw
.nw.c:
	$(NOTANGLE) $(NOTANGLEFLAGS) -R'$@ : *' $< > ../src/$@
	$(INDENTBIN) $(INDENTFLAG) ../src/$@
#	cp -f $@ ../src/
.nw.texi:
	$(NOWEAVE) $(NOWEAVEFLAGS) $(TEXIBACKEND) $< > $@


clean:
	rm -f *~
	rm -f $(SRCFILES) $(TEXIFILES)

distclean: clean
	rm -f *.dvi *.ps
