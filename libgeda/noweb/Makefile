#-----------------------------------------------------------------------------
#
# Makefile pour répertoire libgeda/noweb
#
#-----------------------------------------------------------------------------

# Pour documentation : noweb
NOTANGLE=	notangle
NOTANGLEFLAGS=	
NOWEAVE=	noweave
NOWEAVEFLAGS=	-n -index
TEXIBACKEND=	-backend ../scripts/geda_totexi

# Pour génération doc
MAKEINFO=	makeinfo
TEXI2DVI=	texi2dvi
TEXI2HTML=	texi2html

# Pour réindentation : indent
INDENTBIN=	indent
INDENTFLAG=	-kr -i2


NWFILES=	a_basic.nw						\
		f_basic.nw f_image.nw f_print.nw 			\
		g_basic.nw g_smob.nw 					\
		m_basic.nw						\
		o_arc_basic.nw o_attrib.nw o_basic.nw o_box_basic.nw 	\
		o_bus_basic.nw o_circle_basic.nw o_complex_basic.nw 	\
		o_image.nw o_line_basic.nw o_list.nw o_net_basic.nw 	\
		o_pin_basic.nw o_selection.nw o_text_basic.nw 		\
		s_attrib.nw s_basic.nw s_clib.nw s_color.nw s_conn.nw 	\
		s_cue.nw s_hierarchy.nw s_log.nw s_page.nw 		\
		s_papersizes.nw s_scratch.nw s_slib.nw s_stretch.nw 	\
		s_tile.nw s_undo.nw 					\
		u_basic.nw libgeda.nw

SRCFILES=	a_basic.c						\
		f_basic.c f_image.c f_print.c 				\
		g_basic.c g_smob.c 					\
		m_basic.c						\
		o_arc_basic.c o_attrib.c o_basic.c o_box_basic.c 	\
		o_bus_basic.c o_circle_basic.c o_complex_basic.c 	\
		o_image.c o_line_basic.c o_list.c o_net_basic.c 	\
		o_pin_basic.c o_selection.c o_text_basic.c 		\
		s_attrib.c s_basic.c s_clib.c s_color.c s_conn.c 	\
		s_cue.c s_hierarchy.c s_log.c s_page.c 			\
		s_papersizes.c s_scratch.c s_slib.c s_stretch.c 	\
		s_tile.c s_undo.c 					\
		u_basic.c libgeda.c

TEXIFILES=	a_basic.texi						\
		f_basic.texi f_image.texi f_print.texi 			\
		g_basic.texi g_smob.texi 				\
		m_basic.texi						\
		o_arc_basic.texi o_attrib.texi o_basic.texi 		\
		o_box_basic.texi o_bus_basic.texi o_circle_basic.texi 	\
		o_complex_basic.texi o_image.texi o_line_basic.texi 	\
		o_list.texi o_net_basic.texi o_pin_basic.texi 		\
		o_selection.texi o_text_basic.texi 			\
		s_attrib.texi s_basic.texi s_clib.texi s_color.texi 	\
		s_conn.texi s_cue.texi s_hierarchy.texi s_log.texi 	\
		s_page.texi s_papersizes.texi s_scratch.texi 		\
		s_slib.texi s_stretch.texi s_tile.texi s_undo.texi	\
		u_basic.texi libgeda.texi

TEXIROOT=	../docs/libgeda.texi


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
