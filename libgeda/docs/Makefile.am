
NOWEAVE =		@NOWEAVE@
NOWEAVEFLAGS =		-n -index
TEXIBACKEND =		../scripts/geda_totexi

TEXI2HTML =		@TEXI2HTML@
TEXI2HTMLFLAGS =	-split_chapter

EXTRA_DIST = 		libgedadoc.texi texinfo.tex

info_TEXINFOS =		libgedadoc.texi
libgedadoc_TEXINFOS =	a_basic.texi f_basic.texi f_image.texi f_print.texi   \
			g_basic.texi g_smob.texi libgeda.texi m_basic.texi    \
			o_arc_basic.texi o_attrib.texi o_basic.texi           \
			o_box_basic.texi o_bus_basic.texi o_circle_basic.texi \
			o_complex_basic.texi o_image.texi o_line_basic.texi   \
			o_list.texi o_net_basic.texi o_pin_basic.texi         \
			o_selection.texi o_text_basic.texi s_attrib.texi      \
			s_basic.texi s_clib.texi s_color.texi s_conn.texi     \
			s_cue.texi s_hierarchy.texi s_log.texi s_menu.texi    \
			s_page.texi s_papersizes.texi s_scratch.texi          \
			s_slib.texi s_stretch.texi s_tile.texi s_undo.texi    \
			s_toplevel.texi u_basic.texi
TEXINFO_TEX =		./texinfo.tex


VPATH =			../noweb:docs
SUFFIXES = 		.nw .texi

.nw.texi:
	@echo 'Processing $@ ...'
if BUILDDOC
	$(NOWEAVE) $(NOWEAVEFLAGS) -backend $(TEXIBACKEND) $< > $@
else
	@echo '@node File $(@:.texi=.c),,,Top'                            > $@
	@echo '@chapter File @file{$(@:.texi=.c)}'                       >> $@
	@echo 'Noweave (part of noweb) is not installed on your system.' >> $@
	@echo 'The documentation can not be built from the sources.'     >> $@
endif

html: $(gschemdoc_TEXINFOS) $(info_TEXINFOS)
	$(TEXI2HTML) $(TEXI2HTMLFLAGS) $(info_TEXINFOS)


MOSTLYCLEANFILES = 	*.log *~
CLEANFILES = 		*.log *~
DISTCLEANFILES = 	*.log core FILE *~ prototype.bak           \
			*.html *.dvi *.info *.info-* *.aux *.fn    \
			*.fns *.cp *.ky *.pg *.toc *.tp *.vrs *.vr \
			$(libgedadoc_TEXINFOS)
MAINTAINERCLEANFILES = 	*.log *~ Makefile.in configure
