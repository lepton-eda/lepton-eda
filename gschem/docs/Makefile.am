
NOWEAVE =		@NOWEAVE@
NOWEAVEFLAGS =		-n -index -t4
TEXIBACKEND =		../scripts/geda_totexi

TEXI2HTML =		@TEXI2HTML@
TEXI2HTMLFLAGS =	-split_chapter

man_MANS = 		gschem.1

EXTRA_DIST = 		gschemdoc.texi texinfo.tex gschem.1 gschem.html

BUILT_SOURCES = 	gschem.html

docsdir =               @GEDADOCDIR@/man
docs_DATA =             gschem.html

info_TEXINFOS =		gschemdoc.texi
gschemdoc_TEXINFOS =	a_pan.texi a_zoom.texi g_funcs.texi g_hook.texi       \
			g_key.texi g_rc.texi g_register.texi globals.texi     \
			gschem.texi i_basic.texi i_callbacks.texi i_vars.texi \
			o_arc.texi o_attrib.texi o_basic.texi o_box.texi      \
			o_buffer.texi o_bus.texi o_circle.texi o_complex.texi \
			o_copy.texi o_cue.texi o_delete.texi o_find.texi      \
			o_grips.texi o_line.texi o_misc.texi o_move.texi      \
			o_net.texi o_pin.texi o_select.texi o_slot.texi       \
			o_text.texi o_undo.texi parsecmd.texi                 \
			x_basic.texi x_color.texi x_dialog.texi x_event.texi  \
			x_fileselect.texi x_grid.texi x_image.texi x_log.texi \
			x_menus.texi x_pagesel.texi                           \
			x_preview.texi x_print.texi x_script.texi             \
			x_stroke.texi x_window.texi x_attribedit.texi         \
			x_multiattrib.texi
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

%.html: %.1
	@echo "Creating html manpage (using groff)"
	@rm -rf $*.tmp
	@if which groff >/dev/null 2>/dev/null; then             \
		LC_NUMERIC=C groff -man -T html  $*.1 >$*.tmp   ;\
	else                                                     \
		echo "** Did not find groff **"                 ;\
		echo "Some Documentation might not be correct!" ;\
                touch $*.html                                   ;\
	fi
	@mv -f $*.tmp $*.html

gschem.html: gschem.1


MOSTLYCLEANFILES = 	*.log *~ gschem.html
CLEANFILES = 		*.log *~ gschem.html
DISTCLEANFILES = 	*.log core FILE *~ prototype.bak           \
			*.html *.dvi *.info *.info-* *.aux *.fn    \
			*.fns *.cp *.ky *.pg *.toc *.tp *.vrs *.vr \
			$(gschemdoc_TEXINFOS)
MAINTAINERCLEANFILES = 	*.log *~ Makefile.in configure *.html
