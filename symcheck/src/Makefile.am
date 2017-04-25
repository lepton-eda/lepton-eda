## Process this file with automake to produce Makefile.in

bin_PROGRAMS = lepton-symcheck

# don't forget all *.h files */
lepton_symcheck_SOURCES = lepton-symcheck.c

lepton_symcheck_CPPFLAGS = -I$(top_srcdir)/liblepton/include -I$(srcdir)/../include \
	-I$(top_srcdir) -I$(includedir)
lepton_symcheck_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS) $(GLIB_CFLAGS) \
	$(GUILE_CFLAGS) $(GDK_PIXBUF_CFLAGS)
lepton_symcheck_LDFLAGS = $(GLIB_LIBS) $(GUILE_LIBS) $(GDK_PIXBUF_LIBS)
lepton_symcheck_LDADD = $(top_builddir)/liblepton/src/liblepton.la

if INSTALL_COMPAT_SYMLINKS
NAME = lepton-symcheck$(EXEEXT)
COMPAT_NAME = gsymcheck$(EXEEXT)
install-exec-hook:
	cd $(DESTDIR)$(bindir) && \
	if [ -h $(COMPAT_NAME) ]; then rm -f $(COMPAT_NAME); fi && \
	ln -s $(NAME) $(COMPAT_NAME)
uninstall-hook:
	rm -f $(DESTDIR)$(bindir)/$(COMPAT_NAME)
endif INSTALL_COMPAT_SYMLINKS

MOSTLYCLEANFILES = *.log *.ps core FILE *~
CLEANFILES = *.log *.ps core FILE *~
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in
