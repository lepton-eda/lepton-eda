## Process this file with automake to produce Makefile.in

bin_PROGRAMS = lepton-sch2pcb lepton-renum

# don't forget all *.h files */
lepton_renum_SOURCES = lepton-renum.c lepton-renum.h

lepton_sch2pcb_SOURCES = lepton-sch2pcb.c
lepton_sch2pcb_CPPFLAGS = \
	-DPCBM4DIR=\"$(PCBM4DIR)\" \
	-DPCBLIBPATH=\"$(PCBLIBPATH)\"
lepton_sch2pcb_CFLAGS = $(GLIB_CFLAGS)
lepton_sch2pcb_LDFLAGS = $(GLIB_LIBS)


if INSTALL_COMPAT_SYMLINKS
NAME = lepton-sch2pcb$(EXEEXT)
COMPAT_NAME = gsch2pcb$(EXEEXT)
install-exec-hook:
	cd $(DESTDIR)$(bindir) && \
	if [ -h $(COMPAT_NAME) ]; then rm -f $(COMPAT_NAME); fi && \
	ln -s $(NAME) $(COMPAT_NAME)
uninstall-hook:
	rm -f $(DESTDIR)$(bindir)/$(COMPAT_NAME)
endif INSTALL_COMPAT_SYMLINKS


AM_CFLAGS = $(GCC_CFLAGS) $(MINGW_CFLAGS)
AM_CPPFLAGS = -I$(top_srcdir)/liblepton/include \
	-I$(top_srcdir) -I$(includedir)

MOSTLYCLEANFILES = *.log *.ps core FILE *~ olib.c
CLEANFILES = *.log *.ps core FILE *~  olib.c
DISTCLEANFILES = *.log core FILE *~ olib.c
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in olib.c
