## Process this file with automake to produce Makefile.in

bin_SCRIPTS = lepton-symcheck
EXTRA_DIST = lepton-symcheck.in

do_subst = sed -e 's,[@]libdir[@],$(libdir),g'

lepton-symcheck: lepton-symcheck.in Makefile
	$(do_subst) < $< > $@
	chmod +x $@

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
CLEANFILES = *.log *.ps core FILE *~ $(bin_SCRIPTS)
DISTCLEANFILES = *.log core FILE *~
MAINTAINERCLEANFILES = *.log *.ps core FILE *~ Makefile.in
