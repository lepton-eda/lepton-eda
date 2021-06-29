bin_SCRIPTS = lepton-attrib

if CYGWIN
LIBLEPTONATTRIB=cygleptonattrib-$(LIBLEPTONATTRIB_MAJOR)
if ENABLE_GTK3
LIBGTK = cyggtk-3-0
else
LIBGTK = cyggtk-x11-2.0-0
endif
else
LIBLEPTONATTRIB=libleptonattrib
if ENABLE_GTK3
LIBGTK = libgtk-3
else
LIBGTK = libgtk-x11-2.0
endif
endif

lepton-attrib: lepton-attrib.scm Makefile
	$(do_subst) < $(srcdir)/$@.scm > $@
	chmod +x $@

do_subst = sed \
	-e 's,[@]libdir[@],$(libdir),g' \
	-e 's,[@]GUILE[@],$(GUILE),g' \
	-e 's,[@]LEPTON_SCHEME_MODULE_DIRECTORY[@],$(LEPTON_SCHEME_MODULE_DIRECTORY),g' \
	-e 's,[@]LIBGTK[@],$(LIBGTK),g' \
	-e 's,[@]LIBLEPTONATTRIB[@],$(LIBLEPTONATTRIB),g' \
	-e 's,[@]localedir[@],$(localedir),g' \
	-e 's,[@]ccachedir[@],@LEPTON_SCM_PRECOMPILE_DIR@,g'

EXTRA_DIST = \
	lepton-attrib.scm

CLEANFILES = $(bin_SCRIPTS)
