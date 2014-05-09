# Copyright (C) 2013, 2014 Roland Lutz
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software Foundation,
# Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

noinst_SCRIPTS = xorn
CLEANFILES = xorn xornT
EXTRA_DIST = xorn.in

xorn: $(srcdir)/xorn.in Makefile
	$(AM_V_GEN)sed \
	    -e 's,[@]pythondir[@],$(abs_top_builddir)/built-packages,g' \
	    -e 's,[@]commanddir[@],$(abs_srcdir),g' \
		$(srcdir)/$@.in > $@; \
	chmod +x $@

xornT: $(srcdir)/xorn.in Makefile
	$(AM_V_GEN)sed \
	    -e 's,[@]pythondir[@],$(pythondir),g' \
	    -e 's,[@]commanddir[@],$(commanddir),g' \
		$(srcdir)/xorn.in > $@

install-exec-local: xornT
	@$(NORMAL_INSTALL)
	@test -n "$(bindir)" || exit 0; \
	echo " $(MKDIR_P) '$(DESTDIR)$(bindir)'"; \
	$(MKDIR_P) "$(DESTDIR)$(bindir)" || exit 1; \
	name=`echo "xorn" | sed -e 's,.*/,,;$(transform)'`; \
	echo " $(INSTALL_SCRIPT) xornT '$(DESTDIR)$(bindir)/$$name'"; \
	$(INSTALL_SCRIPT) xornT "$(DESTDIR)$(bindir)/$$name" || exit $$?

uninstall-local:
	@$(NORMAL_UNINSTALL)
	@test -n "$(bindir)" || exit 0; \
	files=`echo "xorn" | sed -e 's,.*/,,;$(transform)'`; \
	dir='$(DESTDIR)$(bindir)'; \
	$(am__uninstall_files_from_dir)

commanddir = $(pkglibdir)
command_PYTHON = \
	extract.py \
	find-symbol-by-pinout.py
