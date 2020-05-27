rcdatadir = $(LEPTONDATADIR)
rcdata_DATA = system-gschemrc lepton-gtkrc gschem-colormap-lightbg gschem-colormap-darkbg gschem-colormap-bw

system-gschemrc: $(top_builddir)/configure system-gschemrc.scm
	@date_ver=$(DATE_VERSION); \
	sed -e"s:[@]DATE_VERSION@:$$date_ver:" \
	  < $(srcdir)/system-gschemrc.scm > system-gschemrc.new; \
	if diff system-gschemrc system-gschemrc.new > /dev/null 2>&1; then \
	  rm system-gschemrc.new; \
	  echo "system-gschemrc is unchanged"; \
	else \
	  echo "Recreating system-gschemrc"; \
	  mv system-gschemrc.new system-gschemrc; \
	fi

EXTRA_DIST = system-gschemrc.scm lepton-gtkrc gschem-colormap-lightbg gschem-colormap-darkbg gschem-colormap-bw

CLEANFILES = system-gschemrc
