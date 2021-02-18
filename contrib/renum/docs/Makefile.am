dist_noinst_MANS = \
	lepton-renum.1

EXTRA_DIST = \
	lepton-renum.1.in \
	README.lepton-renum \
	TODO.lepton-renum \
	why.lepton-renum

.1.in.1:
	d=`$(GUILE) -c '(setlocale LC_ALL "C") \
	  (display (strftime "%B %-d, %Y" \
	  (car (strptime "%Y%m%d" (cadr (program-arguments))))))' $(DATE_VERSION)` ; \
	v="$(DOTTED_VERSION).$(DATE_VERSION)" ; \
	y=`echo $(DATE_VERSION)| sed -e"s/\(....\).*/\1/"`; \
	sed -e"s:[@]DATE@:$$d:" -e"s:[@]VERSION@:$$v:" -e"s:[@]YEAR@:$$y:" < $< > $@

SUFFIXES = .1.in .1

CLEANFILES = $(dist_noinst_MANS)
