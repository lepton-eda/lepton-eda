## $Id$
##

docname=wiki
docsdir = @GEDADOCDIR@/$(docname)

EXTRA_DIST = @datawiki@

install-data-hook: 
	( $(INSTALL) -d $(DESTDIR)$(docsdir); \
          for i in $(EXTRA_DIST); do \
                base=`dirname $$i`; \
		if [ "$$base" != "." ]; then \
			$(INSTALL) -d $(DESTDIR)$(docsdir)/$$base; \
		fi; \
                type=`file $$i | grep -i directory`; \
		if [ "$$type" = "" ]; then \
			$(INSTALL_DATA) $$i $(DESTDIR)$(docsdir)/$$i; \
		fi; \
	  done )

uninstall-hook:
	( for i in $(EXTRA_DIST); do \
		rm -f $(DESTDIR)$(docsdir)/$$i; \
	  done; )
	  -rmdir $(DESTDIR)$(docsdir) 

clean-local:
	rm -f *~ 

maintainer-clean-local:
	rm -f *~ 

dist-clean-local:
	rm -f *~ 

