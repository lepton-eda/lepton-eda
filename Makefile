

SHELL=/bin/sh

#
# Change this to the real version of the distribution
#
VERSION=20020825

# 
# Use this when you are building the CVS version
CD_VERSION=
DIR_PREFIX=

#
# Use this when you are building the Released version (comment out above)
#CD_VERSION=-$(VERSION)
#DIR_PREFIX=geda-


prefix=${HOME}/geda

#
# You can set any global options you want here to be passed to the 
# individual ./configure scripts
#
opts=

############################################################################
# Basic targets
############################################################################

notarget:
	@echo $(VERSION) $(CD_VERSION) $(DIR_PREFIX)
	@echo "gEDA Toplevel Makefile"
	@echo "This Makefile requires libgeda, symbols, gschem, gnetlist,"
	@echo "gsymcheck, utils, docs source tarball packages untarred in this:"
	@echo "`pwd` directory"
	@echo ""
	@echo Type:
	@echo ""
	@echo "${MAKE} install      Installs gEDA into $(prefix)"
	@echo "${MAKE} uninstall    Uninstall everything from $(prefix)"
	@echo "${MAKE} clean        Simple clean only (remove all .o and bins)"
	@echo ""
	@echo "These targets are primarily used by the developers:"
	@echo "${MAKE} reconfig     Recreate ./configure (and all Makefiles)"
	@echo "${MAKE} maint        Total maintenance clean"
	@echo "${MAKE} distcheck    Create distribution (using make distcheck)"
	@echo "${MAKE} distconfig   Create dist ./configure (and all Makefiles)"
	@echo "${MAKE} distclean    Does a make distclean in each subdir"
	@echo "${MAKE} config       Just do the ./configure --prefix=${prefix}"
	@echo "${MAKE} proto        Recreate all prototype.h files"
	@echo "${MAKE} dist         Create distribution (using make dist)"
	@echo "${MAKE} src          Create all *.c files from *.nw files"
	@echo "${MAKE} all          Just build. Do not use! Run make install"
	@echo ""
	@echo ""
	@echo "---------------------------READ THIS---------------------------"
	@echo "Before doing anything set these shell variables!"
	@echo "The build will fail if you do not set these variables!"
	@echo "You will also need to set these variables to run the programs."
	@echo "gEDA programs are going to be installed into ${prefix}"
	@echo ""
	@echo For bourne shell:
	@echo LD_LIBRARY_PATH=${prefix}/lib:\$$LD_LIBRARY_PATH
	@echo export LD_LIBRARY_PATH
	@echo PATH=${prefix}/bin:\$$\{PATH\}
	@echo export PATH
	@echo ""
	@echo For bash:
	@echo export LD_LIBRARY_PATH=${prefix}/lib:\$$LD_LIBRARY_PATH
	@echo export PATH=${prefix}/bin:\$$\{PATH\}
	@echo ""
	@echo For csh/tcsh shell:
	@echo setenv LD_LIBRARY_PATH ${prefix}/lib:\$$LD_LIBRARY_PATH
	@echo setenv PATH ${prefix}/bin:\$$PATH
	@echo ""
	@echo "---------------------------READ THIS---------------------------"


# This installs libgeda and symbols and just builds gschem and gnetlist
all: libgeda symbols gschem gnetlist gsymcheck utils docs

# libgeda_install or symbols_install is not needed since the default 
# libgeda does a make install (ditto for symbols)
# This also installs gschem, gnetlist, and gsymcheck 
install: gschem_install gnetlist_install gsymcheck_install utils_install \
	 docs_install

uninstall: utils_uninstall gsymcheck_uninstall gnetlist_uninstall \
           gschem_uninstall symbols_uninstall libgeda_uninstall docs_uninstall

# This does a maintainer-clean removes EVERYTHING that's config/built
maint: libgeda_maint symbols_maint gschem_maint gnetlist_maint \
       gsymcheck_maint utils_maint docs_maint

# This does a plain clean 
clean: libgeda_clean symbols_clean gschem_clean gnetlist_clean \
       gsymcheck_clean utils_clean docs_clean

# This does a plain clean 
distclean: libgeda_distclean symbols_distclean gschem_distclean \
	   gnetlist_distclean gsymcheck_distclean utils_distclean \
	   docs_distclean

# This just does the appropriate ./configure --prefix=...
config: libgeda_config symbols_config gschem_config gnetlist_config \
        gsymcheck_config utils_config docs_config

# This recreates all ./configure scripts and Makefile.in files
reconfig: libgeda_reconfig symbols_reconfig gschem_reconfig gnetlist_reconfig \
          gsymcheck_reconfig utils_reconfig docs_reconfig

# This creates all *.c files from *.nw files
src: libgeda_src gschem_src

# This recreates all ./configure scripts and Makefile.in files ready
# for distribution (removes dependency checking 
distconfig: libgeda_distconfig symbols_distconfig gschem_distconfig \
            gnetlist_distconfig gsymcheck_distconfig utils_distconfig \
	    docs_distconfig

dist: libgeda_dist symbols_dist gschem_dist \
      gnetlist_dist gsymcheck_dist utils_dist docs_dist
	mv -f libgeda/libgeda*.tar.gz .
	mv -f symbols/geda-symbols*.tar.gz .
	mv -f gschem/geda-gschem*.tar.gz .
	mv -f gnetlist/geda-gnetlist*.tar.gz .
	mv -f gsymcheck/geda-gsymcheck*.tar.gz .
	mv -f utils/geda-utils*.tar.gz .
	mv -f docs/geda-docs*.tar.gz .

distcheck: libgeda_distcheck symbols_distcheck gschem_distcheck \
           gnetlist_distcheck gsymcheck_distcheck utils_distcheck \
	   docs_distcheck
	mv -f libgeda/libgeda*.tar.gz .
	mv -f symbols/symbols*.tar.gz .
	mv -f gschem/gschem*.tar.gz .
	mv -f gnetlist/gnetlist*.tar.gz .
	mv -f gsymcheck/gsymcheck*.tar.gz .
	mv -f utils/utils*.tar.gz .
	mv -f docs/docs*.tar.gz .

# This recreates all ./configure scripts and Makefile.in files
proto: libgeda_proto gschem_proto gnetlist_proto gsymcheck_proto

############################################################################
# Midlevel targets 
############################################################################

# Symbols 
symbols: $(DIR_PREFIX)symbols$(CD_VERSION)/gesym-config
	@echo symbols Installed 

symbols_maint: $(DIR_PREFIX)symbols$(CD_VERSION)/system-commonrc
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); ${MAKE} maintainer-clean )

symbols_clean: $(DIR_PREFIX)symbols$(CD_VERSION)/system-commonrc
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); ${MAKE} clean )

symbols_distclean: $(DIR_PREFIX)symbols$(CD_VERSION)/system-commonrc
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); ${MAKE} distclean )

symbols_install: $(DIR_PREFIX)symbols$(CD_VERSION)/gesym-config
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); ${MAKE} install )

symbols_uninstall: $(DIR_PREFIX)symbols$(CD_VERSION)/system-commonrc
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); ${MAKE} uninstall )

symbols_config: 
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

symbols_reconfig: 
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); aclocal ; autoconf ; automake )

symbols_distconfig: 
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); aclocal ; autoconf ; \
	  automake --include-deps )

symbols_dist: 
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); ${MAKE} dist )

symbols_distcheck: 
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); ${MAKE} distcheck )

# gschem
gschem: $(DIR_PREFIX)gschem$(CD_VERSION)/config.h \
	$(DIR_PREFIX)gschem$(CD_VERSION)/src/gschem
	@echo gschem Built 

gschem_install: libgeda_install symbols_install \
		$(DIR_PREFIX)gschem$(CD_VERSION)/config.h \
	        $(DIR_PREFIX)gschem$(CD_VERSION)/src/gschem
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); ${MAKE} install )

gschem_uninstall: $(DIR_PREFIX)gschem$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); ${MAKE} uninstall )

gschem_config: 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

gschem_reconfig: 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); aclocal ; autoconf ; automake )

gschem_src: 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION)/src; ${MAKE} src )

gschem_distconfig: 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); aclocal ; autoconf ; \
	  automake --include-deps )

gschem_dist: 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); ${MAKE} dist )

gschem_distcheck: 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); ${MAKE} distcheck )

gschem_maint: $(DIR_PREFIX)gschem$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); ${MAKE} maintainer-clean )

gschem_clean: $(DIR_PREFIX)gschem$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); ${MAKE} clean )

gschem_distclean: $(DIR_PREFIX)gschem$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); ${MAKE} distclean )

gschem_proto: $(DIR_PREFIX)gschem$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gschem$(CD_VERSION)/src; ${MAKE} proto )

# gnetlist
gnetlist: $(DIR_PREFIX)gnetlist$(CD_VERSION)/config.h \
	  $(DIR_PREFIX)gnetlist$(CD_VERSION)/src/gnetlist
	@echo gnetlist Built

gnetlist_install: libgeda_install symbols_install \
		  $(DIR_PREFIX)gnetlist$(CD_VERSION)/config.h \
	          $(DIR_PREFIX)gnetlist$(CD_VERSION)/src/gnetlist
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); ${MAKE} install )

gnetlist_uninstall: $(DIR_PREFIX)gnetlist$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); ${MAKE} uninstall )

gnetlist_maint: $(DIR_PREFIX)gnetlist$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); ${MAKE} maintainer-clean )

gnetlist_clean: $(DIR_PREFIX)gnetlist$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); ${MAKE} clean )

gnetlist_distclean: $(DIR_PREFIX)gnetlist$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); ${MAKE} distclean )

gnetlist_proto: $(DIR_PREFIX)gnetlist$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION)/src; ${MAKE} proto )

gnetlist_config: 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

gnetlist_reconfig: 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); aclocal ; autoconf ; automake )

gnetlist_distconfig: 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); aclocal ; autoconf ; \
	  automake --include-deps )

gnetlist_dist: 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); ${MAKE} dist )

gnetlist_distcheck: 
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); ${MAKE} distcheck )

# gsymcheck
gsymcheck: $(DIR_PREFIX)gsymcheck$(CD_VERSION)/config.h \
	   $(DIR_PREFIX)gsymcheck$(CD_VERSION)/src/gsymcheck
	@echo gsymcheck Built 

gsymcheck_install: libgeda_install symbols_install \
		$(DIR_PREFIX)gsymcheck$(CD_VERSION)/config.h \
	        $(DIR_PREFIX)gsymcheck$(CD_VERSION)/src/gsymcheck
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); ${MAKE} install )

gsymcheck_uninstall: $(DIR_PREFIX)gsymcheck$(CD_VERSION)/config.h
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); ${MAKE} uninstall )

gsymcheck_config: 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); \
          ./configure --prefix=$(prefix) $(opts) )

gsymcheck_reconfig: 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); aclocal ; autoconf ; automake )

gsymcheck_distconfig: 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); aclocal ; autoconf ; \
	  automake --include-deps )

gsymcheck_dist: 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); ${MAKE} dist )

gsymcheck_distcheck: 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); ${MAKE} distcheck )

gsymcheck_maint: $(DIR_PREFIX)gsymcheck$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); ${MAKE} maintainer-clean )

gsymcheck_clean: $(DIR_PREFIX)gsymcheck$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); ${MAKE} clean )

gsymcheck_distclean: $(DIR_PREFIX)gsymcheck$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); ${MAKE} distclean )

gsymcheck_proto: $(DIR_PREFIX)gsymcheck$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION)/src; ${MAKE} proto )

# utils
utils: $(DIR_PREFIX)utils$(CD_VERSION)/config.h \
       $(DIR_PREFIX)utils$(CD_VERSION)/src/gmk_sym
	@echo utils Built 

utils_install: libgeda_install symbols_install \
	       $(DIR_PREFIX)utils$(CD_VERSION)/config.h \
	       $(DIR_PREFIX)utils$(CD_VERSION)/src/gmk_sym
	( cd $(DIR_PREFIX)utils$(CD_VERSION); ${MAKE} install )

utils_uninstall: $(DIR_PREFIX)utils$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); ${MAKE} uninstall )

utils_config: 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); \
          ./configure --prefix=$(prefix) $(opts) )

utils_reconfig: 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); aclocal ; autoconf ; automake ) 

utils_distconfig: 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); aclocal ; autoconf ; \
	  automake --include-deps )

utils_dist: 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); ${MAKE} dist )

utils_distcheck: 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); ${MAKE} distcheck )

utils_maint: $(DIR_PREFIX)utils$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); ${MAKE} maintainer-clean )

utils_clean: $(DIR_PREFIX)utils$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); ${MAKE} clean )

utils_distclean: $(DIR_PREFIX)utils$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)utils$(CD_VERSION); ${MAKE} distclean )

# libgeda (has no $(DIR_PREFIX)
libgeda: libgeda$(CD_VERSION)/config.h \
	 libgeda$(CD_VERSION)/src/.libs/libgeda.a
	@echo libgeda Installed 

libgeda_maint: libgeda$(CD_VERSION)/config.h
	( cd libgeda$(CD_VERSION); ${MAKE} maintainer-clean )

libgeda_clean: libgeda$(CD_VERSION)/config.h
	( cd libgeda$(CD_VERSION); ${MAKE} clean )

libgeda_distclean: libgeda$(CD_VERSION)/config.h
	( cd libgeda$(CD_VERSION); ${MAKE} distclean )

libgeda_proto: libgeda$(CD_VERSION)/config.h
	( cd libgeda$(CD_VERSION)/src; ${MAKE} proto )

libgeda_config: 
	( cd libgeda$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

libgeda_reconfig: 
	( cd libgeda$(CD_VERSION); aclocal ; autoconf ; automake )

libgeda_src: 
	( cd libgeda$(CD_VERSION)/src; ${MAKE} src )

libgeda_distconfig: 
	( cd libgeda$(CD_VERSION); aclocal ; autoconf ; \
          automake --include-deps )

libgeda_dist: 
	( cd libgeda$(CD_VERSION); ${MAKE} dist )

libgeda_distcheck: 
	( cd libgeda$(CD_VERSION); ${MAKE} distcheck )

libgeda_uninstall: libgeda$(CD_VERSION)/config.h 
	( cd libgeda$(CD_VERSION); ${MAKE} uninstall )

libgeda_install: libgeda$(CD_VERSION)/config.h 
	( cd libgeda$(CD_VERSION); ${MAKE} install )

# docs
docs: $(DIR_PREFIX)docs$(CD_VERSION)/config.h \
       $(DIR_PREFIX)docs$(CD_VERSION)/attributes/attributes.pdf
	@echo docs Built 

docs_install: symbols_install \
	       $(DIR_PREFIX)docs$(CD_VERSION)/config.h \
	       $(DIR_PREFIX)docs$(CD_VERSION)/attributes/attributes.pdf
	( cd $(DIR_PREFIX)docs$(CD_VERSION); ${MAKE} install )

docs_uninstall: $(DIR_PREFIX)docs$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); ${MAKE} uninstall )

docs_config: 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); \
          ./configure --prefix=$(prefix) $(opts) )

docs_reconfig: 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); aclocal ; autoconf ; automake ) 

docs_distconfig: 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); aclocal ; autoconf ; \
	  automake --include-deps )

docs_dist: 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); ${MAKE} dist )

docs_distcheck: 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); ${MAKE} distcheck )

docs_maint: $(DIR_PREFIX)docs$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); ${MAKE} maintainer-clean )

docs_clean: $(DIR_PREFIX)docs$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); ${MAKE} clean )

docs_distclean: $(DIR_PREFIX)docs$(CD_VERSION)/config.h 
	( cd $(DIR_PREFIX)docs$(CD_VERSION); ${MAKE} distclean )

############################################################################
# Configure related targets 
############################################################################

libgeda$(CD_VERSION)/config.h:
	( cd libgeda$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

$(DIR_PREFIX)gschem$(CD_VERSION)/config.h:
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

$(DIR_PREFIX)gsymcheck$(CD_VERSION)/config.h:
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

$(DIR_PREFIX)utils$(CD_VERSION)/config.h:
	( cd $(DIR_PREFIX)utils$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

$(DIR_PREFIX)gnetlist$(CD_VERSION)/config.h:
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

$(DIR_PREFIX)symbols$(CD_VERSION)/system-commonrc:
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

$(DIR_PREFIX)docs$(CD_VERSION)/config.h:
	( cd $(DIR_PREFIX)docs$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) )

############################################################################
# Executable related related targets 
############################################################################

$(DIR_PREFIX)gschem$(CD_VERSION)/src/gschem:
	( cd $(DIR_PREFIX)gschem$(CD_VERSION); ${MAKE} )

$(DIR_PREFIX)gsymcheck$(CD_VERSION)/src/gsymcheck:
	( cd $(DIR_PREFIX)gsymcheck$(CD_VERSION); ${MAKE} )

$(DIR_PREFIX)gnetlist$(CD_VERSION)/src/gnetlist:
	( cd $(DIR_PREFIX)gnetlist$(CD_VERSION); ${MAKE} )

$(DIR_PREFIX)utils$(CD_VERSION)/src/gmk_sym:
	( cd $(DIR_PREFIX)utils$(CD_VERSION); ${MAKE} )

$(DIR_PREFIX)docs$(CD_VERSION)/attributes/attributes.pdf:
	( cd $(DIR_PREFIX)docs$(CD_VERSION); ${MAKE} )

libgeda$(CD_VERSION)/src/.libs/libgeda.a:
	( cd libgeda$(CD_VERSION); ${MAKE} install )

$(DIR_PREFIX)symbols$(CD_VERSION)/gesym-config:
	( cd $(DIR_PREFIX)symbols$(CD_VERSION); \
	  ./configure --prefix=$(prefix) $(opts) ; ${MAKE} install )

