

SHELL=/bin/sh

#
# Change this to the real version of the distribution
#
VERSION=20010304

# 
# Use this when you are building the CVS version
CD_VERSION=

#
# Use this when you are building the Released version (comment out above)
#CD_VERSION=-$(VERSION)


prefix=${HOME}/geda
opts=

############################################################################
# Basic targets
############################################################################

info:
	@echo $(VERSION) $(CD_VERSION)
	@echo "gEDA Toplevel Makefile"
	@echo "This Makefile requires libgeda, symbols, gschem, gnetlist,"
	@echo "gsymcheck, and utils source tarball packages untarred in this:"
	@echo "`pwd` directory"
	@echo ""
	@echo Type:
	@echo ""
	@echo "${MAKE} install      Installs gEDA into $(prefix)"
	@echo "${MAKE} uninstall    Uninstall everything from $(prefix)"
	@echo "${MAKE} clean        Simple clean only (remove all .o and binaries)"
	@echo ""
	@echo "These targets are primarily used by the developers:"
	@echo "${MAKE} config       Just do the ./configure --prefix=${prefix}"
	@echo "${MAKE} maint        Total maintenance clean"
	@echo "${MAKE} dist         Create distribution (using make dist)"
	@echo "${MAKE} distcheck    Create distribution (using make distcheck)"
	@echo "${MAKE} distconfig   Create dist ./configure (and all Makefiles)"
	@echo "${MAKE} reconfig     Recreate ./configure (and all Makefiles)"
	@echo "${MAKE} proto        Recreate all prototype.h files"
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
all: libgeda symbols gschem gnetlist gsymcheck utils

# libgeda_install or symbols_install is not needed since the default 
# libgeda does a make install (ditto for symbols)
# This also installs gschem, gnetlist, and gsymcheck 
install: gschem_install gnetlist_install gsymcheck_install utils_install

uninstall: utils_uninstall gsymcheck_uninstall gnetlist_uninstall \
           gschem_uninstall symbols_uninstall libgeda_uninstall 

# This does a maintainer-clean removes EVERYTHING that's config/built
maint: libgeda_maint symbols_maint gschem_maint gnetlist_maint \
       gsymcheck_maint utils_maint

# This does a plain clean 
clean: libgeda_clean symbols_clean gschem_clean gnetlist_clean \
       gsymcheck_clean utils_clean

# This just does the appropriate ./configure --prefix=...
config: libgeda_config symbols_config gschem_config gnetlist_config \
        gsymcheck_config utils_config

# This recreates all ./configure scripts and Makefile.in files
reconfig: libgeda_reconfig symbols_reconfig gschem_reconfig gnetlist_reconfig \
          gsymcheck_reconfig utils_reconfig

# This recreates all ./configure scripts and Makefile.in files ready
# for distribution (removes dependency checking 
distconfig: libgeda_distconfig symbols_distconfig gschem_distconfig \
            gnetlist_distconfig gsymcheck_distconfig utils_distconfig

dist: libgeda_dist symbols_dist gschem_dist \
      gnetlist_dist gsymcheck_dist utils_dist
	mv -f libgeda/libgeda*.tar.gz .
	mv -f symbols/symbols*.tar.gz .
	mv -f gschem/gschem*.tar.gz .
	mv -f gnetlist/gnetlist*.tar.gz .
	mv -f gsymcheck/gsymcheck*.tar.gz .
	mv -f utils/utils*.tar.gz .

distcheck: libgeda_distcheck symbols_distcheck gschem_distcheck \
           gnetlist_distcheck gsymcheck_distcheck utils_distcheck
	mv -f libgeda/libgeda*.tar.gz .
	mv -f symbols/symbols*.tar.gz .
	mv -f gschem/gschem*.tar.gz .
	mv -f gnetlist/gnetlist*.tar.gz .
	mv -f gsymcheck/gsymcheck*.tar.gz .
	mv -f utils/utils*.tar.gz .

# This recreates all ./configure scripts and Makefile.in files
proto: libgeda_proto gschem_proto gnetlist_proto gsymcheck_proto

############################################################################
# Midlevel targets 
############################################################################

# Symbols 
symbols: symbols$(CD_VERSION)/gesym-config
	@echo symbols Installed 

symbols_maint: symbols$(CD_VERSION)/system-commonrc
	( cd symbols$(CD_VERSION); ${MAKE} maintainer-clean )

symbols_clean: symbols$(CD_VERSION)/system-commonrc
	( cd symbols$(CD_VERSION); ${MAKE} clean )

symbols_install: symbols$(CD_VERSION)/gesym-config
	( cd symbols$(CD_VERSION); ${MAKE} install )

symbols_uninstall: symbols$(CD_VERSION)/system-commonrc
	( cd symbols$(CD_VERSION); ${MAKE} uninstall )

symbols_config: 
	( cd symbols$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

symbols_reconfig: 
	( cd symbols$(CD_VERSION); autoreconf --force ; automake )

symbols_distconfig: 
	( cd symbols$(CD_VERSION); autoreconf --force ; automake --include-deps )

symbols_dist: 
	( cd symbols$(CD_VERSION); ${MAKE} dist )

symbols_distcheck: 
	( cd symbols$(CD_VERSION); ${MAKE} distcheck )

# gschem
gschem: gschem$(CD_VERSION)/config.h gschem$(CD_VERSION)/src/gschem
	@echo gschem Built 

gschem_install: libgeda_install symbols_install \
		gschem$(CD_VERSION)/config.h gschem$(CD_VERSION)/src/gschem
	( cd gschem$(CD_VERSION); ${MAKE} install )

gschem_uninstall: gschem$(CD_VERSION)/config.h 
	( cd gschem$(CD_VERSION); ${MAKE} uninstall )

gschem_config: 
	( cd gschem$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

gschem_reconfig: 
	( cd gschem$(CD_VERSION); autoreconf --force ; automake )

gschem_distconfig: 
	( cd gschem$(CD_VERSION); autoreconf --force ; automake --include-deps )

gschem_dist: 
	( cd gschem$(CD_VERSION); ${MAKE} dist )

gschem_distcheck: 
	( cd gschem$(CD_VERSION); ${MAKE} distcheck )

gschem_maint: gschem/config.h 
	( cd gschem$(CD_VERSION); ${MAKE} maintainer-clean )

gschem_clean: gschem$(CD_VERSION)/config.h 
	( cd gschem$(CD_VERSION); ${MAKE} clean )

gschem_proto: gschem$(CD_VERSION)/config.h 
	( cd gschem$(CD_VERSION); ${MAKE} proto )

# gnetlist
gnetlist: gnetlist$(CD_VERSION)/config.h gnetlist$(CD_VERSION)/src/gnetlist
	@echo gnetlist Built

gnetlist_install: libgeda_install symbols_install \
		  gnetlist$(CD_VERSION)/config.h gnetlist$(CD_VERSION)/src/gnetlist
	( cd gnetlist$(CD_VERSION); ${MAKE} install )

gnetlist_uninstall: gnetlist$(CD_VERSION)/config.h 
	( cd gnetlist$(CD_VERSION); ${MAKE} uninstall )

gnetlist_maint: gnetlist$(CD_VERSION)/config.h 
	( cd gnetlist$(CD_VERSION); ${MAKE} maintainer-clean )

gnetlist_clean: gnetlist$(CD_VERSION)/config.h 
	( cd gnetlist$(CD_VERSION); ${MAKE} clean )

gnetlist_proto: gnetlist$(CD_VERSION)/config.h 
	( cd gnetlist$(CD_VERSION); ${MAKE} proto )

gnetlist_config: 
	( cd gnetlist$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

gnetlist_reconfig: 
	( cd gnetlist$(CD_VERSION); autoreconf --force ; automake )

gnetlist_distconfig: 
	( cd gnetlist$(CD_VERSION); autoreconf --force ; automake --include-deps )

gnetlist_dist: 
	( cd gnetlist$(CD_VERSION); ${MAKE} dist )

gnetlist_distcheck: 
	( cd gnetlist$(CD_VERSION); ${MAKE} distcheck )

# gsymcheck
gsymcheck: gsymcheck$(CD_VERSION)/config.h gsymcheck$(CD_VERSION)/src/gsymcheck
	@echo gsymcheck Built 

gsymcheck_install: libgeda_install symbols_install \
		gsymcheck$(CD_VERSION)/config.h gsymcheck$(CD_VERSION)/src/gsymcheck
	( cd gsymcheck$(CD_VERSION); ${MAKE} install )

gsymcheck_uninstall: gsymcheck$(CD_VERSION)/config.h
	( cd gsymcheck$(CD_VERSION); ${MAKE} uninstall )

gsymcheck_config: 
	( cd gsymcheck$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

gsymcheck_reconfig: 
	( cd gsymcheck$(CD_VERSION); autoreconf --force ; automake )

gsymcheck_distconfig: 
	( cd gsymcheck$(CD_VERSION); autoreconf --force ; automake --include-deps )

gsymcheck_dist: 
	( cd gsymcheck$(CD_VERSION); ${MAKE} dist )

gsymcheck_distcheck: 
	( cd gsymcheck$(CD_VERSION); ${MAKE} distcheck )

gsymcheck_maint: gsymcheck$(CD_VERSION)/config.h 
	( cd gsymcheck$(CD_VERSION); ${MAKE} maintainer-clean )

gsymcheck_clean: gsymcheck$(CD_VERSION)/config.h 
	( cd gsymcheck$(CD_VERSION); ${MAKE} clean )

gsymcheck_proto: gsymcheck$(CD_VERSION)/config.h 
	( cd gsymcheck$(CD_VERSION); ${MAKE} proto )

# utils
utils: utils$(CD_VERSION)/config.h utils$(CD_VERSION)/src/gmk_sym
	@echo utils Built 

utils_install: libgeda_install symbols_install \
	       utils$(CD_VERSION)/config.h utils$(CD_VERSION)/src/gmk_sym
	( cd utils$(CD_VERSION); ${MAKE} install )

utils_uninstall: utils$(CD_VERSION)/config.h 
	( cd utils$(CD_VERSION); ${MAKE} uninstall )

utils_config: 
	( cd utils$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

utils_reconfig: 
	( cd utils$(CD_VERSION); autoreconf --force ; automake )

utils_distconfig: 
	( cd utils$(CD_VERSION); autoreconf --force ; automake --include-deps )

utils_dist: 
	( cd utils$(CD_VERSION); ${MAKE} dist )

utils_distcheck: 
	( cd utils$(CD_VERSION); ${MAKE} distcheck )

utils_maint: utils$(CD_VERSION)/config.h 
	( cd utils$(CD_VERSION); ${MAKE} maintainer-clean )

utils_clean: utils$(CD_VERSION)/config.h 
	( cd utils$(CD_VERSION); ${MAKE} clean )

# libgeda
libgeda: libgeda$(CD_VERSION)/config.h libgeda$(CD_VERSION)/src/.libs/libgeda.a
	@echo libgeda Installed 

libgeda_maint: libgeda$(CD_VERSION)/config.h
	( cd libgeda$(CD_VERSION); ${MAKE} maintainer-clean )

libgeda_clean: libgeda$(CD_VERSION)/config.h
	( cd libgeda$(CD_VERSION); ${MAKE} clean )

libgeda_proto: libgeda$(CD_VERSION)/config.h
	( cd libgeda$(CD_VERSION); ${MAKE} proto )

libgeda_config: 
	( cd libgeda$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

libgeda_reconfig: 
	( cd libgeda$(CD_VERSION); autoreconf --force ; automake )

libgeda_distconfig: 
	( cd libgeda$(CD_VERSION); autoreconf --force ; automake --include-deps )

libgeda_dist: 
	( cd libgeda$(CD_VERSION); ${MAKE} dist )

libgeda_distcheck: 
	( cd libgeda$(CD_VERSION); ${MAKE} distcheck )

libgeda_uninstall: libgeda$(CD_VERSION)/config.h 
	( cd libgeda$(CD_VERSION); ${MAKE} uninstall )

libgeda_install: libgeda$(CD_VERSION)/config.h 
	( cd libgeda$(CD_VERSION); ${MAKE} install )

############################################################################
# Configure related targets 
############################################################################

libgeda$(CD_VERSION)/config.h:
	( cd libgeda$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

gschem$(CD_VERSION)/config.h:
	( cd gschem$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

gsymcheck$(CD_VERSION)/config.h:
	( cd gsymcheck$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

utils$(CD_VERSION)/config.h:
	( cd utils$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

gnetlist$(CD_VERSION)/config.h:
	( cd gnetlist$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

symbols$(CD_VERSION)/system-commonrc:
	( cd symbols$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) )

############################################################################
# Executable related related targets 
############################################################################

gschem$(CD_VERSION)/src/gschem:
	(cd gschem$(CD_VERSION); ${MAKE} )

gsymcheck$(CD_VERSION)/src/gsymcheck:
	(cd gsymcheck$(CD_VERSION); ${MAKE} )

gnetlist$(CD_VERSION)/src/gnetlist:
	(cd gnetlist$(CD_VERSION); ${MAKE} )

utils$(CD_VERSION)/src/gmk_sym:
	(cd utils$(CD_VERSION); ${MAKE} )

libgeda$(CD_VERSION)/src/.libs/libgeda.a:
	( cd libgeda$(CD_VERSION); ${MAKE} install )

symbols$(CD_VERSION)/gesym-config:
	( cd symbols$(CD_VERSION); ./configure --prefix=$(prefix) $(opts) ; ${MAKE} install )

