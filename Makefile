

SHELL=/bin/sh

prefix=${HOME}/geda
opts=

############################################################################
# Basic targets
############################################################################

info:
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
	@echo "${MAKE} reconfig     Recreate ./configure (and all Makefiles)"
	@echo "${MAKE} distconfig   Create dist ./configure (and all Makefiles)"
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

uninstall: utils_install gsymcheck_uninstall gnetlist_uninstall \
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

# This recreates all ./configure scripts and Makefile.in files
proto: libgeda_proto gschem_proto gnetlist_proto gsymcheck_proto

############################################################################
# Midlevel targets 
############################################################################

# Symbols 
symbols: symbols/gesym-config
	@echo symbols Installed 

symbols_maint: symbols/system-commonrc
	( cd symbols; ${MAKE} maintainer-clean )

symbols_clean: symbols/system-commonrc
	( cd symbols; ${MAKE} clean )

symbols_install: symbols/gesym-config
	( cd symbols; ${MAKE} install )

symbols_uninstall: symbols/system-commonrc
	( cd symbols; ${MAKE} uninstall )

symbols_config: 
	( cd symbols; ./configure --prefix=$(prefix) $(opts) )

symbols_reconfig: 
	( cd symbols; autoreconf --force ; automake )

symbols_distconfig: 
	( cd symbols; autoreconf --force ; automake --include-deps )

# gschem
gschem: gschem/config.h gschem/src/gschem
	@echo gschem Built 

gschem_install: libgeda_install symbols_install \
		gschem/config.h gschem/src/gschem
	( cd gschem; ${MAKE} install )

gschem_uninstall: gschem/config.h 
	( cd gschem; ${MAKE} uninstall )

gschem_config: 
	( cd gschem; ./configure --prefix=$(prefix) $(opts) )

gschem_reconfig: 
	( cd gschem; autoreconf --force ; automake )

gschem_distconfig: 
	( cd gschem; autoreconf --force ; automake --include-deps )

gschem_maint: gschem/config.h 
	( cd gschem; ${MAKE} maintainer-clean )

gschem_clean: gschem/config.h 
	( cd gschem; ${MAKE} clean )

gschem_proto: gschem/config.h 
	( cd gschem; ${MAKE} proto )

# gnetlist
gnetlist: gnetlist/config.h gnetlist/src/gnetlist
	@echo gnetlist Built

gnetlist_install: libgeda_install symbols_install \
		  gnetlist/config.h gnetlist/src/gnetlist
	( cd gnetlist; ${MAKE} install )

gnetlist_uninstall: gnetlist/config.h 
	( cd gnetlist; ${MAKE} uninstall )

gnetlist_maint: gnetlist/config.h 
	( cd gnetlist; ${MAKE} maintainer-clean )

gnetlist_clean: gnetlist/config.h 
	( cd gnetlist; ${MAKE} clean )

gnetlist_proto: gnetlist/config.h 
	( cd gnetlist; ${MAKE} proto )

gnetlist_config: 
	( cd gnetlist; ./configure --prefix=$(prefix) $(opts) )

gnetlist_reconfig: 
	( cd gnetlist; autoreconf --force ; automake )

gnetlist_distconfig: 
	( cd gnetlist; autoreconf --force ; automake --include-deps )

# gsymcheck
gsymcheck: gsymcheck/config.h gsymcheck/src/gsymcheck
	@echo gsymcheck Built 

gsymcheck_install: libgeda_install symbols_install \
		gsymcheck/config.h gsymcheck/src/gsymcheck
	( cd gsymcheck; ${MAKE} install )

gsymcheck_uninstall: gsymcheck/config.h
	( cd gsymcheck; ${MAKE} uninstall )

gsymcheck_config: 
	( cd gsymcheck; ./configure --prefix=$(prefix) $(opts) )

gsymcheck_reconfig: 
	( cd gsymcheck; autoreconf --force ; automake )

gsymcheck_distconfig: 
	( cd gsymcheck; autoreconf --force ; automake --include-deps )

gsymcheck_maint: gsymcheck/config.h 
	( cd gsymcheck; ${MAKE} maintainer-clean )

gsymcheck_clean: gsymcheck/config.h 
	( cd gsymcheck; ${MAKE} clean )

gsymcheck_proto: gsymcheck/config.h 
	( cd gsymcheck; ${MAKE} proto )

# utils
utils: utils/config.h utils/src/gmk_sym
	@echo utils Built 

utils_install: libgeda_install symbols_install \
	       utils/config.h utils/src/gmk_sym
	( cd utils; ${MAKE} install )

utils_uninstall: utils/config.h 
	( cd utils; ${MAKE} uninstall )

utils_config: 
	( cd utils; ./configure --prefix=$(prefix) $(opts) )

utils_reconfig: 
	( cd utils; autoreconf --force ; automake )

utils_distconfig: 
	( cd utils; autoreconf --force ; automake --include-deps )

utils_maint: utils/config.h 
	( cd utils; ${MAKE} maintainer-clean )

utils_clean: utils/config.h 
	( cd utils; ${MAKE} clean )

# libgeda
libgeda: libgeda/config.h libgeda/src/.libs/libgeda.a
	@echo libgeda Installed 

libgeda_maint: libgeda/config.h
	( cd libgeda; ${MAKE} maintainer-clean )

libgeda_clean: libgeda/config.h
	( cd libgeda; ${MAKE} clean )

libgeda_proto: libgeda/config.h
	( cd libgeda; ${MAKE} proto )

libgeda_config: 
	( cd libgeda; ./configure --prefix=$(prefix) $(opts) )

libgeda_reconfig: 
	( cd libgeda; autoreconf --force ; automake )

libgeda_distconfig: 
	( cd libgeda; autoreconf --force ; automake --include-deps )

libgeda_uninstall: libgeda/config.h 
	( cd libgeda; ${MAKE} uninstall )

libgeda_install: libgeda/config.h 
	( cd libgeda; ${MAKE} install )

############################################################################
# Configure related targets 
############################################################################

libgeda/config.h:
	( cd libgeda; ./configure --prefix=$(prefix) $(opts) )

gschem/config.h:
	( cd gschem; ./configure --prefix=$(prefix) $(opts) )

gsymcheck/config.h:
	( cd gsymcheck; ./configure --prefix=$(prefix) $(opts) )

utils/config.h:
	( cd utils; ./configure --prefix=$(prefix) $(opts) )

gnetlist/config.h:
	( cd gnetlist; ./configure --prefix=$(prefix) $(opts) )

symbols/system-commonrc:
	( cd symbols; ./configure --prefix=$(prefix) $(opts) )

############################################################################
# Executable related related targets 
############################################################################

gschem/src/gschem:
	(cd gschem; ${MAKE} )

gsymcheck/src/gsymcheck:
	(cd gsymcheck; ${MAKE} )

gnetlist/src/gnetlist:
	(cd gnetlist; ${MAKE} )

utils/src/gmk_sym:
	(cd utils; ${MAKE} )

libgeda/src/.libs/libgeda.a:
	( cd libgeda; ${MAKE} install )

symbols/gesym-config:
	( cd symbols; ./configure --prefix=$(prefix) $(opts) ; ${MAKE} install )

