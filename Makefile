
# Thanks to Kazu Hirata for suggestion some comments which would make this
# Makefile a bit more "user friendly"

#SUBDIRS=libgeda symbols gschem 
#SUBDIRS=libgeda symbols gschem gnetlist gpcb
SUBDIRS=libgeda symbols gschem gpcb gnetlist gsymcheck utils

prefix=${HOME}/geda

all: targets

install::
	for i in $(SUBDIRS) ;\
	do \
	echo "configuring/making/installing" all "in $$i..."; \
	( \
		LD_LIBRARY_PATH=$(prefix)/lib:${LD_LIBRARY_PATH} ; \
		export LD_LIBRARY_PATH ; \
		PATH=$(prefix)/bin:${PATH} ; \
		export PATH ; \
		cd $$i ; \
		 ./configure --prefix=$(prefix); \
		make install \
	); \
	done
	@echo Finished!
	@echo "If you ran make install: than be sure to set"
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

config::
	for i in $(SUBDIRS) ;\
	do \
	echo "configuring" all "in $$i..."; \
	( \
		LD_LIBRARY_PATH=$(prefix)/lib:${LD_LIBRARY_PATH} ; \
		export LD_LIBRARY_PATH ; \
		PATH=$(prefix)/bin:${PATH} ; \
		export PATH ; \
		cd $$i ; \
		./configure --prefix=$(prefix) \
	); \
	done

clean::	
	for i in $(SUBDIRS) ;\
	do \
	echo "cleaning" all "in $$i..."; \
	(cd $$i ; make clean); \
	done
	rm -f *~ *.log
	@echo Finished!

uninstall::	
	for i in $(SUBDIRS) ;\
	do \
	echo "uninstalling" all "in $$i... (errors can be ignored)"; \
	(cd $$i ; make uninstall); \
	done
	@echo Finished!


maint::	
	for i in $(SUBDIRS) ;\
	do \
	echo "maint-cleaning" all "in $$i..."; \
	(cd $$i ; make maintainer-clean); \
	done
	rm -f *~ *.log
	@echo Finished!

build::	
	for i in $(SUBDIRS) ;\
	do \
	echo "building" all "in $$i..."; \
	( \
		LD_LIBRARY_PATH=$(prefix)/lib:${LD_LIBRARY_PATH} ; \
		export LD_LIBRARY_PATH ; \
		PATH=$(prefix)/bin:${PATH} ; \
		export PATH ; \
		cd $$i ; \
		make \
	); \
	done
	@echo Finished!

reconfig: 
	for i in $(SUBDIRS) ;\
	do \
	echo "reconfiging" all "in $$i..."; \
	(cd $$i ; autoreconf --force ; automake ); \
	done
	@echo Finished!

proto: 
	for i in $(SUBDIRS) ;\
	do \
	echo "Makeing prototype in $$i..."; \
	(cd $$i ; make proto ); \
	done
	@echo Finished!


distconfig: 
	for i in $(SUBDIRS) ;\
	do \
	echo "distconfiging" all "in $$i..."; \
	(cd $$i ; autoreconf --force ; automake --include-deps); \
	done
	@echo Finished!

justinstall::
	for i in $(SUBDIRS) ;\
	do \
	echo "justinstalling" all "in $$i..."; \
	( \
		LD_LIBRARY_PATH=$(prefix)/lib:${LD_LIBRARY_PATH} ; \
		export LD_LIBRARY_PATH ; \
		PATH=$(prefix)/bin:${PATH} ; \
		export PATH ; \
		cd $$i ; \
		make install \
	); \
	done
	@echo Finished!

targets::
	@echo ""
	@echo Type:
	@echo ""
	@echo "make install      Installs into $(prefix) directory"
	@echo "make config       Just do the ./configure --prefix=${prefix}"
	@echo "make build        Just builds, doesn't install"
	@echo "make clean        Simple clean only"
	@echo "make maint        Total maintenance clean"
	@echo "make reconfig     Create ./configure (recreate configure)"
	@echo "make proto        Recreate all prototype.h files"
	@echo "make distconfig   Create dist ./configure (recreate configure)"
	@echo "make justinstall  Just install, no building \(if not needed\)"
	@echo "make uninstall    Install everything from $(prefix)"
	@echo ""
	@echo ""
	@echo "Note that, before doing anything, be sure to set"
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

