
#SUBDIRS=libgeda symbols gschem 
#SUBDIRS=libgeda symbols gschem gnetlist gpcb
SUBDIRS=libgeda symbols gschem gnetlist 

prefix=${HOME}/geda

all: targets

install::
	for i in $(SUBDIRS) ;\
	do \
	echo "making" all "in $$i..."; \
	(cd $$i ; ./configure --prefix=$(prefix); make install); \
	done
	@echo Finished!
	@echo "If you ran make install: than be sure to set"
	@echo ""
	@echo For bourne shell:
	@echo export LD_LIBRARY_PATH=${prefix}:\$$LD_LIBRARY_PATH
	@echo export PATH=${prefix}:\$$\{PATH\}
	@echo ""
	@echo For csh/tcsh shell:
	@echo setenv LD_LIBRARY_PATH ${prefix}:\$$LD_LIBRARY_PATH
	@echo setenv PATH ${prefix}:\$$PATH
	@echo ""

clean::	
	for i in $(SUBDIRS) ;\
	do \
	echo "making" all "in $$i..."; \
	(cd $$i ; make clean); \
	done
	rm -f *~ *.log
	@echo Finished!

uninstall::	
	for i in $(SUBDIRS) ;\
	do \
	echo "making" all "in $$i..."; \
	(cd $$i ; make uninstall); \
	done
	@echo Finished!


maint::	
	for i in $(SUBDIRS) ;\
	do \
	echo "making" all "in $$i..."; \
	(cd $$i ; make maintainer-clean); \
	done
	rm -f *~ *.log
	@echo Finished!

build::	
	for i in $(SUBDIRS) ;\
	do \
	echo "making" all "in $$i..."; \
	(cd $$i ; make); \
	done
	@echo Finished!

config: 
	for i in $(SUBDIRS) ;\
	do \
	echo "making" all "in $$i..."; \
	(cd $$i ; autoreconf --force ; automake ); \
	done
	@echo Finished!

distconfig: 
	for i in $(SUBDIRS) ;\
	do \
	echo "making" all "in $$i..."; \
	(cd $$i ; autoreconf --force ; automake --include-deps; ./configure --prefix=$(prefix)); \
	done
	@echo Finished!

justinstall::
	for i in $(SUBDIRS) ;\
	do \
	echo "making" all "in $$i..."; \
	(cd $$i ; make install); \
	done
	@echo Finished!

targets::
	@echo ""
	@echo Type:
	@echo ""
	@echo "make install      Installs into $(prefix) directory"
	@echo "make build        Just builds, doesn't install"
	@echo "make clean        Simple clean only"
	@echo "make maint        Total maintenance clean"
	@echo "make config       ./configure --prefix=$(prefix) (recreate conf)"
	@echo "make distconfig   Distribution ./configure (recreate conf)"
	@echo "make justinstall  Just install, no building \(if not needed\)"
	@echo "make uninstall    Install everything from $(prefix)"
	@echo ""
