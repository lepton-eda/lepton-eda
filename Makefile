
SUBDIRS=libgeda symbols gschem

prefix=${HOME}/geda

all: build

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
	(cd $$i ; autoreconf --force ; automake ; ./configure --prefix=$(prefix)); \
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

