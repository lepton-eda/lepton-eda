
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
	(cd $$i ; autoreconf --force ; automake); \
	done
	@echo Finished!

