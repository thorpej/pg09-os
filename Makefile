SUBDIRS=	fixed-ram \
		fixed-rom \
		banked-rom1 banked-rom2 banked-rom3 \
		rom-image

all: exports
	@for dir in $(SUBDIRS); do			\
		echo "===> ALL in $$dir";		\
		(cd $$dir && $(MAKE) all);		\
	done

exports:
	@for dir in $(SUBDIRS); do			\
		echo "===> EXPORTS in $$dir";		\
		(cd $$dir && $(MAKE) exports);		\
	done

clean:
	@for dir in $(SUBDIRS); do			\
		echo "===> CLEAN in $$dir";		\
		(cd $$dir && $(MAKE) clean);		\
	done
