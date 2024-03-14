SUBDIRS=	banked-rom1 banked-rom2 \
		fixed-rom \
		rom-image

all:
	@for dir in $(SUBDIRS); do			\
		(cd $$dir && $(MAKE) all);		\
	done

clean:
	@for dir in $(SUBDIRS); do			\
		(cd $$dir && $(MAKE) clean);		\
	done
