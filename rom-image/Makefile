ROM_IMAGE=	pg09-rom.bin

ROM_SEGMENTS=	../fixed-rom/pg09-from.bin \
		../banked-rom1/pg09-brom1.bin \
		../banked-rom2/pg09-brom2.bin \
		../banked-rom3/pg09-brom3.bin

all: $(ROM_IMAGE)

exports:
	@true

.PHONY: pg09-rom.bin
pg09-rom.bin:
	@rm -f $@;						\
	for segment in $(ROM_SEGMENTS); do			\
		if [ ! -f $$segment ]; then			\
			echo "Segment $$segment is missing.";	\
			exit 1;					\
		fi;						\
		size=$$(stat -L -f %z "$$segment");		\
		if [ $$size -gt 8192 ]; then			\
			echo "Segment $$segment is too large.";	\
			rm -f $@;				\
			exit 1;					\
		fi;						\
		echo "Appending $$segment...";			\
		dd if=$$segment obs=8192 conv=osync >> $@	\
		    2>/dev/null;				\
	done;							\
	/bin/ls -l $@

clean:
	-rm -f $(ROM_IMAGE)
