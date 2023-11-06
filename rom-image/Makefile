ROM_IMAGE=	pg09-rom.bin

all: $(ROM_IMAGE)

.PHONY: pg09-rom.bin
pg09-rom.bin:
	cat ../fixed-rom/pg09-from.bin \
	    ../banked-rom1/pg09-brom1.bin \
	    > pg09-rom.bin

clean:
	-rm -f $(ROM_IMAGE)
