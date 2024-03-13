ROM_BANK=	2
ROM_IMAGE=	pg09-brom$(ROM_BANK).bin

all: $(ROM_IMAGE)

.PHONY: ../fixed-ram/pg09-fram.exp
../fixed-ram/pg09-fram.exp:
	cd ../fixed-ram && $(MAKE)

.PHONY: $(ROM_IMAGE)
$(ROM_IMAGE): ../fixed-ram/pg09-fram.exp
	asm6809 -l pg09-brom$(ROM_BANK).lst \
	    -s pg09-brom$(ROM_BANK).sym -o $@ pg09-brom$(ROM_BANK).s
	sh ../lib/gen-bcalls.sh $(ROM_BANK) \
	    pg09-brom$(ROM_BANK).sym pg09-brom$(ROM_BANK).exp

clean_fram:
	cd ../fixed-ram && $(MAKE) clean

clean: clean_fram
	-rm -f $(ROM_IMAGE) *.lst *.sym *.tmp
