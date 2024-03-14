ROM_BANK=	1
ROM_IMAGE=	pg09-brom$(ROM_BANK).bin
ROM_EXPORTS=	pg09-brom$(ROM_BANK).exp
ROM_IMAGE_DEPS=	../fixed-ram/pg09-fram.exp $(ROM_EXPORTS) \
		pg09-brom$(ROM_BANK)-abi.s

all: $(ROM_IMAGE)

exports: $(ROM_EXPORTS)

.PHONY: ../fixed-ram/pg09-fram.exp
../fixed-ram/pg09-fram.exp:
	cd ../fixed-ram && $(MAKE)

$(ROM_EXPORTS): pg09-brom$(ROM_BANK)-abi.s
	asm6809 -d __GENERATE_EXPORTS=1 \
	    -s pg09-brom$(ROM_BANK).sym pg09-brom$(ROM_BANK)-abi.s
	sh ../lib/gen-bcalls.sh $(ROM_BANK) pg09-brom$(ROM_BANK).sym $@

.PHONY: $(ROM_IMAGE)
$(ROM_IMAGE): $(ROM_IMAGE_DEPS)
	asm6809 -l pg09-brom$(ROM_BANK).lst -o $@ pg09-brom$(ROM_BANK).s

clean_fram:
	cd ../fixed-ram && $(MAKE) clean

clean: clean_fram
	-rm -f $(ROM_IMAGE) *.lst *.sym *.tmp
