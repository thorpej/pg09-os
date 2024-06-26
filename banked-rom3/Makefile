ROM_BANK=	3
ROM_IMAGE=	pg09-brom$(ROM_BANK).bin
ROM_EXPORTS=	pg09-brom$(ROM_BANK).exp
ROM_IMAGE_DEPS=	../fixed-ram/pg09-fram.exp pg09-brom$(ROM_BANK).exp \
		pg09-brom$(ROM_BANK)-abi.s

all: $(ROM_IMAGE)

exports: $(ROM_EXPORTS)

.PHONY: ../fixed-ram/pg09-fram.exp
../fixed-ram/pg09-fram.exp:
	cd ../fixed-ram && $(MAKE)

$(ROM_EXPORTS): pg09-brom$(ROM_BANK)-abi.s
	asm6809 -d __GENERATE_EXPORTS=1 -d MC6839_BANK=$(ROM_BANK) \
	    -E $@ pg09-brom$(ROM_BANK)-abi.s

.PHONY: $(ROM_IMAGE)
$(ROM_IMAGE): $(ROM_IMAGE_DEPS)
	asm6809 -l pg09-brom$(ROM_BANK).lst -o $@ pg09-brom$(ROM_BANK).s

clean_fram:
	cd ../fixed-ram && $(MAKE) clean

clean: clean_fram
	-rm -f $(ROM_IMAGE) *.lst *.sym *.tmp
