ROM_IMAGES=	pg09-brom0.bin

ASMDEPS=	../../pg09-system/asm/pg09_defs.s			\
		../fixed-ram/pg09-fram.exp				\
		../drivers/cons/cons.s					\
		../drivers/mc6809/mc6809_defs.s				\
		../drivers/w65c21/w65c21_defs.s				\
		../drivers/w65c22/w65c22_defs.s				\
		../lib/memzero16.s					\
		../lib/puts.s

BROM0_DEPS=	$(ASMDEPS) pg09-brom0.s

all: $(ROM_IMAGES)

.PHONY: ../fixed-ram/pg09-fram.exp
../fixed-ram/pg09-fram.exp:
	cd ../fixed-ram && $(MAKE)

pg09-brom0.bin: $(BROM0_DEPS)
	asm6809 -l pg09-brom0.lst -s pg09-brom0.sym -o $@ pg09-brom0.s
	sh ../lib/gen-bcalls.sh 0 pg09-brom0.sym pg09-brom0.exp

clean_fram:
	cd ../fixed-ram && $(MAKE) clean

clean: clean_fram
	-rm -f $(ROM_IMAGES) *.lst *.sym *.tmp
