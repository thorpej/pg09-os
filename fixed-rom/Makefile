ROM_IMAGES=	pg09-from.bin

ASMDEPS=	../../pg09-system/asm/pg09_defs.s			\
		../fixed-ram/pg09-fram.exp				\
		../drivers/cons/cons.s					\
		../drivers/mc6809/mc6809_defs.s				\
		../drivers/w65c21/w65c21_defs.s				\
		../drivers/w65c22/w65c22_defs.s				\
		../lib/memzero.s					\
		../lib/puts.s

FROM_DEPS=	$(ASMDEPS) pg09-from.s

all: $(ROM_IMAGES)

.PHONY: ../fixed-ram/pg09-fram.exp
../fixed-ram/pg09-fram.exp:
	cd ../fixed-ram && $(MAKE)

pg09-from.bin: $(FROM_DEPS)
	asm6809 -l pg09-from.lst -o $@ pg09-from.s

clean:
	-rm -f $(ROM_IMAGES) *.lst *.tmp
