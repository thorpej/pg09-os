ROM_IMAGES=	pg09-from.bin

ASMDEPS=	../../pg09-system/asm/pg09_defs.s			\
		../../pg09-system/asm/w65c21_defs.s			\
		../../pg09-system/asm/w65c22_defs.s			\
		../lib/memzero.s

FROM_DEPS=	$(ASMDEPS) pg09-from.s

all: $(ROM_IMAGES)

pg09-from.bin: $(FROM_DEPS)
	asm6809 -l pg09-from.lst -o $@ pg09-from.s

clean:
	-rm -f $(ROM_IMAGES) *.lst *.tmp
