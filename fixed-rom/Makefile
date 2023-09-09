ROM_IMAGES=	pg09-from.bin

ASMDEPS=	../../pg09-system/asm/pg09_defs.s			\
		../fixed-ram/pg09-fram.exp				\
		../drivers/cons/cons.s					\
		../drivers/mc6809/mc6809_defs.s				\
		../drivers/w65c21/w65c21_defs.s				\
		../drivers/w65c22/w65c22_defs.s				\
		../lib/memzero16.s					\
		../lib/puts.s

FROM_DEPS=	$(ASMDEPS) pg09-from.s

CONFIGFLAGS+=	-d CONFIG_DISPLAY_TMS9918A=1

all: $(ROM_IMAGES)

.PHONY: ../fixed-ram/pg09-fram.exp
../fixed-ram/pg09-fram.exp:
	cd ../fixed-ram && $(MAKE)

.PHONY: build_date.s
build_date.s:
	echo "build_date fcn \"`date`\"" > build_date.s

.PHONY: sys-api
sys-api:
	sh ./gen-sysapi.sh pg09-from.sym ../sys-api/pg09-os.exp

pg09-from.bin: Makefile build_date.s $(FROM_DEPS)
	asm6809 $(CONFIGFLAGS) -l pg09-from.lst -s pg09-from.sym \
	    -o $@ pg09-from.s
	$(MAKE) sys-api

clean_fram:
	cd ../fixed-ram && $(MAKE) clean

clean: clean_fram
	-rm -f $(ROM_IMAGES) *.lst *.sym *.tmp build_date.s
