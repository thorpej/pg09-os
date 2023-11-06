ROM_IMAGE=	pg09-from.bin

CONFIGFLAGS+=	-d CONFIG_DISPLAY_TMS9918A=1
CONFIGFLAGS+=	-d CONFIG_NHACP=1
CONFIGFLAGS+=	-d CONFIG_NHACP_W65C51=1

all: $(ROM_IMAGE)

.PHONY: ../fixed-ram/pg09-fram.exp
../fixed-ram/pg09-fram.exp:
	cd ../fixed-ram && $(MAKE)

.PHONY: build_date.s
build_date.s:
	echo "build_date fcn \"`date`\"" > build_date.s

.PHONY: sys-api
sys-api:
	sh ./gen-sysapi.sh pg09-from.sym ../sys-api/pg09-os.exp

.PHONY: pg09-from.bin
pg09-from.bin:
	asm6809 $(CONFIGFLAGS) -l pg09-from.lst -s pg09-from.sym \
	    -E pg09-from.exp -o $@ pg09-from.s
	$(MAKE) sys-api

clean_fram:
	cd ../fixed-ram && $(MAKE) clean

clean: clean_fram
	-rm -f $(ROM_IMAGE) *.lst *.sym *.tmp build_date.s
