ROM_IMAGE=	pg09-from.bin

CONFIGFLAGS+=	-d CONFIG_TL16C550=1
CONFIGFLAGS+=	-d CONFIG_CONSOLE_TL16C550=1
CONFIGFLAGS+=	-d CONFIG_NHACP_TL16C550=1

#CONFIGFLAGS+=	-d CONFIG_CONSOLE_W65C51=1
#CONFIGFLAGS+=	-d CONFIG_NHACP_W65C51=1

CONFIGFLAGS+=	-d CONFIG_DISPLAY_TMS9918A=1

CONFIGFLAGS+=	-d CONFIG_NHACP=1

all: $(ROM_IMAGE)

exports: sys-api

.PHONY: ../fixed-ram/pg09-fram.exp
../fixed-ram/pg09-fram.exp:
	cd ../fixed-ram && $(MAKE)

.PHONY: build_date.s
build_date.s:
	echo "build_date fcn \"`date`\"" > build_date.s

sys-api: pg09-from-abi-head.s pg09-from-abi-tail.s pg09-from-abi.s
	asm6809 -d __GENERATE_EXPORTS=1 \
	    -s pg09-from.sym pg09-from-abi.s
	sh ./gen-sysapi.sh pg09-from.sym ../sys-api/pg09-os.exp

.PHONY: pg09-from.bin
pg09-from.bin: build_date.s sys-api
	asm6809 $(CONFIGFLAGS) -l pg09-from.lst -o $@ pg09-from.s

clean_fram:
	cd ../fixed-ram && $(MAKE) clean

clean: clean_fram
	-rm -f $(ROM_IMAGE) *.lst *.sym *.tmp build_date.s
