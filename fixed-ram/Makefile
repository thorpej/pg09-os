EXP_IMAGES=	pg09-fram.exp

ASMDEPS=	../../pg09-system/asm/pg09_defs.s

FRAM_DEPS=	$(ASMDEPS) pg09-fram.s

all: $(EXP_IMAGES)

CONFIGFLAGS+=	-d CONFIG_DISPLAY_TMS9918A=1
CONFIGFLAGS+=	-d CONFIG_NHACP=1
CONFIGFLAGS+=	-d CONFIG_NHACP_W65C51=1

.PHONY: pg09-fram.exp
pg09-fram.exp: $(FRAM_DEPS) Makefile
	asm6809 $(CONFIGFLAGS) -l pg09-fram.lst -E $@ pg09-fram.s
	sort < $@ > $(@).tmp && mv $(@).tmp $@

clean:
	-rm -f $(EXP_IMAGES) *.lst *.tmp
