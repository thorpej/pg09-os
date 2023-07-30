EXP_IMAGES=	pg09-fram.exp

ASMDEPS=	../../pg09-system/asm/pg09_defs.s

FRAM_DEPS=	$(ASMDEPS) pg09-fram.s

all: $(EXP_IMAGES)

pg09-fram.exp: $(FRAM_DEPS)
	asm6809 -l pg09-fram.lst -E $@ pg09-fram.s
	sort < $@ > $(@).tmp && mv $(@).tmp $@

clean:
	-rm -f $(EXP_IMAGES) *.lst *.tmp
