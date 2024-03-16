;
; Copyright (c) 2023 Jason R. Thorpe.
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
; OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
; SUCH DAMAGE.
;

;
; Fixed ROM image for the 6809 Playground.
;

	include "../../pg09-system/asm/pg09_defs.s"
	include "../drivers/mc6809/mc6809_defs.s"
	include "../drivers/w65c21/w65c21_defs.s"
	include "../lib/ascii_defs.s"
	include "../lib/asm_macros.inc"

	include "../fixed-ram/pg09-fram.exp"

	include "../sys-api/pg09-os.exp"
	include "../sys-api/display-api.exp"
	include "../sys-api/file-api.exp"
	include "../sys-api/timer-api.exp"

	include "../banked-rom1/pg09-brom1.exp"
	include "../banked-rom2/pg09-brom2.exp"

	include	"pg09-from-abi-head.s"	; sets origin

	setdp	-1	; Disable automatic direct page addressing

	CODE

	include	"build_date.s"

ram_size_512K
	fcn	"512K"
ram_size_1024K
	fcn	"1024K"
ram_size_1536K
	fcn	"1536K"
ram_size_2048K
	fcn	"2048K"

ram_size_strings
	fdb	ram_size_512K
	fdb	ram_size_1024K
	fdb	ram_size_1536K
	fdb	ram_size_2048K

print_ram_size
	deca				; chip count to table index
	lsla				; table index to offset
	ldx	#ram_size_strings	; X = ram size string table
	ldx	A,X			; X = pointer to string
	jmp	puts			; tail-call to puts

exit					; exported name
warm_boot
	;
	; Disable IRQs until we can reset the timers.
	;
	orcc	#CC_I

	;
	; Switch back to the kernel stack.
	;
	lds	#KSTACK_TOP

	;
	; Invalidate the jump address.
	;
	ldd	#$FFFF
	std	jump_addr
	clr	can_continue

	;
	; Push an empty interrupt frame onto the stack and record it
	; as the current iframe.  Make warm_boot() appear as the
	; return address above the iframe.
	;
	ldx	#MONITOR_IFRAME
	lda	#IFE_SIZE
	jsr	memzero8
	lda	#(CC_E | CC_F)
	sta	IFF_CCR,X

warmer_boot
	;
	; Disable IRQs until we can reset the timers.
	;
	orcc	#CC_I

	ldy	#warm_boot
	sty	IFE_SIZE,X
	stx	current_iframe

	;
	; Re-initialize DP to the reset value.
	;
	clra
	tfr	A,DP

	;
	; Reset all timers.
	;
	jsr	timer_reset

	;
	; Re-initialize the console.
	;
	jsr	cons_reinit

	;
	; Enable IRQs.
	;
	andcc	#~CC_I

	;
	; XXX Here is where we would re-load the shell and return
	; XXX back to it, unless it is the thing that exited.
	;

	jmp	monitor_main		; Go into the main monitor loop!

call_ret
	;
	; Push the current set of registers onto the stack so we can
	; preserve them in the monitor iframe.  Then move those register
	; values into the monitor iframe location.
	;
	; Use LEAS because it does not affect the CC register.  But to
	; do that, we also need to use PC-relative indexed addressing.
	;
	leas	MONITOR_IFRAME+IFE_SIZE,PCR
	pshs	CC,A,B,DP,X,Y,U,PC
	ldx	#MONITOR_IFRAME
	bra	warmer_boot		; go finish warm-booting

probe_ram
	lda	#$55			; store $55 at probe location
	sta	,X
	lda	,X			; load it back
	cmpa	#$55			; same?
	bne	1F			; nope!
	lsla				; store $AA at probe location
	sta	,X
	lda	,X			; load it back
	cmpa	#$AA			; same?
1	rts				; CC_Z is set if there is RAM here!

cold_boot
	;
	; Initialize memory banking.
	;
	; -> Set up all banking ports as outputs.
	; -> Select bank 0 for all banked regions.
	;

	; Ensure DDR is selected by clearing each CR.
	clr	LOW_BANKER_PIA+PIA_REG_CRA
	clr	LOW_BANKER_PIA+PIA_REG_CRB
	clr	HIGH_BANKER_PIA+PIA_REG_CRA
	clr	HIGH_BANKER_PIA+PIA_REG_CRB

	lda	#$FF			; All port pins are outputs
	sta	LOW_BANKER_PIA+PIA_REG_DDRA
	sta	LOW_BANKER_PIA+PIA_REG_DDRB
	sta	HIGH_BANKER_PIA+PIA_REG_DDRA
	sta	HIGH_BANKER_PIA+PIA_REG_DDRB

	; Disable DDR access / enable peripheral interface access.
	lda	#PIA_CRx_DDR_PI
	sta	LOW_BANKER_PIA+PIA_REG_CRA
	sta	LOW_BANKER_PIA+PIA_REG_CRB
	sta	HIGH_BANKER_PIA+PIA_REG_CRA
	sta	HIGH_BANKER_PIA+PIA_REG_CRB

	; Set all banked regions to bank 0.
	clr	LBRAM0_BANK_REG
	clr	LBRAM1_BANK_REG
	clr	HBRAM_BANK_REG
	clr	ROM_BANK_REG

	;
	; Put the stack in LBRAM1 so we can clear FRAM.
	;
	; N.B. this is the first load of the stack pointer, which will
	; unmask NMIs.  We're also assuming that at least 1 low RAM chip
	; is installed.
	;
	lds	#LBRAM1_START+LBRAM1_SIZE

	;
	; Clear out FRAM.
	;
	ldx	#FRAM_START
	ldd	#FRAM_SIZE
	jsr	memzero16

	;
	; Now switch to the kernel stack in FRAM.
	;
	lds	#KSTACK_TOP

	;
	; Probe the amount of low and high banked RAM.
	;
	lda	#1		; assume at least one low RAM chip
	sta	lbram_nchips
1	lda	lbram_nchips
	lsla			; Multiply by 16
	lsla
	lsla
	lsla
	sta	LBRAM0_BANK_REG	; switch low RAM 0 to that bank
	ldx	#0		; probe address 0
	bsr	probe_ram
	bne	1F		; No RAM in this bank.
	inc	lbram_nchips	; count the chip!
	lda	lbram_nchips
	cmpa	#4		; Do we have all 4?
	beq	1F		; Yup!
	bra	1B
1
	clr	LBRAM0_BANK_REG	; reset bank to 0.

	lda	#1		; assume at least one high RAM chip
	sta	hbram_nchips
	lda	#64		; set high RAM bank to 64
	sta	HBRAM_BANK_REG
	ldx	#HBRAM_START	; probe HBRAM address
	jsr	probe_ram
	bne	1F
	inc	hbram_nchips	; if it was there, there are 2 chips.
1
	clr	HBRAM_BANK_REG	; reset bank to 0.

	;
	; Initialize the interrupt controller.
	;
	; -> Set up all enable ports as outputs.
	; -> Set all enable ports to 0.
	; -> Set the IRQ Vector Base register.
	;

	; Ensure DDR is selected by clearing each CR.
	clr	IRQ_ENABLE_PIA+PIA_REG_CRA
	clr	IRQ_ENABLE_PIA+PIA_REG_CRB

	lda	#$FF			; All port pins are outputs
	sta	IRQ_ENABLE_PIA+PIA_REG_DDRA
	sta	IRQ_ENABLE_PIA+PIA_REG_DDRB

	; Disable DDR access / enable peripheral interface access.
	lda	#PIA_CRx_DDR_PI
	sta	IRQ_ENABLE_PIA+PIA_REG_CRA
	sta	IRQ_ENABLE_PIA+PIA_REG_CRB

	; Set all IRQ enables to 0.
	clr	IRQ_ENABLE_L_REG
	clr	IRQ_ENABLE_H_REG

	; Set the IRQ Vector Base register.
	ldd	#irq_vectab
	sta	IRQ_VECBASE_REG
	stb	IRQ_VECBASE_REG+1

	; Initialize all slots to the default handler.
	lda	#16
	ldx	#irq_vectab
	ldy	#vec_irq
1	sty	,X++
	deca
	bne	1B

	;
	; Enable IRQs.
	;
	andcc	#~CC_I

	;
	; Initialize the displays and console.
	; N.B. The display subsystem must be enabled first!
	;
	jsr	display_init
	jsr	cons_init

	;
	; Hello, world!
	;
	jsr	iputs
	fcc	"@thorpej's 6809 Playground OS\r\n"
	fcc	"Version "
	fcc	"0.6"		; Change version number here!
	fcc	"\r\n"
	fcn	"Built: "
	ldx	#build_date
	jsr	puts
	jsr	puts_crlf

	;
	; Report the CPU we're built for and it's clock speed.
	;
	jsr	iputs
	fcc	"CPU: "
	if	CONFIG_6309
	fcc	"6309E"
	else
	fcc	"6809E"
	endif
	fcn	" @ "
	lda	CLOCK_SPEED_REG
	jsr	printdec8
	jsr	iputs
	fcn	"MHz\r\n"

	;
	; Report the RAM sizes.
	;
	jsr	iputs
	fcn	"Low RAM: "
	lda	lbram_nchips
	jsr	print_ram_size
	jsr	puts_crlf

	jsr	iputs
	fcn	"High RAM: "
	lda	hbram_nchips
	jsr	print_ram_size
	jsr	puts_crlf

	jmp	warm_boot		; Now go do a warm boot.

	;
	; Library routines
	;
	include "../lib/memcpy8.s"
	include "../lib/memcpy16.s"
	include "../lib/memset8.s"
	include "../lib/memset16.s"
	include "../lib/memzero8.s"
	include "../lib/memzero16.s"
	include "../lib/parsedec.s"
	include "../lib/parsehex.s"
	include "../lib/parsetbl.s"
	include "../lib/parsetok.s"
	include "../lib/parsews.s"
	include "../lib/parseeol.s"
	include "../lib/printhex.s"
	include "../lib/printdec.s"
	include "../lib/puts.s"
	include "../lib/strcmp.s"
	include "../lib/strtoupper.s"
	include "../lib/toupper.s"
	include "../lib/mulDx10.s"
	include "../lib/udiv16.s"

	if CONFIG_NHACP
	; NHACP lives in banked ROM
	include "../nhacp/nhacp.exp"
	endif

	;
	; Device drivers.
	;
	include "../drivers/cons/cons.s"
	include "../drivers/cons/cons_getline.s"
	include "../drivers/display/display.s"

	if CONFIG_TL16C550
	include "../drivers/tl16c550/tl16c550.exp"
	include "../drivers/tl16c550/tl16c550.s"
	endif

	if CONFIG_CONSOLE_TL16C550
	include "../drivers/tl16c550/tl16c550-cons.s"
	elsif CONFIG_CONSOLE_W65C51
	include "../drivers/w65c51/w65c51.s"
	endif

	if CONFIG_DISPLAY_TMS9918A
	include "../sys-api/tms9918a-api.exp"
	include "../drivers/tms9918a/tms9918a-base.s"
	include "../drivers/tms9918a/tms9918a-tty.s"
	; include "../drivers/tms9918a/tms9918a-font-spleen.s"
	include "../drivers/tms9918a/tms9918a-font-onascii.s"
	endif

	if CONFIG_NHACP_TL16C550
	include "../drivers/tl16c550/tl16c550-nhacp.s"
	elsif CONFIG_NHACP_W65C51
	include "../drivers/w65c51/w65c51-nhacp.s"
	endif

;
; brom_switch
;	Switch ROM banks.
;
; Arguments --
;	A - bank to switch to
;
; Returns --
;	A - previous bank number
;
; Clobbers --
;	None.
;
brom_switch
	pshs	X		; Save X.
	ldx	#ROM_BANK_REG	; bank register
	anda	#BROM_MAXBANK
bank_switch_common
	pshs	B		; Save B.
bank_switch_common0
	ldb	,X		; B = previous bank number
	sta	,X		; set new bank
	tfr	B,A		; return old bank in A
	puls	B,X,PC		; Restore and return.

;
; lbram_switch
;	Switch Low RAM banks.
;
; Arguments --
;	A - bank to switch to + banking flags.
;
; Returns --
;	A - previous bank number
;
; Clobbers --
;	None.
;
; Notes --
;	This routine examines the upper 2 bits of A and behaves as
;	follows:
;
;		00	switch both LBRAM0 and LBRAM1
;		01	switch only LBRAM0
;		10	switch only LBRAM1
;		11	switch both LBRAM0 and LBRAM1
;
;	This allows programs that don't particularly care about the
;	16K split to naively treat the region as a single 32K unit.
;
;	N.B. when switching BOTH banks, the "previous bank" return
;	will be for LBRAM0 only!
;
lbram_switch
	pshs	B,X		; Save registers.
	bita	#$c0		; any flag bits set?
	bne	1F		; yes, go handle them.
lbram_switch_both		; (expected common case is "no")
	sta	LBRAM1_BANK_REG	 ; set LBRAM1 first
	ldx	#LBRAM0_BANK_REG ; then go do LBRAM0
	bra	bank_switch_common0

1	tfr	A,B		; argument into B
	anda	#LBRAM_MAXBANK	; get rid of flag bits
	andb	#$c0		; get rid of bank number bits
	rolb			; Get the upper 2 bits...
	rolb			; ...of B into...
	rolb			; ...the lower 2 bits.
	aslb			; index to table offset
	ldx	#lbram_switch_jmptab
	jmp	[B,X]		; go handle the case we're asked to handle.

lbram_switch_jmptab
	fdb	lbram_switch_both
	fdb	lbram_switch_0
	fdb	lbram_switch_1
	fdb	lbram_switch_both

lbram_switch_0
	ldx	#LBRAM0_BANK_REG
	bra	bank_switch_common0

lbram_switch_1
	ldx	#LBRAM1_BANK_REG
	bra	bank_switch_common0

;
; hbram_switch
;	Switch High RAM banks.
;
; Arguments --
;	A - bank to switch to
;
; Returns --
;	A - previous bank number
;
; Clobbers --
;	None.
;
hbram_switch
	pshs	X		; Save X.
	ldx	#HBRAM_BANK_REG	; bank register
	anda	#HBRAM_MAXBANK
	bra	bank_switch_common

;
; brom_call
;	Call a routine in banked ROM.  The ROM bank will be switched as
;	needed and restored upon return.
;
; Arguments --
;	A banked call descriptor is an immediate in the instruction
;	stream.  This descriptor is a 3 byte datum that has the format:
;
;		fcc	bank_number
;		fdb	system address of routine's jump table entry
;
;	The reason for mandating a jump table is to de-tangle the symbol
;	namespace between the fixed ROM image and the banked ROM images.
;	Banked ROM images can, as a separate step, define their outside
;	entry points for other modules to import.
;
;	Other arguments are defined by the subroutine being called.
;
; Returns --
;	Defined by the subroutine being called.
;
; Clobbers --
;	None.
;
; Notes --
;
;	Routines that are called using this trampoline have a different ABI.
;	A normal subroutine call has a stack that looks like this:
;
;		2,S	<maybe arguments pushed onto the stack>
;		1,S	return address (lsb)
;		0,S	return address (msb)
;
;	But the banked call trampoline uses the stack as a working
;	area.  The stack when the target function is called looks
;	like this:
;
;		13,S	<maybe arguments pushed onto the stack>
;		12,S	return address (lsb) (back to caller of trampoline)
;		11,S	return address (msb) (back to caller of trampoline)
;		10,S	...
;		9,S	...
;		8,S	...
;		7,S	...
;		6,S	...
;		5,S	<scratch area for trampoline>
;		4,S	saved bank
;		3,S	target routine address (lsb)
;		2,S	target routine address (msb)
;		1,S	return address (to trampoline) (lsb)
;		0,S	return address (to trampoline) (msb)
;
;	...and even this is not a guarantee.
;
;	For this reason, it is recommended that arguments are passed
;	just in registers.  If you really want to push arguments onto
;	the stack, consider using the U register to pass that location
;	to the subroutine.
;
brom_call
	;
	; 1,S		return address (lsb)
	; 0,S		return address (msb) (actually pointer to desc)
	;
	pshs	CC,A,X,U	; Save A, X, and U, make a spot for CC
	;
	; 7,S		return address (lsb)
	; 6,S		return address (msb) (actually pointer to desc)
	; 5,S		saved U (lsb)
	; 4,S		saved U (msb)
	; 3,S		saved X (lsb)
	; 2,S		saved X (msb)
	; 1,S		saved A
	; 0,S		slot for new CC to return
	;
	ldx	6,S		; X = pointer to descriptor
	lda	,X+		; A = target bank #
	ldu	,X++		; U = address of jump table slot
	stx	6,S		; Advance return address past descriptor
	bsr	brom_switch	; Switch banks.
	pshs	A		; Save previous bank.
	ldu	,U		; U = value in jump table slot
	pshs	U		; Push target routine address onto stack
	;
	; 10,S		return address (lsb)
	; 9,S		return address (msb) (the real deal)
	; 8,S		saved U (lsb)
	; 7,S		saved U (msb)
	; 6,S		saved X (lsb)
	; 5,S		saved X (msb)
	; 4,S		saved A
	; 3,S		slot for new CC to return
	; 2,S		saved bank
	; 1,S		target routine address (lsb)
	; 0,S		target routine address (msb)
	;
	ldu	7,S		; restore U for function call
	ldx	5,S		; restore X for function call
	lda	4,S		; restore A for function call
	jsr	[,S]		; call the subroutine
	pshs	CC		; save resulting CC
	;
	; 11,S		return address (lsb)
	; 10,S		return address (msb) (the real deal)
	; 9,S		saved U (lsb)
	; 8,S		saved U (msb)
	; 7,S		saved X (lsb)
	; 6,S		saved X (msb)
	; 5,S		saved A
	; 4,S		slot for new CC to return
	; 3,S		saved bank
	; 2,S		target routine address (lsb)
	; 1,S		target routine address (msb)
	; 0,S		new CC to return
	;
	; From this point on, we'll only adjust the stack once before
	; return because it'll be a few cycles faster.
	;
	stu	8,S		; stash updated U
	stx	6,S		; stash updated X
	sta	5,S		; stash updated A
	lda	,S		; get resulting CC into A
	sta	4,S		; save it in CC return slot
	lda	3,S		; get saved bank
	bsr	brom_switch	; Switch to to saved bank
	leas	4,S		; pop that stuff off the stack
	;
	; 7,S		return address (lsb)
	; 6,S		return address (msb) (the real deal)
	; 5,S		updated U (lsb)
	; 4,S		updated U (msb)
	; 3,S		updated X (lsb)
	; 2,S		updated X (msb)
	; 1,S		updated A
	; 0,S		updated CC
	;
	puls	CC,A,X,U,PC	; Restore and return.

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Interrupt API
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; irq_set_handler
;	Set the handler for the specified IRQ.
;
; Arguments --
;	A - IRQ number (0 - 15)
;
;	X - Handler address
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
irq_set_handler
	pshs	A,Y		; save registers
	cmpa	#15
	bhi	99F		; don't to anything if bogus IRQ number
	asla			; IRQ number to table offset
	ldy	#irq_vectab	; Y = vector table address
	stx	A,Y		; store handler address
99	puls	A,Y,PC		; restore and return

;
; irq_get_handler
;	Get the handler for the specified IRQ.
;
; Arguments --
;	A - IRQ number (0 - 15)
;
; Returns --
;	X - Handler address
;
; Clobbers --
;	None.
;
irq_get_handler
	pshs	A		; save registers
	cmpa	#15
	bhi	99F		; don't to anything if bogus IRQ number
	asla			; IRQ number to table offset
	ldx	#irq_vectab	; X = vector table address
	ldx	A,X		; get handler address
1	puls	A,PC		; restore and return
99	ldx	#vec_irq
	bra	1B

;
; irq_clear_handler
;	Clear the handler for the specified IRQ.
;
; Arguments --
;	A - IRQ number (0 - 15)
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
irq_clear_handler
	pshs	X		; save registers
	cmpa	#15
	bhi	99F		; don't do anything if bogus IRQ number
	bsr	irq_disable	; make sure it's disabled first
	ldx	#vec_irq	; default IRQ handler
	bsr	irq_set_handler
99	puls	X,PC		; restore and return

;
; irq_enable
;	Enable the specified IRQ.
; irq_disable
;	Disable the specified IRQ.
;
; Arguments --
;	A - IRQ number (0 - 15)
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
irq_enable
	pshs	A,X,Y		; save registers
	ldx	#irq_do_enable	; push action
	pshs	X
	bra	1F

irq_disable
	pshs	A,X,Y		; save registers
	ldx	#irq_do_disable	; push action
	pshs	X
1
	ldx	#irq_bittab	; X = IRQ bit table
	cmpa	#8		; IRQ >= 8?
	bhs	1F		; Go handle it.
	ldy	#IRQ_ENABLE_L_REG
	bra	2F
1
	cmpa	#15		; don't do anything if bogus IRQ number
	bhi	99F
	suba	#8		; adjust IRQ
	ldy	#IRQ_ENABLE_H_REG
2
	lda	A,X		; IRQ number -> mask bit
	jmp	[,S]		; DO EEEEET!

irq_bittab
	fcb	$01
	fcb	$02
	fcb	$04
	fcb	$08
	fcb	$10
	fcb	$20
	fcb	$40
	fcb	$80

irq_do_enable
	ora	,Y		; set bit in enable register
	bra	99F

irq_do_disable
	coma			; invert mask
	anda	,Y		; clear bit in enable register
99
	sta	,Y		; update enable register
	leas	2,S		; pop action
	puls	A,X,Y,PC	; restore and return

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; File I/O API
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

fs_avail
	if CONFIG_NHACP_TL16C550
	fdb	ace_nhacp_fsops
	elsif CONFIG_NHACP_W65C51
	fdb	dacia_fsops
	endif
fs_avail_end

;
; fs_atindex
;	Look up and return the file system at the specified
;	drive index.
;
; Arguments --
;	A - drive specifier for the mount (A: == 1)
;
; Returns --
;	X - pointer to fsops for mounted file system, NULL if none at
;	that drive index.
;
; Clobbers --
;	A
;
fs_atindex
	tsta
	beq	99F		; invalid drive specifier
	cmpa	#fs_maxdrives
	bhi	99F		; invalid drive specifier
	deca			; convert to 0-based index
	asla			; index to table offset
	ldx	#fs_drives
	ldx	A,X		; X = ops at drive slot
	rts
99
	ldx	#0
	rts

;
; fs_getcur
;	Look up and return the current file system (drive).
;
; Arguments --
;	None.
;
; Returns --
;	X - pointer to fsops for the current drive, NULL if no drive
;	currently selected.
;
; Clobbers --
;	None.
;
fs_getcur
	pshs	A
	lda	fs_curdrive
	bsr	fs_atindex
	puls	A,PC

;
; fs_setcur
;	Set the current file system (drive).
;
; Arguments --
;	A - drive specifier for the mount (A: == 1)
;
; Returns --
;	Sets Z if there is no file system at the specified slot,
;	or if the slot is invalid.  Clears Z upon success.
;
; Clobbers --
;	None.
;
fs_setcur
	pshs	A,X

	tsta
	beq	99F		; invalid drive specifier
	cmpa	#fs_maxdrives
	bhi	99F		; invalid drive specifier

	deca			; convert to 0-based index
	asla			; index to table offset
	ldx	#fs_drives
	ldx	A,X		; X = fsops pointer at specified slot
	beq	99F		; no FS at this slot

	lda	,S		; recover argument
	sta	fs_curdrive	; set as current drive
	andcc	#~CC_Z		; clear Z to indicate success
98	puls	A,X,PC
99
	orcc	#CC_Z		; set Z to indicate failure
	bra	98B

;
; fs_mount
;	Mount a file system.
;
; Arguments --
;	X - pointer to NUL-terminated device name of fsops representing
;	the file system to mount.
;
;	A - drive specifier for the mount (A: == 1)
;
; Returns --
;	A - error code if mount fails (0 == no error, mount succeeded)
;
; Clobbers --
;	None.
;
fs_mount
	pshs	A,B,X,Y

	; Look up the fsops by the provided name.
	ldx	#fs_avail
	pshs	X
	;
	; 4,S	device name pointer argument
	; 3,S	saved B
	; 2,S	saved A
	; 0,S	pointer to current slot in fs_avail
	;
1	cmpx	#fs_avail_end
	lbeq	fs_mount_esrch	; file system not found

	; Get the name pointer for the next candidate.  Since fsov_devname
	; is the first field in the struct, we can use indirect addressing
	; to load it.
	ldy	[,X++]		; also advance to next slot
	stx	,S		; remember next slot
	ldx	4,S		; recover devname argument
	jsr	strcmp		; compare names
	beq	1F		; found a match!
	ldx	,S		; recover next slot
	bra	1B		; go try again

1	; Pull the next slot back into X, and walk back to the slot
	; that matched and load that pointer into X.
	puls	X
	ldx	,--X

	; X now points to the matching fsops.

	; Now, check to see if the fsops provided is currently
	; mounted somewhere else.
	ldy	#fs_drives
	clrb
1	cmpx	B,Y		; X same as fs_drives[idx]?
	beq	fs_mount_ebusy
	addb	#2		; next table offset
	cmpb	#(fs_maxdrives * 2)
	beq	1F		; finished scanning
	bra	1B
1
	; Next, check to see if something is already mounted in this slot.
	ldb	,S		; Get drive specifier
	beq	fs_mount_einval	; invalid drive specifier
	cmpb	#fs_maxdrives
	bhi	fs_mount_einval	; invalid drive specifier
	decb			; make 0-based index
	aslb			; convert to table offset
	tst	B,Y		; check the slot
	bne	fs_mount_ebusy	; something already there.

	; Ok, everything looks good.  Try to mount the file system.
	jsr	[fsov_mount,X]
	tsta			; error return?
	bne	fs_mount_error

	; Success!  Record the mount in the table.
	stx	B,Y

	; If no drive is currently selected, then select this one we
	; just mounted.
	lda	fs_curdrive
	bne	98F
	lda	,S		; Get drive specifier
	sta	fs_curdrive

	; Since we just selected this as the current drive, change the
	; current working directory to "/" on that drive.
	lda	#'/'
	sta	,-S		; S now points to the string "/"
	lda	#1		; A = length of string (1)
	tfr	S,X		; X = "/"
	jsr	fs_chdir	; Go do it.
	leas	1,S		; pop the 1 byte path.
	tsta			; Did an error occur?
	beq	98F		; nope, we are all set.
	tfr	A,B		; save error code

	lda	#'@'
	adda	fs_curdrive
	jsr	[SysSubr_cons_putc]
	jsr	iputs
	fcn	":/ - "
	tfr	B,A		; recover error code
	BCall	"errorstr_print"
	jsr	puts_crlf
	clr	fs_curdrive	; clear the current drive on error.

98	clr	,S		; return 0 (no error) in A
99	puls	A,B,X,Y,PC	; restore and return

fs_mount_error
	sta	,S		; error into A stack slot
	bra	99B

fs_mount_ebusy
	lda	#EBUSY
	bra	fs_mount_error

fs_mount_einval
	lda	#EINVAL
	bra	fs_mount_error

fs_mount_esrch
	leas	2,S		; pop temp from stack
	lda	#ESRCH
	bra	fs_mount_error

;
; fs_ummount
;	Unmount a file system.
;
; Arguments --
;	A - drive specifier for the mount (A: == 1)
;
; Returns --
;	A - error code if unmount fails (0 == no error, unmount succeeded)
;
; Clobbers --
;	None.
;
fs_unmount
	pshs	A,X,Y

	tst	A
	beq	fs_unmount_einval
	cmpa	#fs_maxdrives
	bhi	fs_unmount_einval

	deca			; convert to 0-based index
	asla			; index to table offset
	ldx	#fs_drives
	leax	A,X		; X points to drive slot
	ldy	,X		; Y = fsops of file system
	beq	fs_unmount_esrch

	jsr	[fsov_unmount,Y]
	clr	,X+		; zero out the drive slot
	clr	,X+

	; If we just unmounted the current drive, clear the current
	; drive.
	lda	fs_curdrive
	cmpa	,S
	bne	1F
	clr	fs_curdrive
1	clr	,S		; return 0 (no error) in A
99	puls	A,X,Y,PC	; restore and return

fs_unmount_error
	sta	,S
	bra	99B

fs_unmount_einval
	lda	#EINVAL
	bra	fs_unmount_error

fs_unmount_esrch
	lda	#ESRCH
	bra	fs_unmount_error

;
; fs_chdir
;	Change the current working directory.
;
; Arguments --
;	A - length of path string.
;	X - pointer to path string.
;
; Returns --
;	A - error code
;
; Clobbers --
;	None.
;
fs_chdir
	pshs	B,X,Y
	;
	; 4,S
	; 3,S	saved Y
	; 2,S
	; 1,S	saved X
	; 0,S	saved B
	;
	; XXX No drive letter processing yet.
	;
	ldx	#monitor_fargs
	sta	fopen_namelen,X	; stash the name length, D now free
	ldy	#monitor_fcb	; Y = FCB
	sty	fopen_fcb,X	; FCB always at offset 0

	; Initialize the open arguments.
	ldd	1,S		; pointer to path
	std	fopen_name,X
	ldd	#O_RDONLY+O_DIRECTORY
	std	fopen_flags,X

	; Open the directory.
	jsr	[SysSubr_file_open]
	lda	fcb_error,Y
	bne	99F		; get out now if error occurred.

	; Now that the new CWD is open, close the existing one.
	; X still points to the arguments buffer.
	ldy	#cwd_fcb
	sty	fclose_fcb,X
	jsr	[SysSubr_file_close]

	; Now copy monitor FCB to the CWD FCB, and zap the monitor FCB.
	ldx	#cwd_fcb
	ldy	#monitor_fcb
	lda	#fcb_fcbsz
	jsr	memcpy8
	ldx	#monitor_fcb
	jsr	memzero8

	clra			; success!

99	puls	B,X,Y,PC	; restore and return

;
; file_open
;	Open a file.
;
; Arguments --
;	X - pointer to file open arguments
;
; Returns --
;	Error status in File Control Block.
;
; Clobbers --
;	None.
;
file_open
	;
	; XXX No drive letter processing yet.
	;
	pshs	Y

	; Make sure this FCB isn't busy already. Since the file ops
	; pointer is the first field in the FCB, we can just use
	; indirect addressing, rather than issuing 2 load instructions.
	ldy	[fopen_fcb,X]
	bne	file_open_ebusy

	if CONFIG_NHACP_TL16C550
	ldy	#ace_nhacp_fileops
	elsif CONFIG_NHACP_W65C51
	ldy	#dacia_fileops
	else
	XXX ERROR ERROR ERROR XXX
	endif

	sty	[fopen_fcb,X]	; store fileops pointer in FCB
	jsr	[fov_open,Y]
	puls	Y,PC

file_open_ebusy
	pshs	A
	lda	#EBUSY
	bra	file_io_error

;
; file_io
;	Perform I/O to a file.
;
; Arguments --
;	X - pointer to file IO arguments
;
; Returns --
;	Error status in File Control Block.
;
; Clobbers --
;	None.
;
file_io
	pshs	Y

	; Get the file ops vector.  Since the file ops pointer
	; is the first field in the FCB, we can just use indirect
	; addressing, rather than issuing 2 load instructions.
	ldy	[fio_fcb,X]
	beq	file_io_ebadf	; NULL fov -> EBADF
	jsr	[fov_io,Y]
	puls	Y,PC

file_io_ebadf
	pshs	A
	lda	#EBADF

file_io_error
	ldy	fio_fcb,X
	sta	fcb_error,Y
	puls	A,Y,PC

;
; file_close
;	Close an open file.
;
; Arguments --
;	X - pointer to file close arguments
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
file_close
	pshs	Y

	; Get the file ops vector.  Since the file ops pointer
	; is the first field in the FCB, we can just use indirect
	; addressing, rather than issuing 2 load instructions.
	ldy	[fclose_fcb,X]
	beq	file_io_ebadf	; NULL fov -> EBADF
	jsr	[fov_close,Y]
	; File is now closed, clear the file ops vector.
	ldy	#0
	sty	[fclose_fcb,X]
	puls	Y,PC

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Timer API
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; timer_add
;	Add a timer.
;
; Arguments --
;	X - pointer to timer object
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
timer_add
	pshs	X,Y		; save registers

	; X = current element on list
	; Y = address of last timer's "next" pointer

	ldy	#timer_list
1	ldx	,Y
	beq	98F		; got NULL, insert at the tail
	cmpx	,S		; equal to the argument?
	beq	99F		; yes, just get out.
	leay	tmr_next,X	; Y = &X->tmr_next
	bra	1B		; check again

98	ldx	,S		; recover timer argument
	clr	tmr_next,X	; ensure new timer's "next" is NULL
	clr	tmr_next+1,X
	clr	tmr_t1,X	; ensure the timer is not armed
	clr	tmr_t0,X
	stx	,Y		; store the pointer

99	puls	X,Y,PC		; restore and return

;
; timer_remove
;	Remove a timer.
;
; Arguments --
;	X - pointer to the timer object
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
timer_remove
	pshs	X,Y		; save registers

	; X = current element on list
	; Y = address of last timer's "next" pointer

	ldy	#timer_list
1	ldx	,Y
	beq	99F		; got NULL, no work to do
	cmpx	,S		; equal to the argument?
	beq	98F		; yes, go unlink it
	leay	tmr_next,X	; Y = &X->tmr_next
	bra	1B

98	ldx	tmr_next,X	; X = X->tmr_next
	stx	,Y		; *Y = X->tmr_next
	ldx	,S		; recover timer argument
	clr	tmr_next,X	; ensure timer's "next" is NULL
	clr	tmr_next+1,X

99	puls	X,Y,PC		; restore and return

;
; timer_process
;	Process the registered timers.  This is called from whatever
;	60Hz interrupt source we're using.
;
; Arguments --
;	None.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
timer_process
	pshs	A,B,X		; save registers

	; X = current element on list.  We can use it as the list head
	; pointer as well, since the "next" pointer is the first field
	; in the object.

	ldx	#timer_list
1	ldx	tmr_next,X	; X = X->tmr_next
	beq	99F		; got NULL, get out

	ldd	tmr_t1,X	; D = remaining ticks
	beq	1B		; timer not active, get next timer

	subd	#1		; Decrement timer.
	std	tmr_t1,X	; store remaining ticks
	bne	1B		; timer not yet 0, get next timer

	ldd	tmr_callout,X	; callout routine set?
	beq	1B		; no, get next timer

	jsr	[tmr_callout,X]	; call callout routine
	bra	1B		; get next timer

99	puls	A,B,X,PC	; restore and return

;
; timer_reset
;	Reset all of the timers.  Called at warm boot.  Any timer
;	that wishes to persist must re-register.
;
; Arguments --
;	None.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
timer_reset
	pshs	A,B		; save registers
	ldd	#0		; D = 0
	std	timer_list	; timer_list = NULL (atomic wrt. interrupts)
	puls	A,B,PC		; restore and return

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Miscellaneous routines
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; panic
;	Something is seriously effed up.
;
; Arguments --
;	X - panic string
;
; Returns --
;	LOL, no.
;
; Clobbers --
;	WTF cares?
;
panic
	jsr	iputs
	fcn	"panic: "
	jsr	puts
	jsr	puts_crlf
1	bra	1B

;
; error
;	Print an error message prefix.
;
; Arguments --
;	None.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
error
	jsr	iputs
	fcn	"ERROR: "
	rts

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;
	; Monitor environment -- main loop
	;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

monitor_main
	lds	current_iframe		; Reset the stack pointer
	jsr	monitor_getline		; X = command line
	jsr	parsews			; toss leading whitespace
	ldy	#monitor_cmdtab		; Y = command table
	jsr	parsetbl_lookup		; A = command index
	asla				; index -> offset
	ldy	#monitor_cmdjmptab	; Y = command jump table
	jmp	[A,Y]			; go run command

;
; monitor_getline
;	Get an input line for the monitor environment.
;
; Arguments --
;	None.
;
; Returns --
;	X -- pointer to the input buffer.
;
;	A -- Also contains the strlen() of the input line.
;
; Clobbers --
;	None.
;
; Notes --
;	SysSubr_cons_getline returns the input buffer in U, we
;	move it into X.
;
monitor_getline
	pshs	U			; Save U
	jsr	iputs
	fcn	"pgmon> "
	jsr	[SysSubr_cons_getline]	; Get the input line.
	tfr	U,X			; Return pointer in Y
	puls	U,PC			; Restore and return

monitor_cmdtab
	fcc	'@'+$80			; access memory
	fcc	'J'+$80			; jump to address
	fcc	'C'+$80			; continue
	fcc	"RESE",'T'+$80		; reset system
	fcc	'R'+$80			; print / set register
	fcc	"LOAD",'S'+$80		; load S-Records
	fcc	'?'+$80			; help
	fcc	"OIN",'K'+$80		; not "moo".
	fcc	"OF",'F'+$80		; power off system
	fcc	"MOUN",'T'+$80		; mount file system
	fcc	"UMOUN",'T'+$80		; unmount file system
	fcc	"L",'S'+$80		; list directory
	fcc	"DI",'R'+$80		; list directory
	fcc	0

monitor_cmdjmptab
	fdb	cmd_access_mem
	fdb	cmd_jump
	fdb	cmd_continue
	fdb	cmd_reset
	fdb	cmd_reg
	fdb	cmd_loads
	fdb	cmd_help
	fdb	cmd_oink
	fdb	cmd_off
	fdb	cmd_mount
	fdb	cmd_umount
	fdb	cmd_ls
	fdb	cmd_ls
	fdb	cmd_unknown

;
; cmd_unknown
;	Unknown command handler.
;
cmd_unknown
	tst	,X
	beq	1F			; don't report error for empty line
	jsr	error
	jsr	iputs
	fcn	"unknown command"
	bra	suggest_help

;
; syntax_error
;
;	Generic syntax error handler for commands.
;
syntax_error
	jsr	error
	jsr	iputs
	fcn	"syntax error"

suggest_help
	jsr	iputs
	fcn	" - ? for help\r\n"
1	jmp	monitor_main

;
; cmd_access_mem
;	Access memory.
;
;	@addr			- print 1 byte
;	@			- print 1 byte at next address
;	@addr,len		- print len bytes
;	@,len			- print len bytes next address
;	@addr val [val ...]	- set bytes starting at address
;	@ val [val ...]		- set bytes starting at next address
;	@addr,len val		- set length bytes at address to value
;	@,len val		- set length bytes at next address to value
;
cmd_access_mem
	jsr	parse_addr	; D = address
	beq	1F		; No address specified.
	std	mem_access_addr	; Stash the address

1	lda	,X		; A = *X
	cmpa	#','		; Is it a comma?
	bne	1F		; No length specified.

	leax	1,X		; Advance past comma.
	jsr	parsedec	; D = length
	lbeq	syntax_error	; Not a number? Syntax error.
	lbvs	syntax_error	; Overflow? Syntax error.
	bra	2F

1	M_clrd			; No length specified
2	std	mem_access_len

	jsr	parsews		; gobble up the whitespace
	beq	cmd_access_mem_rd ; no whitespace... probably a read.
	jsr	parseeol	; check for EOL
	bne	cmd_access_mem_rd ; definitely a read.

	;
	; We're writing, then.  Based on the length, we're easier
	; writing out multiple bytes as specified on the command line,
	; or we're writing out a single byte multiple times.
	;
	; D will still contain that length from above.
	;
	ldy	mem_access_addr	; Y = address to access
	cmpd	#0		; D == 0?
	bne	cmd_access_memset ; No -> memset()

cmd_access_mem_wr
	jsr	parsehex8	; A = next value
	beq	syntax_error	; Z set? Syntax error.
	bvs	syntax_error	; V set? Syntax error.
	sta	,Y+		; Write value, advance address
	sty	mem_access_addr	; Update this as we go.
	jsr	parseeol	; skip whitespace, check for EOL
	beq	cmd_access_mem_wr ; not EOL, check for more values
	jmp	monitor_main	; All done.

cmd_access_memset
	jsr	parsehex8	; A = value
	beq	syntax_error	; Z set? Syntax error.
	bvs	syntax_error	; V set? Syntax error.
	pshs	A		; Stash it temporarily.
	jsr	parseeol	; Only one value allowed.
	lbeq	syntax_error	; Not EOL? Syntax error.
	ldd	mem_access_len	; D = length
	leax	D,Y		; X = end address
	puls	A		; Get value back into A
	pshs	X		; Get end address onto stack
1	sta	,Y+
	cmpy	,S		; Y == end?
	bne	1B		; Nope, keep going.
	; No need to pop length off the stack.
	jmp	monitor_main

cmd_access_mem_rd
	jsr	parseeol	; make sure we're at EOL
	lbeq	syntax_error	; No -> syntax error

	leas	-1,S		; Push a slot onto the stack
				; for counting bytes.

	ldy	mem_access_addr	; Y = address to access

	ldd	mem_access_len	; If length unspecified or 1, then
	cmpd	#1		; we have a simplier display format.
	bls	5F

1	tfr	Y,D		; Print the address.
	andb	#$F0		; Always align start address.
	tfr	D,Y
	jsr	printhex16
	jsr	iputs
	fcn	":"

	lda	#16		; Byte count = 8
	sta	,S

2	cmpy	mem_access_addr
	bhs	2F
	jsr	iputs
	fcn	" .."
	dec	,S
	leay	1,Y
	bra	2B

2	lda	,Y+		; A = byte being accessed
	jsr	iputs
	fcn	" "
	jsr	printhex8

	ldd	mem_access_len
	subd	#1		; Subtract 1 from length
	std	mem_access_len	; store it back
	beq	3F		; Get out if we're done.

	dec	,S		; byte count--
	bne	2B		; just go around again if not 0

	jsr	puts_crlf
	bra	1B

3	sty	mem_access_addr	; Remember where we left off.
	jsr	puts_crlf
	jmp	monitor_main

5	tfr	Y,D		; Print the address.
	jsr	printhex16
	jsr	iputs
	fcn	": "
	lda	,Y+		; Get the byte
	jsr	printhex8	; Print it.
	jsr	puts_crlf
	jmp	monitor_main

;
; parse_addr
;	Parse an address.  This is just a 16-bit hexadecimal number
;	but may also be one of our symbolic address names.
;
; Arguments --
;	X - pointer to buffer to be parsed
;
; Returns --
;	D - value of parsed number.
;
;	X - Updated to point to the first non-hex character following
;	the number.
;
;	CC_Z is clear if any valid digits were parsed, CC_Z set and the
;	value in D is 0 if no valid digits were found.
;
;	CC_V is set if the number overflowed 16 bits.  The value
;	$FFFF is loaded into D in that case.  If the value fits in
;	16 bits, then CC_V is cleared.
;
; Clobbers --
;	None.
;
parse_addr
	pshs	Y		; Save registers
	; First check for symbolic addresses.
	ldy	#symbolic_addrtab
	jsr	parsetbl_lookup
	cmpa	#symbolic_addrs_cnt ; A == table entry count?
	beq	2F		; Yes, try parsing numbers.
	asla			; table index -> offset
	ldy	#symbolic_addrs	; Y = symbolic addresses table
	ldd	A,Y		; D = address from table
	andcc	#~(CC_Z|CC_V)	; clear Z to return true and clear V
1	puls	Y,PC		; Restore and return

2	; Just parse as a number.
	jsr	parsehex16
	beq	1B		; Z set, just get out
	bvs	3F		; V set, we'll set Z and then get out
	bra	1B		; Otherwise, all good, get out.

3	orcc	#CC_Z		; Set Z to return false
	bra	1B

symbolic_addrtab
	fcc	"ROM_BANK_RE",'G'+$80
	fcc	"LBRAM0_BANK_RE",'G'+$80
	fcc	"LBRAM1_BANK_RE",'G'+$80
	fcc	"HBRAM_BANK_RE",'G'+$80
	fcc	"CLOCK_SPEED_RE",'G'+$80
	fcc	"LBRAM0_STAR",'T'+$80
	fcc	"LBRAM1_STAR",'T'+$80
	fcc	"HBRAM_STAR",'T'+$80
	fcc	"BROM_STAR",'T'+$80
	fcb	0

symbolic_addrs
	fdb	ROM_BANK_REG
	fdb	LBRAM0_BANK_REG
	fdb	LBRAM1_BANK_REG
	fdb	HBRAM_BANK_REG
	fdb	CLOCK_SPEED_REG
	fdb	LBRAM0_START
	fdb	LBRAM1_START
	fdb	HBRAM_START
	fdb	BROM_START
symbolic_addrs_end
symbolic_addrs_cnt	equ	((symbolic_addrs_end - symbolic_addrs) / 2)

;
; cmd_jump
;	Jump to an address.  We use whatever the current interrupt frame is.
;
cmd_jump
	jsr	parseeol		; consume whitespace, check for EOL
	bne	2F			; EOL -> check for valid jump_addr
	jsr	parse_addr		; D = the address
	lbeq	syntax_error		; Not a number? Syntax error.
	jsr	parseeol		; check for EOL
	lbeq	syntax_error		; No? Syntax error.
1	ldx	current_iframe		; X = interrupt frame
	std	IFE_PC,X		; Store jump address.
	ldd	#call_ret		; return address slot = call_ret
	std	IFE_SIZE,X
	leas	,X			; S = interrupt frame
	rti				; ...and GO!

2	ldd	jump_addr		; Get jump address
	cmpd	#$FFFF			; Address valid?
	bne	1B			; Yes, go use it.

	jsr	error
	jsr	iputs
	fcn	"no valid jump address.\r\n"
	jmp	monitor_main

;
; cmd_reg
;	Print or set a register.
;
cmd_reg
	BCall	"cmd_reg"
	jmp	monitor_main

;
; cmd_loads
;	Load S-Records.
;
cmd_loads
	BCall	"cmd_loads"
	jmp	monitor_main

;
; cmd_continue
;	Continue an interrupted task
;
cmd_continue
	tst	can_continue
	beq	1F
	clr	can_continue
	rti

1	jsr	iputs
	fcn	"Cannot continue current iframe.\r\n"
	jmp	monitor_main

;
; cmd_reset
;	Reset the system
;
cmd_reset
	lda	#PSU_CTRL_RESET
	sta	PSU_CTRL_REG
	jmp	monitor_main

;
; cmd_off
;	Power off the system
;
cmd_off
	lda	#PSU_CTRL_POWEROFF
	sta	PSU_CTRL_REG
	jmp	monitor_main

;
; cmd_help
;	Get help.
;
cmd_help
	BCall	"cmd_help"
	jmp	monitor_main

;
; cmd_oink
;	The dumbest little easter egg.
;
cmd_oink
	BCall	"cmd_oink"
	jmp	monitor_main

;
; cmd_mount
;	Mount a file system.
;
;	mount uart1 a:		; mount UART1 at A:
;	mount			; display mounted file systems
;
cmd_mount
	BCall	"cmd_mount"
	jmp	monitor_main

;
; cmd_umount
;	Unmount a file system.
;
;	umount a:		; unmount whatever is at A:
;
cmd_umount
	BCall	"cmd_umount"
	jmp	monitor_main

;
; cmd_ls
;	List a directory
;
;	ls
;
cmd_ls
	BCall	"cmd_ls"
	jmp	monitor_main

;
; debugger
;	The debugger entry point.  This basically sets up some
;	state and then jumps into the monitor loop.
;
debugger
	sts	current_iframe		; current frame = our stack
	lda	#1			; We can "continue" from this frame
	sta	can_continue
	jsr	iputs
	fcn	"S=$"
	tfr	S,D
	jsr	printhex16
	jsr	puts_crlf
	jsr	iputs
	fcn	"Stopped at PC=$"
	ldd	IFE_PC,S
	jsr	printhex16
	jsr	puts_crlf
	jmp	monitor_main

	;
	; VECTOR HANDLERS
	;
vec_reserved
	;
	; This is the illegal opcode vector on the 6309 when operating
	; in native mode.
	;
	bra	vec_reserved		; hard hang. This should never happen.

vec_swi3
	rti

vec_swi2
	rti

vec_firq
	rti

vec_irq
	rti

vec_swi
	rti

vec_nmi
	;
	; Re-enable interrupts ASAP, in case the console is relying
	; on them.
	;
	andcc	#~CC_I
	jsr	iputs
	fcn	"Debugger switch!\r\n"
	jmp	debugger

vec_reset	equ	cold_boot

	include	"pg09-from-abi-tail.s"

	;
	; VECTOR TABLE
	;
	org	$FFF0
vector_table
	fdb	vec_reserved
	fdb	vec_swi3
	fdb	vec_swi2
	fdb	vec_firq
	fdb	vec_irq
	fdb	vec_swi
	fdb	vec_nmi
	fdb	vec_reset
