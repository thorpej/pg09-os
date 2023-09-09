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

	include "../banked-rom0/pg09-brom0.exp"

	setdp	-1	; Disable automatic direct page addressing

	;
	; System SUBROUTINE jump table.  These are called like normal
	; subroutines, not with SWI as with a system call, i.e.:
	;
	;	jsr	[SysSubr_cons_getc]
	;
SysSubr		macro
SysSubr_\1	fdb	\1
		endm

	;
	; DO NOT CHANGE THE ORDER OR THE ORIGIN OF THESE STATEMENTS UNLESS
	; YOU KNOW EXACTLY WHAT YOU ARE DOING!  THESE ARE PART OF THE OS ABI!
	;
	org	FROM_START

	SysSubr warm_boot	; gen-sysapi.sh fixes this up to SysSubr_exit

	SysSubr	brom_call
	SysSubr brom_switch
	SysSubr	lbram_switch
	SysSubr	hbram_switch

	SysSubr	cons_getc
	SysSubr cons_pollc
	SysSubr cons_putc
	SysSubr cons_getline

	SysSubr display_get_count
	SysSubr display_get_default
	SysSubr display_get_descriptor

	;
	; System ADDRESS equates.  These are the exported names of
	; important system addresses that programs might need to
	; care about.
	;
	; Programs should not care about Fixed RAM (reserved for
	; the operating system), nor should they care about Fixed
	; ROM (only use the exported jump table slot addresses).
	;
SysAddr_LowBankedRAM0		set	LBRAM0_START
SysAddr_LowBankedRAM0_size	set	LBRAM0_SIZE
SysAddr_LowBankedRAM1		set	LBRAM1_START
SysAddr_LowBankedRAM1_size	set	LBRAM1_SIZE
	; Convenience for programs that don't care abou banking.
SysAddr_LowBankedRAM		set	LBRAM0_START
SysAddr_LowBankedRAM_size	set	LBRAM0_SIZE+LBRAM1_SIZE
SysAddr_HighBankedRAM		set	HBRAM_START
SysAddr_HighBankedRAM_size	set	HBRAM_SIZE
SysAddr_BankedROM		set	BROM_START
SysAddr_BankedROM_size		set	BROM_SIZE

	include	"build_date.s"

warm_boot
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
	ldy	#warm_boot
	sty	IFE_SIZE,X
	stx	current_iframe

	;
	; Re-initialize DP to the reset value.
	;
	clra
	tfr	A,DP

	;
	; Re-initialize the console.
	;
	jsr	cons_reinit

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
	; unmask NMIs.
	;
	lds	#LBRAM1_START+LBRAM1_SIZE

	;
	; Clear out FRAM.
	;
	ldx	#FRAM_START
	ldd	#FRAM_SIZE
	jsr	memzero16

	;
	; Now switch to the kernel stack.
	;
	lds	#KSTACK_TOP

	;
	; Initialize the console.
	;
	jsr	cons_init

	;
	; Hello, world!
	;
	jsr	iputs
	fcc	"@thorpej's 6809 Playground OS, version "
	fcc	"0.3"		; Change version number here!
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

	jmp	warm_boot		; Now go do a warm boot.

	;
	; Library routines
	;
	include "../lib/memzero8.s"
	include "../lib/memzero16.s"
	include "../lib/parsedec.s"
	include "../lib/parsehex.s"
	include "../lib/parsetbl.s"
	include "../lib/parsews.s"
	include "../lib/parseeol.s"
	include "../lib/printhex.s"
	include "../lib/printdec.s"
	include "../lib/puts.s"
	include "../lib/toupper.s"
	include "../lib/mulDx10.s"
	include "../lib/udiv16.s"

	;
	; Device drivers.
	;
	include "../drivers/cons/cons.s"
	include "../drivers/cons/cons_getline.s"
	include "../drivers/display/display.s"
	include "../drivers/w65c51/w65c51.s"

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
	tfr	A,B		; argument into B
	anda	#LBRAM_MAXBANK	; get rid of flag bits
	rolb			; Get the upper 2 bits...
	rolb			; ...of B into...
	rolb			; ...the lower 2 bits.
	andb	#3		; get rid of irrelavant bits
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

lbram_switch_both
	sta	LBRAM1_BANK_REG	 ; set LBRAM1 first
	ldx	#LBRAM0_BANK_REG ; then go do LBRAM0
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
;	Call a routine in banked ROM.  Routines that are called using
;	this trampoline have a different ABI.  A normal subroutine call
;	has a stack that looks like this:
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
;	Other arguments defined by the subroutine being called.
;
; Returns --
;	Defined by the subroutine being called.
;
; Clobbers --
;	None.
;
brom_call
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
	ldu	[,X++]		; U = value in jump table slot
	stx	6,S		; Advance return address past descriptor
	bsr	brom_switch	; Switch banks.
	pshs	A		; Save previous bank.
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
	; 0,S		slot for new CC to return
	;
	puls	CC,A,X,U,PC	; Restore and return.

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
