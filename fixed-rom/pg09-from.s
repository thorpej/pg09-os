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
	include "../banked-rom0/pg09-brom0.exp"

	setdp	-1	; Disable automatic direct page addressing

	;
	; System SUBROUTINE jump table.  These are called like normal
	; subroutines, not with SWI as with a system call, i.e.:
	;
	;	jsr	[SysSubr_brom_call]
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

	;
	; System ADDRESS equates.  These are the exported names of
	; important system addresses that programs might need to
	; care about.
	;
	; Programs should not care about Fixed RAM (reserved for
	; the operating system), nor should they care about Fixed
	; ROM (only use the exported jump table slot addresses).
	;
SysAddr_LowBankedRAM		set	LBRAM_START
SysAddr_LowBankedRAM_size	set	LBRAM_SIZE
SysAddr_HighBankedRAM		set	HBRAM_START
SysAddr_HighBankedRAM_size	set	HBRAM_SIZE
SysAddr_BankedROM		set	BROM_START
SysAddr_BankedROM_size		set	BROM_SIZE

pg09os_hello
	fcc	"@thorpej's 6809 Playground OS, version "
	fcc	"0.1"		; Change version number here!
	fcn	"\r\n"

pg09_cpu_banner
	fcc	"CPU: "
	if	CONFIG_6309
	fcc	"6309E"
	else
	fcc	"6809E"
	endif
	fcn	" @ "
pg09_cpu_banner_tail
	fcn	"MHz\r\n"

pg09_hbram_banner
	fcn	"HBRAM: "
pg09_hbram_banner_tail
	fcn	"KB\r\n"

pg09_hbram_probe_bad_str
	fcn	"bad HBRAM"

; HBRAM bank values for the last bank of each 512K RAM chip.
pg09_hbram_probe_tab
	fcb	$3F, $7F, $BF, $FF

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
	; XXX cons_reinit

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
	clr	ROM_BANKER_PIA+PIA_REG_CRA
	clr	RAM_BANKER_PIA+PIA_REG_CRA
	clr	RAM_BANKER_PIA+PIA_REG_CRB

	lda	#$FF			; All port pins are outputs
	sta	ROM_BANKER_PIA+PIA_REG_DDRA
	sta	RAM_BANKER_PIA+PIA_REG_DDRA
	sta	RAM_BANKER_PIA+PIA_REG_DDRB

	; Disable DDR access / enable peripheral interface access.
	lda	#PIA_CRx_DDR_PI
	sta	ROM_BANKER_PIA+PIA_REG_CRA
	sta	RAM_BANKER_PIA+PIA_REG_CRA
	sta	RAM_BANKER_PIA+PIA_REG_CRB

	; Set all banked regions to bank 0.
	clr	ROM_BANK_REG
	clr	LBRAM_BANK_REG
	clr	HBRAM_BANK_REG

	;
	; Put the stack in LBRAM so we can clear FRAM.
	;
	; N.B. this is the first load of the stack pointer, which will
	; unmask NMIs.
	;
	lds	#LBRAM_START+LBRAM_SIZE

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
	ldx	#pg09os_hello
	jsr	puts

	;
	; Report the CPU we're built for and it's clock speed.
	;
	ldx	#pg09_cpu_banner
	jsr	puts
	lda	CLOCK_SPEED_REG
	jsr	printdec8
	ldx	#pg09_cpu_banner_tail
	jsr	puts

	;
	; Probe the High Banked RAM size.  We assume there's at least
	; 512KB, because the kernel requires it.
	;
	ldx	#pg09_hbram_probe_tab
	ldy	#HBRAM_START
	clra				; table index / probed chip count

1	ldb	A,X
	stb	HBRAM_BANK_REG

	clr	,Y			; Store a 0 and make sure it sticks
	tst	,Y
	bne	2F

	ldb	#$55			; Store $55 and make sure it sticks
	stb	,Y
	ldb	,Y
	cmpb	#$55
	bne	2F

	asl	,Y			; Store $AA and make sure it sticks
	ldb	,Y
	cmpb	#$AA
	bne	2F

	inca				; Increment chip count.
	cmpa	#4			; Have we probed all 4 chips?
	blt	1B			; Nope, go back around.

2	tsta
	beq	panic_bad_hbram		; 0 means no HBRAM worked.
	deca				; convert chip count to table index
	ldb	A,X			; get max HBRAM bank #
	stb	hbram_max_bank		; stash it for later
	clr	HBRAM_BANK_REG		; back to bank 0

	inca				; Back to chip count.
	asla				; Multiply by 2 to form the MSB
	clrb				; of # KB of HBRAM.

	ldx	#pg09_hbram_banner
	jsr	puts
	jsr	printdec16
	ldx	#pg09_hbram_banner_tail
	jsr	puts

	jmp	warm_boot		; Now go do a warm boot.

panic_bad_hbram
	ldx	#pg09_hbram_probe_bad_str
	jmp	panic

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
bank_switch_common
	pshs	B		; Save B.
	ldb	,X		; B = previous bank number
	sta	,X		; set new bank
	tfr	B,A		; return old bank in A
	puls	B,X,PC		; Restore and return.

;
; lbram_switch
;	Switch Low RAM banks.
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
lbram_switch
	pshs	X		; Save X.
	ldx	#LBRAM_BANK_REG	; bank register
	bra	bank_switch_common

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
	bra	bank_switch_common

;
; brom_call
;	Call a routine in banked ROM.  Routines that are called using
;	this trampoline have a different ABI.  A normal subroutine call
;	has a stack that looks like this:
;
;		2,S	<maybe arguments pushed onto the stack>
;		0,S	return address
;
;	But a banked-call looks like this:
;
;		8,S	<maybe arguments pushed onto the stack>
;		6,S	return address (back to caller of trampoline)
;		5,S	...
;		4,S	...
;		3,S	scratch area for trampoline
;		2,S	saved previous bank number
;		0,S	return address (back to trampoline)
;
;	For this reason, it is recommended that arguments are passed
;	just in registers.  If you really want to push arguments onto
;	the stack, consider using the U register to pass that location
;	to the subroutine.
;
; Arguments --
;	Y - pointer to banked call descriptor.  A banked call descriptor
;	is a 3 byte datum that has the format:
;
;		1,Y	system address of routine jump table entry
;		0,Y	bank number
;
;	N.B. this similar to what is pushed onto the stack during
;	a banked call, but is not the same.  The reason for mandating
;	a jump table is to de-tangle the symbol namespace between the
;	fixed ROM image and the banked ROM images.  Banked ROM images
;	can, as a separate step, define their outside entry points for
;	other modules to import.
;
;	Y register is undefined upon entry to a banked ROM call and
;	banked ROM calls may use Y without first saving it.
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
	pshs	A,Y		; Save A and Y.
	;
	; 3,S		return address
	; 2,S		saved Y (lsb)
	; 1,S		saved Y (msb)
	; 0,S		saved A
	;
	lda	,Y		; A = target bank #
	ldy	1,Y		; Y = address of jump table slot
	bsr	brom_switch	; Switch banks.
	pshs	A		; Save previous bank.
	;
	; 4,S		return address
	; 3,S		saved Y (lsb)
	; 2,S		saved Y (msb)
	; 1,S		saved A
	; 0,S		saved bank
	;
	lda	1,S		; retrieve saved A
	jsr	[,Y]		; call the subroutine
	sta	1,S		; stash what we'll return in A
	puls	A		; get saved bank
	;
	; 3,S		return address
	; 2,S		saved Y (lsb)
	; 1,S		saved Y (msb)
	; 0,S		saved A
	;
	bsr	brom_switch	; Switch to to saved bank
	puls	A,Y,PC		; Restore and return.

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
	lds	#KSTACK_TOP		; Reset the stack pointer
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
	fcc	'R'+$80			; print / set register
	fcc	"LOAD",'S'+$80		; load S-Records
	fcc	'?'+$80			; help
	fcc	"OIN",'K'+$80		; not "moo".
	fcc	0

monitor_cmdjmptab
	fdb	cmd_access_mem
	fdb	cmd_jump
	fdb	cmd_reg
	fdb	cmd_loads
	fdb	cmd_help
	fdb	cmd_oink
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
	fcc	"LBRAM_BANK_RE",'G'+$80
	fcc	"HBRAM_BANK_RE",'G'+$80
	fcc	"CLOCK_SPEED_RE",'G'+$80
	fcc	"LBRAM_STAR",'T'+$80
	fcc	"HBRAM_STAR",'T'+$80
	fcc	"BROM_STAR",'T'+$80
	fcb	0

symbolic_addrs
	fdb	ROM_BANK_REG
	fdb	LBRAM_BANK_REG
	fdb	HBRAM_BANK_REG
	fdb	CLOCK_SPEED_REG
	fdb	LBRAM_START
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
	jsr	parseeol		; consume whitespace, check for EOL
	bne	cmd_reg_printall	; if EOL, print all regs

	lda	#1			; Push a 1 onto the stack (count)
	pshs	A

	ldy	#cmd_reg_parsetab	; Y = reg name table
	jsr	parsetbl_lookup		; lookup the reg name
	lbeq	syntax_error		; Not found, syntax error.
	pshs	A			; push index onto stack

	jsr	parseeol		; consume whitespace, check for EOL
	bne	cmd_reg_printloop	; if EOL, print the one reg

	jsr	parsehex16		; D = new value
	lbeq	syntax_error		; Not number, syntax error.
	lbvs	syntax_error		; Overflow, syntax error.
	pshs	D			; squirrel it away on the stack
	jsr	parseeol		; make sure we're at EOL now.
	lbeq	syntax_error		; No, syntax error.

	ldy	current_iframe
	ldx	#cmd_reg_iframeoffs	; X = frame offset table
	ldb	2,S			; B = index
	ldb	B,X			; B = frame offset
	leay	B,Y			; Y += register offset in iframe

	ldb	2,S			; B = index (again)
	cmpb	#cmd_reg_iframeoffs16	; Does index say "16-bit value"?
	bhs	1F			; Go deal with it.

	tsta				; Did we overflow 8 bits?
	lbne	syntax_error		; Yes, syntax error.
	ldb	1,S			; B = value
	stb	,Y			; store the value in the iframe.
	jmp	monitor_main

1	ldd	,S			; D = value
	std	,Y			; store the value in the iframe
	jmp	monitor_main

cmd_reg_printall
	lda	#9			; push 9 onto the stack for count
	pshs	A
	clr	,-S			; push 0 onto the stack for index

cmd_reg_printloop
	ldy	current_iframe
	lda	,S			; get current index
	ldx	#cmd_reg_printnames	; X = print name table

	asla				; index to pointer table offset
	ldx	A,X			; X = print name
	lsra				; back to index

	jsr	puts			; print register name
	jsr	iputs			; and separator
	fcn	": "

	ldx	#cmd_reg_iframeoffs	; X = frame offsets
	ldb	A,X			; B = frame offset
	leay	B,Y			; Y += offset into iframe

	cmpa	#cmd_reg_iframeoffs16	; 16-bit value in frame?
	bhs	1F			; go deal with it

	lda	,Y			; load 8-bit value from frame
	jsr	printhex8
	bra	2F

1	ldd	,Y			; load 16-bit value from frame
	jsr	printhex16

2	jsr	puts_crlf
	inc	,S			; index++
	dec	1,S			; count--
	bne	cmd_reg_printloop	; Go around again if more to do.
	jmp	monitor_main

cmd_reg_parsetab
	fcc	"CC",'R'+$80
	fcc	'A'+$80
	fcc	'B'+$80
	fcc	'D','P'+$80
	fcc	'D'+$80
	fcc	'X'+$80
	fcc	'Y'+$80
	fcc	'U'+$80
	fcc	'P','C'+$80
	fcc	0

cmd_reg_iframeoffs
	fcc	IFE_CCR
	fcc	IFE_A
	fcc	IFE_B
	fcc	IFE_DP
cmd_reg_iframeoffs_D
	fcc	IFE_A		; D
	fcc	IFE_X
	fcc	IFE_Y
	fcc	IFE_U
	fcc	IFE_PC

cmd_reg_iframeoffs16	equ	(cmd_reg_iframeoffs_D - cmd_reg_iframeoffs)

cmd_reg_printnames
	fdb	reg_printname_CCR
	fdb	reg_printname_A
	fdb	reg_printname_B
	fdb	reg_printname_DP
	fdb	reg_printname_D
	fdb	reg_printname_X
	fdb	reg_printname_Y
	fdb	reg_printname_U
	fdb	reg_printname_PC
	fdb	0

reg_printname_CCR
	fcn	"CCR"
reg_printname_A
	fcn	"  A"
reg_printname_B
	fcn	"  B"
reg_printname_DP
	fcn	" DP"
reg_printname_D
	fcn	"  D"
reg_printname_X
	fcn	"  X"
reg_printname_Y
	fcn	"  Y"
reg_printname_U
	fcn	"  U"
reg_printname_PC
	fcn	" PC"

;
; cmd_loads
;	Load S-Records.
;
	include	"../lib/s19-loader.exp"
BCall_s19_load_desc
	fcc	BCall_s19_load_bank
	fdb	BCall_s19_load_slot
cmd_loads
	; Push a s19ctx onto the stack.
	leas	-s19ctx_ctxsize,S
	tfr	S,U			; U = s19ctx
	ldx	#cons_getc		; X = cons_getc
	stx	s19ctx_getc,U		; set the s19 getc routine

	; Make sure the jump_addr is invalid in case the load fails.
	ldd	#$FFFF
	std	jump_addr

	jsr	iputs
	fcn	"Waiting for S-Records...\r\n"
	ldy	#BCall_s19_load_desc	; Go load them!
	jsr	brom_call
	bne	1F			; Go handle any error.

	jsr	iputs
	fcn	"Read "
	ldd	s19ctx_nrecs,U	; Print record counts
	jsr	printdec16
	jsr	iputs
	fcn	" records ("
	ldd	s19ctx_ignrecs,U
	jsr	printdec16
	jsr	iputs
	fcn	" ignored)\r\n"

	ldd	s19ctx_addr,U		; Get the entry point
	std	jump_addr		; ...and make it jump'able.

	jsr	iputs
	fcn	"Entry point: "
	jsr	printhex16
	jsr	puts_crlf
	jmp	monitor_main

1	lda	s19ctx_error,U		; Get the error code
	cmpa	#s19_error_data
	beq	2F
	cmpa	#s19_error_abort
	beq	3F

	jsr	error
	jsr	iputs
	fcn	"unknown error\r\n"
	jmp	monitor_main

2	jsr	error
	jsr	iputs
	fcn	"S-Record data error\r\n"
	jmp	monitor_main

3	jsr	iputs
	fcn	"S-Record load aborted.\r\n"
	jmp	monitor_main

;
; cmd_help
;	Get help.
;
BCall_cmd_help_desc
	fcc	BCall_cmd_help_bank
	fdb	BCall_cmd_help_slot
cmd_help
	ldy	#BCall_cmd_help_desc
	jsr	brom_call
	jmp	monitor_main

;
; cmd_oink
;	The dumbest little easter egg.
;
BCall_cmd_oink_desc
	fcc	BCall_cmd_oink_bank
	fdb	BCall_cmd_oink_slot
cmd_oink
	ldy	#BCall_cmd_oink_desc
	jsr	brom_call
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
	rti

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
