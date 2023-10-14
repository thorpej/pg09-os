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
; ROM Bank 0 image for the 6809 Playground.
;

	include "../../pg09-system/asm/pg09_defs.s"
	include "../drivers/mc6809/mc6809_defs.s"
	include "../lib/ascii_defs.s"
	include "../lib/asm_macros.inc"

	include "../fixed-ram/pg09-fram.exp"
	include "../fixed-rom/pg09-from.exp"

	include "../sys-api/pg09-os.exp"
	include "../sys-api/file-api.exp"

	setdp	-1	; Disable automatic direct page addressing

	org	BROM_START

;
; Banked call jump table.  The order of this table defines the ABI
; of the module in this ROM bank.
;
	BCall_decl cmd_reg
	BCall_decl cmd_loads
	BCall_decl cmd_help
	BCall_decl cmd_oink
	BCall_decl errorstr_print

;
; Code goes here.
;

	include "../lib/memzero8.s"
	include "../lib/parseeol.s"
	include "../lib/parsehex.s"
	include "../lib/parsews.s"
	include "../lib/parsetbl.s"
	include "../lib/printdec.s"
	include "../lib/printhex.s"
	include	"../lib/puts.s"
	include "../lib/toupper.s"
	include "../lib/udiv16.s"

;
; errorstr_print
;	Print an error string corresponding to a standard
;	(file-api, NHACP) error code.
;
; Arguments --
;	A - error code
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
errorstr_print
	pshs	A,X		; save registers

	tsta			; A == 0?
	beq	20F		; yes, unknown error case

	cmpa	#ELASTERR	; A > ELASTERR?
	bhi	20F		; yes, unknown error case

	deca			; 0 is skipped in the table
	asla			; index to offset
	ldx	#errorstr_tab
	ldx	A,X		; X = pointer to string
	jsr	puts		; print it
	bra	99F		; done!

20	jsr	iputs
	fcn	"unknown error "
	jsr	printdec8

99	puls	A,X,PC		; restore and return

errorstr_tab
	;	0 is skipped
	fdb	errorstr_enotsup
	fdb	errorstr_eperm
	fdb	errorstr_enoent
	fdb	errorstr_eio
	fdb	errorstr_ebadf
	fdb	errorstr_enomem
	fdb	errorstr_eacces
	fdb	errorstr_ebusy
	fdb	errorstr_eexist
	fdb	errorstr_eisdir
	fdb	errorstr_einval
	fdb	errorstr_enfile
	fdb	errorstr_efbig
	fdb	errorstr_enospc
	fdb	errorstr_eseek
	fdb	errorstr_enotdir
	fdb	errorstr_enotempty
	fdb	errorstr_esrch
	fdb	errorstr_ensess
	fdb	errorstr_eagain
	fdb	errorstr_erofs

errorstr_enotsup
	fcn	"operation not supported"
errorstr_eperm
	fcn	"operation not permitted"
errorstr_enoent
	fcn	"requested file does not exist"
errorstr_eio
	fcn	"I/O error"
errorstr_ebadf
	fcn	"bad file descriptor"
errorstr_enomem
	fcn	"out of memory"
errorstr_eacces
	fcn	"access denied"
errorstr_ebusy
	fcn	"resource is busy"
errorstr_eexist
	fcn	"file already exists"
errorstr_eisdir
	fcn	"file is a directory"
errorstr_einval
	fcn	"invalid argument"
errorstr_enfile
	fcn	"too many open files"
errorstr_efbig
	fcn	"file is too large"
errorstr_enospc
	fcn	"out of space"
errorstr_eseek
	fcn	"illegal seek"
errorstr_enotdir
	fcn	"file is not a directory"
errorstr_enotempty
	fcn	"directory is not empty"
errorstr_esrch
	fcn	"no such process or session"
errorstr_ensess
	fcn	"too many sessions"
errorstr_eagain
	fcn	"try again later"
errorstr_erofs
	fcn	"storage object is write-protected"

error
	jsr	iputs
	fcn	"ERROR: "
	rts

syntax_error
	jsr	error
	jsr	iputs
	fcn	"syntax error"

suggest_help
	jsr	iputs
	fcn	" - ? for help\r\n"
	rts

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
	beq	cmd_reg_err_pop1	; Not found, syntax error.
	pshs	A			; push index onto stack

	jsr	parseeol		; consume whitespace, check for EOL
	bne	cmd_reg_printloop	; if EOL, print the one reg

	jsr	parsehex16		; D = new value
	beq	cmd_reg_err_pop2	; Not number, syntax error.
	bvs	cmd_reg_err_pop2	; Overflow, syntax error.
	pshs	D			; squirrel it away on the stack
	jsr	parseeol		; make sure we're at EOL now.
	beq	cmd_reg_err_pop4	; No, syntax error.

	ldy	current_iframe
	ldx	#cmd_reg_iframeoffs	; X = frame offset table
	ldb	2,S			; B = index
	ldb	B,X			; B = frame offset
	leay	B,Y			; Y += register offset in iframe

	ldb	2,S			; B = index (again)
	cmpb	#cmd_reg_iframeoffs16	; Does index say "16-bit value"?
	bhs	1F			; Go deal with it.

	tsta				; Did we overflow 8 bits?
	bne	cmd_reg_err_pop4	; Yes, syntax error.
	ldb	1,S			; B = value
	stb	,Y			; store the value in the iframe.
	bra	2F

1	ldd	,S			; D = value
	std	,Y			; store the value in the iframe
2	leas	4,S			; clean up stack
	rts

cmd_reg_err_pop1
	leas	1,S			; clean up stack
	bra	syntax_error

cmd_reg_err_pop2
	leas	2,S			; clean up stack
	bra	syntax_error

cmd_reg_err_pop4
	leas	4,S			; clean up stack
	bra	syntax_error

cmd_reg_printall
	lda	#9			; push 9 onto the stack for count
	pshs	A
	clr	,-S			; push 0 onto the stack for index

cmd_reg_printloop
	;
	; If we get here, we've pushed 2 bytes onto the stack that
	; we need to clean up before we return (count and index).
	;
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
	leas	2,S			; pop temp stack space
	rts

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
cmd_loads
	; Push a s19ctx onto the stack.
	leas	-s19ctx_ctxsize,S
	tfr	S,U			; U = s19ctx
	ldx	SysSubr_cons_getc	; X = cons_getc
	stx	s19ctx_getc,U		; set the s19 getc routine

	; Make sure the jump_addr is invalid in case the load fails.
	ldd	#$FFFF
	std	jump_addr
	clr	can_continue

	jsr	iputs
	fcn	"Waiting for S-Records...\r\n"
	lbsr	s19_load		; Go load them!
	bne	1F			; Go handle any error.

	jsr	iputs
	fcn	"Read "
	ldd	s19ctx_nrecs,U		; Print record counts
	jsr	printdec16
	jsr	iputs
	fcn	" records ("
	ldd	s19ctx_ignrecs,U
	jsr	printdec16
	jsr	iputs
	fcn	" ignored)\r\n"

	ldd	s19ctx_addr,U		; Get the entry point
	std	jump_addr		; ...and make it jump'able.
	clr	can_continue

	jsr	iputs
	fcn	"Entry point: "
	jsr	printhex16
	jsr	puts_crlf
	bra	4F

1	lda	s19ctx_error,U		; Get the error code
	cmpa	#s19_error_data
	beq	2F
	cmpa	#s19_error_abort
	beq	3F

	jsr	error
	jsr	iputs
	fcn	"unknown error\r\n"
	bra	4F

2	jsr	error
	jsr	iputs
	fcn	"S-Record data error\r\n"
	bra	4F

3	jsr	iputs
	fcn	"S-Record load aborted.\r\n"

4	leas	s19ctx_ctxsize,S	; pop context off stack.
	rts

	include "../lib/s19-loader.exp"
	include "../lib/s19-loader.s"

;
; cmd_help
;	Get help.
;
cmd_help
	; First check for command help.
	jsr	parseeol		; if we are at EOL
	bne	cmd_help_generic	; then generic help it is.

	; Whitespace already consumed by parseeol().  Now look up
	; the command in the help table.
	ldy	#monitor_helptab	; Y = help table
	jsr	parsetbl_lookup		; A = command index
	asla				; index -> offset
	ldy	#monitor_helpjmptab	; Y = help jump table
	jmp	[A,Y]			; go get the help

monitor_helptab
	fcc	'@'+$80			; access memory
	fcc	'C'+$80			; continue from debugger
	fcc	'J'+$80			; jump to address
	fcc	"RESE",'T'+$80		; reset the system
	fcc	"REG",'S'+$80		; registers
	fcc	'R'+$80			; print / set register
	fcc	"LOAD",'S'+$80		; load S-Records
	fcc	"ADDR",'S'+$80		; symbolic addresses
	fcc	"OF",'F'+$80		; power off the system
	fcc	"MOUN",'T'+$80		; mount a file system
	fcc	"UMOUN",'T'+$80		; unmount a file system
	fcc	"FSDEV",'S'+$80		; file system devices
	fcc	0

monitor_helpjmptab
	fdb	cmd_help_access_mem
	fdb	cmd_help_continue
	fdb	cmd_help_jump
	fdb	cmd_help_reset
	fdb	cmd_help_regs
	fdb	cmd_help_reg
	fdb	cmd_help_loads
	fdb	cmd_help_addrs
	fdb	cmd_help_off
	fdb	cmd_help_mount
	fdb	cmd_help_umount
	fdb	cmd_help_fsdevs
	fdb	cmd_help_generic

cmd_help_generic
	jsr	iputs
	fcc	"Available commands:\r\n"
	fcc	"@      - access memory\r\n"
	fcc	"C      - continue from debugger\r\n"
	fcc	"J      - jump to address\r\n"
	fcc	"R      - print / set register\r\n"
	fcc	"LOADS  - load S-Records\r\n"
	fcc	"RESET  - reset the system\r\n"
	fcc	"OFF    - power off the system\r\n"
	fcc	"MOUNT  - mount a file system\r\n"
	fcc	"UMOUNT - unmount a file system\r\n"
	fcc	"?      - help\r\n"
	fcn	"Use '? <cmd>' for additional help.\r\n"
	rts

cmd_help_access_mem
	jsr	iputs
	fcc	"@addr               - print 1 byte\r\n"
	fcc	"@                   - print 1 byte at next address\r\n"
	fcc	"@addr,len           - print len bytes\r\n"
	fcc	"@,len               - print len bytes at next address\r\n"
	fcc	"@addr val [val ...] - set bytes starting at address\r\n"
	fcc	"@ val [val ...]     - set bytes starting at next address\r\n"
	fcc	"@addr,len val       - set len bytes at address to value\r\n"
	fcc	"@,len val           - set len bytes at next address to value\r\n"
	fcn	"Use '? addrs' for a list of symbolic addresses.\r\n"
	rts

cmd_help_continue
	jsr	iputs
	fcc	"C - continue from debugger.\r\n"
	fcn	"This command is only valid after an NMI or breakpoint.\r\n"
	rts

cmd_help_jump
	jsr	iputs
	fcc	"J      - jump to entry point of loaded program\r\n"
	fcn	"J addr - jump to specified address\r\n"
	rts

cmd_help_reg
	jsr	iputs
	fcc	"R         - print all registers\r\n"
	fcc	"R reg     - print single register\r\n"
	fcc	"R reg val - set register to value\r\n"
	fcn	"Use '? regs' for a list of registers.\r\n"
	rts

cmd_help_loads
	jsr	iputs
	fcc	"LOADS - load S-Records\r\n"
	fcc	"S19-style S-Records are loaded from the console.\r\n"
	fcc	"Use 'J' to start loaded program.\r\n"
	fcn	"Use CTRL-C to abort loading.\r\n"
	rts

cmd_help_addrs
	jsr	iputs
	fcc	"Available symbolic addresses:\r\n"
	fcc	"  ROM_BANK_REG\r\n"
	fcc	"  LBRAM0_BANK_REG\r\n"
	fcc	"  LBRAM1_BANK_REG\r\n"
	fcc	"  HBRAM_BANK_REG\r\n"
	fcc	"  CLOCK_SPEED_REG\r\n"
	fcc	"  LBRAM0_START\r\n"
	fcc	"  LBRAM1_START\r\n"
	fcc	"  HBRAM_START\r\n"
	fcc	"  BROM_START\r\n"
	fcc	0
	rts

cmd_help_regs
	jsr	iputs
	fcc	"8-bit registers:\r\n"
	fcc	"  A B CCR DP\r\n\r\n"
	fcc	"16-bit registers:\r\n"
	fcn	"  D X Y U PC\r\n"
	rts

cmd_help_reset
	jsr	iputs
	fcc	"Resets the system by asking the PMU to\r\n"
	fcn	"assert the /RESET signal.\r\n"
	rts

cmd_help_off
	jsr	iputs
	fcc	"Powers off the system by asking the PMU to\r\n"
	fcn	"switch off the ATX power supply.\r\n"
	rts

cmd_help_mount
	jsr	iputs
	fcc	"MOUNT fsdev drive - mount the file system on fsdev at drive\r\n"
	fcc	"Example:\r\n"
	fcc	"  MOUNT UART1 A:\r\n"
	fcn	"Valid drives: A: - "
	lda	#('A' + fs_maxdrives - 1)
	jsr	[SysSubr_cons_putc]
	jsr	iputs
	fcc	":\r\n"
	fcn	"Use '? fsdevs' for a list of file system devices.\r\n"
	rts

cmd_help_umount
	jsr	iputs
	fcc	"UMOUNT drive - unmount the file system at drive\r\n"
	fcc	"Example:\r\n"
	fcn	"  UMOUNT A:\r\n"
	rts

cmd_help_fsdevs
	jsr	iputs
	fcn	"Available file system devices:\r\n"
	ldx	#fs_avail
1	cmpx	#fs_avail_end
	beq	99F

	ldy	,X++		; get fsops pointer
	pshs	X		; remember next slot on stack

	jsr	iputs		; indent
	fcn	"  "

	ldx	fsov_devname,Y
	jsr	puts
	jsr	iputs
	fcn	" ("
	ldx	fsov_fsname,Y
	jsr	puts
	jsr	iputs
	fcn	")\r\n"

	puls	X		; pop next slot off stack
	bra	1B
99	rts

;
; cmd_oink
;	The dumbest little easter egg.
;
cmd_oink
	jsr	iputs
	fcc	"^. .^\r\n"
	fcc	"( @ )\r\n"
	fcn	"OINK!\r\n"
	rts

	org	BROM_START+BROM_SIZE-1
	nop
