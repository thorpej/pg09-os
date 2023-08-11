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

	include "../sys-api/pg09-os.exp"

	setdp	-1	; Disable automatic direct page addressing

	org	BROM_START

;
; Banked call jump table.  The order of this table defines the ABI
; of the module in this ROM bank.
;
	BCall_decl cmd_help
	BCall_decl cmd_oink
	BCall_decl s19_load

;
; Code goes here.
;

	include "../lib/memzero8.s"
	include "../lib/parseeol.s"
	include "../lib/parsews.s"
	include "../lib/parsetbl.s"
	include	"../lib/puts.s"
	include "../lib/toupper.s"

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
	fcc	'J'+$80			; jump to address
	fcc	"REG",'S'+$80		; registers
	fcc	'R'+$80			; print / set register
	fcc	"LOAD",'S'+$80		; load S-Records
	fcc	"ADDR",'S'+$80		; symbolic addresses
	fcc	0

monitor_helpjmptab
	fdb	cmd_help_access_mem
	fdb	cmd_help_jump
	fdb	cmd_help_regs
	fdb	cmd_help_reg
	fdb	cmd_help_loads
	fdb	cmd_help_addrs
	fdb	cmd_help_generic

cmd_help_generic
	jsr	iputs
	fcc	"Available commands:\r\n"
	fcc	"@     - access memory\r\n"
	fcc	"J     - jump to address\r\n"
	fcc	"R     - print / set register\r\n"
	fcc	"LOADS - load S-Records\r\n"
	fcc	"?     - help\r\n"
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
	fcc	"  LBRAM_BANK_REG\r\n"
	fcc	"  HBRAM_BANK_REG\r\n"
	fcc	"  CLOCK_SPEED_REG\r\n"
	fcc	"  LBRAM_START\r\n"
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
