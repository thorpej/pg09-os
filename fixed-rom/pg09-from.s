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

	include "../fixed-ram/pg09-fram.exp"

	setdp	-1	; Disable automatic direct page addressing

	;
	; System SUBROUTINE jump table.  These are called like normal
	; subroutines, not with SWI as with a system call, i.e.:
	;
	;	jsr	[SysSubr_brom_call]
	;
SysSubr		macro
		export	SysSubr_\1
SysSubr_\1	fdb	\1
		endm

	;
	; DO NOT CHANGE THE ORDER OR THE ORIGIN OF THESE STATEMENTS UNLES
	; YOU KNOW EXACTLY WHAT YOU ARE DOING!  THESE ARE PART OF THE OS ABI!
	;
	org	FROM_START

	SysSubr	brom_call
	SysSubr brom_switch
	SysSubr	lbram_switch
	SysSubr	hbram_switch

pg09os_hello
	fcc	"@thorpej's 6809 Playground OS, version "
	fcc	"0.1"		; Change version number here!
	fcn	"\r\n"

fixed_rom_start
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

1	bra	1B			; hard hang for now.

	;
	; Library routines
	;
	include "../lib/memzero.s"
	include "../lib/printhex.s"
	include "../lib/puts.s"

	;
	; Device drivers.
	;
	include "../drivers/cons/cons.s"
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
;		2,Y	bank number
;		0,Y	system address of routine jump table entry
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
	lda	2,Y		; A = target bank #
	ldy	,Y		; Y = address of jump table slot
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

vec_reset	equ	fixed_rom_start

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
