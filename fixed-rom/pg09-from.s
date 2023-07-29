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

	org	FROM_START

; Put the standard kernel stack at the top of FRAM.
KSTACK_TOP	equ	FRAM_START+FRAM_SIZE

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
	jsr	iputs
	fcn	"@thorpej's 6809 Playground\r\n"

1	bra	1B			; hard hang for now.

	;
	; Library routines
	;
	include "../lib/memzero.s"
	include "../lib/puts.s"

	;
	; Device drivers.
	;
	include "../drivers/cons/cons.s"
	include "../drivers/w65c51/w65c51.s"

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
