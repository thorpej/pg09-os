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
; printhex8
;	Prints an 8-bit hexadecimal number
;
; Arguments --
;	A - the value to print
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
printhex8
	pshs	A,X		; Save registers.

	leax	printhex_nybbletab,PCR

	lsra			; A >>= 4
	lsra
	lsra
	lsra
	lda	A,X		; A = nybble character
	jsr	[SysSubr_cons_putc]

	lda	,S		; A = saved argument
	anda	#$0f		; mask off upper nybble
	lda	A,X		; A = nybble character
	jsr	[SysSubr_cons_putc]

	puls	A,X,PC		; Restore and return

printhex_nybbletab
	fcc	"0123456789ABCDEF"

;
; printhex16
;	Prints a 16-bit hexadecimal numbner
;
; Arguments --
;	D - the value to print
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
printhex16
	pshs	D		; Save D
	bsr	printhex8	; Upper byte alredy in A
	tfr	B,A		; Get lower byte (in B) into A
	bsr	printhex8	; Print it.
	puls	D,PC		; Restore and return.
