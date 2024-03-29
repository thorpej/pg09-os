; 
; Copyright (c) 2022, 2023 Jason R. Thorpe.
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
; This is adapted from:
;
;	6809 Machine Code Programming
;	by Dave Barrow
;	1984
;
;	Granada Technical Books
;	Granada Publishing Ltd
;	8 Grafton Street, London W1X 3LA
;
;	ISBN 0 246 12532-2
;
; The following changes have been made:
;
; ==> Tidying up the symbol namespace.
; ==> Minor formatting tweaks, and slight wording tweaks in comments.
; ==> Additional comments added (e.g. to document stack usage, etc.)
;

;
; udiv8
;	8-bit unsigned binary division
;
; Arguments --
;	A -- dividend
;	B -- divisor
;
; Returns -
;	A -- quotient
;	B -- remainder
;
; Clobbers --
;	None.
;
; Notes --
;	Division by zero results in output A=$FF and B=input A
;
udiv8
	pshs	CC,B		; Save registers
	;
	; 1,S	B
	; 0,S	CC
	;
	ldb	#8		; Push loop count onto the
	pshs	B		; stack.

	;
	; 2,S	B
	; 1,S	CC
	; 0,S	loop count
	;
	clrb			; Zero out B

	;
	; Loop 8 times, trying to subtract the divisor at each digit
	; place, forming the quotient 1 bit at a time.  Quotient shifts
	; in as the dividend shifts out.
	;
1	asla			; shift next dividend bit through
	rolb			; into remainder (b), clearning
				; next quotient bit.
	cmpb	2,S		; Test if divisor can be subtracted
	blo	2F		; and skip (quotient bit = 0) if not.
	subb	2,S		; It can, so subtract and set the
	inca			; quotient bit at the corresponding place.
2	dec	,S		; Repeat until all 8 dividend bits
	bne	1B		; been shifted, B holds the remainder.

	;
	; Put the remainder into B's stack slot.
	;
	stb	2,S
	leas	1,S		; pop the loop count off the stack
	puls	CC,B,PC		; Restore and return
