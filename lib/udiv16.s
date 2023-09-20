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
; udiv16
;	16-bit unsigned binary division
;
; Arguments --
;	X -- dividend
;	Y -- divisor
;
; Returns -
;	X -- quotient
;	Y -- remainder
;
; Clobbers --
;	None.
;
; Notes --
;	Division by zero results in output X=$FFFF and Y=input X
;
udiv16
	pshs	D,X,Y,CC	; Save registers
	;
	; 6,S	Y (lsb)
	; 5,S	Y (msb)		divisor working copy
	; 4,S	X (lsb)
	; 3,S	X (msb)		dividend working copy
	; 2,S	D (lsb)
	; 1,S	D (msb)	
	; 0,S	CC
	;
	ldb	#16		; Push loop count onto the
	pshs	B		; stack.

	;
	; 7,S	Y (lsb)
	; 6,S	Y (msb)		divisor working copy
	; 5,S	X (lsb)
	; 4,S	X (msb)		dividend working copy
	; 3,S	D (lsb)
	; 2,S	D (msb)	
	; 1,S	CC
	; 0,S	loop count
	;
	M_clrd			; Zero out D

	;
	; Loop 16 times, trying to subtract the divisor at each digit
	; place, forming the quotient 1 bit at a time.  Quotient shifts
	; in as the dividend shifts out.
	;
1	asl	5,S		; shift next divident bit through
	rol	4,S		; into remainder (D), clearing
	rolb			; next quotient bit at bit 0 5,S.
	rola
	cmpd	6,S		; Test if divisor can be subtracted
	blo	2F		; and skip (quotient bit = 0) if not.
	subd	6,S		; It can, so subtract and set the
	inc	5,S		; quotient bit at the corresponding place.
2	dec	,S		; Repeat until all 16 divident bits have
	bne	1B		; been shifted, D holds the remainder.

	;
	; Put the remainder into Y's stack slot.  X's slot has been
	; modified in-place and holds the quotient.
	;
	std	6,S
	leas	1,S		; pop the loop count off the stack
	puls	D,X,Y,CC,PC	; Restore and return
