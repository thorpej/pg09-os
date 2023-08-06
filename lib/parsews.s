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
; parsews
;	Parses whitespace.  Essentially, skips over it and reports
;	if it was present.
;
; Arguments --
;	X - pointer to the buffer to be parsed
;
; Returns --
;	X - Updated to point to the first non-whitespace character following
;	the whitespace.
;
;	CC_Z is clear if whitespace was encountered, set if it is not.
;
; Clobbers --
;	None.
;
parsews
	pshs	A		; Save A, since we use it
	clr	,-S		; Push a zero onto the stack.

	;
	; Loop through and continue until we get to a non-whitespace
	; character.
	;
1	lda	,X+		; A = *X++
	cmpa	#' '		; Is it a space?
	beq	3F		; Yes, count it!
	cmpa	#'\t'		; Is it a tab?
	beq	3F		; Yes, count it!

2	leax	-1,X		; Back X up to point to the last non-WS char
	tst	,S+		; Set Z based on WS character count and pop
	puls	A,PC		; restore and return

3	inc	,S		; Count the character.
	bra	1B		; Go back around.
