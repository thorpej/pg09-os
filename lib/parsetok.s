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
; parsetok
;	Parses a token.  Essentially, skips any leading whitespace,
;	remembers the location of the first non-whitespace character
;	(the beginning of the token), and then skips again until
;	whitespace or EOL is encountered and remembers that location
;	as well.
;
; Arguments --
;	X - pointer to the buffer to be parsed
;
; Returns --
;	X - Updated to point to the first non-whitespace character following
;	any whitespace.
;
;	Y - Updated to point to the first whitespace character (or EOL)
;	after the token.
;
;	CC_Z is clear if a token was encountered, set if one was not.
;
; Clobbers --
;	None.
;
parsetok
	lbsr	parsews		; skip any whitespace (updates X)
	tst	,X		; *X == NUL?
	beq	99F		; yes, get out.

	pshs	A		; Save A, since we use it
	leay	1,X		; Y advances over token looking for WS/EOL.
1	lda	,Y+		; A = *Y++
	beq	2F		; EOL? Done!
	cmpa	#' '		; Space? Done!
	beq	2F
	cmpa	#'\t'		; Tab? Done!
	beq	2F
	bra	1B		; keep scanning.

2	leay	-1,Y		; back up Y to point at first WS/EOL
	andcc	#~CC_Z		; a token was found
	puls	A,PC		; restore and return

99	orcc	#CC_Z
	rts
