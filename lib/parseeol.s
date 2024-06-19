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
; parseeol
;	Parses and end-of-line.  This skips trailing whitespace and then
;	ensures that there is nothing else after it -- just a NUL.
;
; Arguments --
;	X - pointer to the buffer to be parsed
;
; Returns --
;	X - Updated to point to the first non-whitespace character following
;	the whitespace.
;
;	CC_Z is clear if only whitespace was encountered before EOL,
;	set if it is not.
;
; Clobbers --
;	None.
;
parseeol
	lbsr	parsews		; skip the whitespace.
	tst	,X		; *X == NUL?
	bne	1F		; If not, return error;
	andcc	#~CC_Z		; clear Z to return true.
	rts

1	orcc	#CC_Z		; set Z to return false.
	rts
