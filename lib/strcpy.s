;       
; Copyright (c) 2022 Jason R. Thorpe.
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
; strcpy
;	Copy a NUL-terminated string.
;
; Arguments --
;	X - destination
;	Y - source
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
strcpy
	pshs	A,X,Y		; Save registers
strcpy_strcat_common
1	lda	,Y+		; A = *src++
	beq	1F		; NUL byte? If yes, then finish up.
	sta	,X+		; *dst++ = A
	bra	1B		; Loop.
1	clr	,X		; NUL-terminate destination
	puls	A,X,Y,PC	; Restore and return.

;
; strcat
;	Concatenate NUL-terminated strings.
;
; Arguments --
;	X - destination
;	Y - source
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
strcat
	pshs	A,X,Y		; Save registers
1	lda	,X+		; A = *dst++
	bne	1B		; If not NUL, then loop.
	leax	-1,X		; Point X at the NUL
	bra	strcpy_strcat_common ; Now do the copy.
