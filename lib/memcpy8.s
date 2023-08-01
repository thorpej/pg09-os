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
; memcpy8
;	Copy memory, 8-bit length.
;
; Arguments --
;	X - destination
;	Y - source
;	A - byte count (0 == 256)
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
memcpy8
	pshs	A,B,X,Y		; Save registers
memcpy8_common
1	ldb	,Y+		; B = *src++
	stb	,X+		; *dst++ = B
	deca			; A--
	bne	1B		; Keep going if more bytes left
	puls	A,B,X,Y,PC	; Restore and return

;
; memmove8
;	Copy memory, 8-bit length, buffers may overlap.
;
; Arguments --
;	X - destination
;	Y - source
;	A - byte count (0 == 256)
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
memmove8
	pshs	A,B,X,Y		; Save registers
	;
	; 5,S	Y (lsb)
	; 4,S	Y (msb)
	; 3,S	X (lsb)
	; 2,S	X (msb)
	; 1,S	B
	; 0,S	A
	;
	cmpx	4,S		; dst > src?
	bhi	memcpy8_common	; Yes, go copy forwards.

	; Copy backwards.
	tsta			; A == 0?
	beq	1F		; Yes, have to add 256
	leax	A,X		; X += A
	leay	A,Y		; Y += A
	bra	2F		; go do copy

1	leax	256,X		; X += 256
	leay	256,Y		; Y += 256

2	ldb	,-Y		; B = *--Y
	stb	,-X		; *--X = B
	deca			; A--
	bne	2B		; Keep going if more bytes left
	puls	A,B,X,Y,PC	; Restore and return
