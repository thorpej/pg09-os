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
; memcpy16
;	Copy memory, 16-bit length.
;
; Arguments --
;	X - destination
;	Y - source
;	D - byte count (0 == 65536)
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
memcpy16
	pshs	D,X,Y		; Save registers
memcpy16_common
	;
	; 5,S	Y (lsb)
	; 4,S	Y (msb)
	; 3,S	X (lsb)
	; 2,S	X (msb)
	; 1,S	D (lsb)
	; 0,S	D (msb)
	;
	leay	D,X		; Compute ending address
	pshs	Y		; push it onto the stack
	;
	; 7,S	Y (lsb)
	; 6,S	Y (msb)
	; 5,S	X (lsb)
	; 4,S	X (msb)
	; 3,S	D (lsb)
	; 2,S	D (msb)
	; 1,S	end address (lsb)
	; 0,S	end address (msb)
	;
	ldy	6,S		; recover source argument
1	ldb	,Y+		; B = *src++
	stb	,X+		; *dst++ = B
	cmpx	,S		; X == ending address?
	bne	1B		; Nope, keep going
	leas	2,S		; pop ending address off stack
	puls	D,X,Y,PC	; Restore and return

;
; memmove6
;	Copy memory, 16-bit length, buffers may overlap.
;
; Arguments --
;	X - destination
;	Y - source
;	D - byte count (0 == 65536)
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
memmove16
	pshs	D,X,Y		; Save registers
	;
	; 5,S	Y (lsb)
	; 4,S	Y (msb)
	; 3,S	X (lsb)
	; 2,S	X (msb)
	; 1,S	D (lsb)
	; 0,S	D (msb)
	;
	cmpx	4,S		; dst > src?
	bhi	memcpy16_common	; Yes, go copy forwards.

	; Copy backwards.
	leax	D,X		; X += D
	leay	D,Y		; Y += D

1	ldb	,-Y		; B = *--Y
	stb	,-X		; *--X = B
	cmpx	2,S		; X == destination address?
	bne	1B		; Nope, keep going
	puls	D,X,Y,PC	; Restore and return
