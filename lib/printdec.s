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
; printdec8
;	Prints an unsigned 8-bit decimal number
;
; Arguments --
;	A - number to be printed
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
printdec8
	pshs	A,B		; Save registers
	tfr	A,B		; B is lsb of 16-bit A
	clra			; msb is 0
	bsr	printdec16	; Go print it.
	puls	A,B,PC		; Restore and return.

;
; printdecs8
;	Prints an signed 8-bit decimal number
;
; Arguments --
;	A - number to be printed
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
printdecs8
	pshs	A,B		; Save registers
	tfr	A,B		; B is lsb of 16-bit A
	sex			; Sign-extend into A
	bsr	printdecs16	; Go print it.
	puls	A,B,PC		; Restore and return.

;
; printdec16
;	Prints an unsigned 16-bit decimal number
;
; Arguments --
;	D - number to be printed
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
; Notes --
;	All the work is done in printdecs() (signed version).  See that
;	routine for all the gory details.  All we're doing is setting
;	up the stack the same way that printdecs() expects it, and then
;	jumping into the the middle (to skip the handling of negative
;	numbers).
;
printdec16
	pshs	D,X,Y,U		; Save registers
	pshs	D		; Save additional woking copy of D
	bra	printdec16_common

;
; printdecs16
;	Prints a signed 16-bit decimal number
;
; Arguments --
;	D - number to be printed
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
printdecs16
	;
	; The basic algorithm here is to iterate over the digits,
	; starting with the least-significant and extracting them
	; using divide and modulo, pushing their characters onto
	; the stack, then passing a pointer to that stack buffer
	; to puts().
	;
	pshs	D,X,Y,U		; Save registers

	;
	; Push another (working) copy of D (the push above is
	; just to avoid clobbers).
	;
	pshs	D

	;
	; 9,S	U (lsb)
	; 8,S	U (msb)
	; 7,S	Y (lsb)
	; 6,S	Y (msb)
	; 5,S	X (lsb)
	; 4,S	X (msb)
	; 3,S	D (lsb)
	; 2,S	D (msb)		saved copy
	; 1,S	D (lsb)
	; 0,S	D (msb)		working copy
	;

	;
	; Before we begin, we need to know if the number is negative.
	; If so, we negate it.  The easiest way for us to do that is
	; to load D with 0 and then subtract the argument (at 0,S) from
	; D.
	;
	; The argument is still in D, so we can TSTA, which is smaller
	; and faster.
	;
	tsta			; argument negative?
	bpl	printdec16_common ; Nope...

	if	CONFIG_6309
	negd			; D = 0 - D
	else
	M_clrd			; D = 0
	subd	0,S		; D -= argument
	endif	; CONFIG_6309
	std	0,S		; Store it back.

	lda	#'-'		; print a unary - character
	jsr	[SysSubr_cons_putc]

printdec16_common
	;
	; Allocate scratch space on the stack:
	; ==> 2 bytes to store the value 10, the divisor argument to udiv16().
	; ==> 1 byte to NUL-terminate the buffer.
	;
	; BUT JASON, YOU DON'T NEED ANOTHER NUL BYTE TO TERMINATE THE
	; BUFFER BECAUSE YOU ALREADY HAVE THE ZERO MSB OF A 16-BIT 10!
	;
	; While that may be true, I also want to have a 16-bit zero
	; in order to easily detect that the quotient is zero by using
	; the cmpx instruction after calling udiv16().
	;
	leas	-3,S
	clr	,S		; NUL-terminate the buffer.
	ldd	#10		; Store the value 10...
	std	1,S		; ...for use later.
	leau	,S		; U = S .. points to scratch area

	;
	; Scratch area now looks like this:
	;
	; 4,U
	; 3,U	D	working copy
	; 2,U	10	lsb of 16-bit value 10
	; 1,U	0	msb if 16-bit value 10
	; 0,U	0	NUL string terminator
	;
	ldx	3,U		; X = arg
1	ldy	1,U		; Y = 10
	lbsr	udiv16		; X = X / 10, Y = X % 10
	tfr	Y,D		; D = Y (B = LSB -- 0...9)
	addb	#'0'		; Convert to ASCII
	pshs	B		; Push digit onto stack
	cmpx	,U		; X == 0?
	bne	1B		; Nope, keep going

	;
	; S now points to the ASCII decimal digits of the number
	; to be printed.
	;
	leax	,S		; X = S
	lbsr	puts		; Print it!

	leas	5,U		; toss the scratch area and string buffer
	puls	D,X,Y,U,PC	; Restore and return
