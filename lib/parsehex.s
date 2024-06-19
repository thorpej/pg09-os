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
; parsehex8
;	Parses an unsigned 8-bit hexadecimal number in ASCII
;
; Arguments --
;	X - pointer to the buffer to be parsed
;
; Returns --
;	A - value of the parsed number.
;
;	X - Updated to point to the first non-hex character following
;	the number.
;
;	CC_Z is clear if any valid digits were parsed, CC_Z set and the
;	value in A is 0 if no valid digits were found.
;
;	CC_V is set if the number overflowed 8 bits.  The value
;	$FF is loaded into A in that case.  If the value fits in
;	8 bits, then CC_V is cleared.
;
parsehex8
	pshs	B		; Save B
	bsr	parsehex16	; Parse the number
	beq	1F		; get out now if parsehex16() returne false
	bvs	1F		; ...or if it overflowed (A = $FF already)
	tsta			; Conveniently clears CC_V.
	bne	2F		; If A != 0, we overflowed 8 bits.
	tfr	B,A		; Return value in A.
	andcc	#~CC_Z		; clear CC_Z to return true.

1	puls	B,PC		; Restore and return

2	lda	#$FF
	orcc	#CC_V		; CC_Z is clear by virtue of getting here.
	bra	1B

;
; parsehex16
;	Parses an unsigned 16-bit hexadecimal number in ASCII
;
; Arguments --
;	X - pointer to the buffer to be parsed
;
; Returns --
;	D - value of the parsed number.
;
;	X - Updated to point to the first non-hex character following
;	the number.
;
;	CC_Z is clear if any valid digits were parsed, CC_Z set and the
;	value in D is 0 if no valid digits were found.
;
;	CC_V is set if the number overflowed 16 bits.  The value
;	$FFFF is loaded into D in that case.  If the value fits in
;	16 bits, then CC_V is cleared.
;
; Clobbers --
;	None.
;
parsehex16
	;
	; We are going to loop through the digits, one by one,
	; multiply the running total by 16 and adding the new
	; digit each time.
	;
	; Allocate zeroed space on the stack for the digit count,
	; and initialize Y (the running total) to 0.
	;
	pshs	Y		; save Y (used for running total)
	clra
	pshs	A		; initial digit count is 0
	ldy	#0

	;
	; S,2	Y (lsb)		saved value
	; S,1	Y (msb)		saved value
	; S,0	digit count
	;
1	lda	,X+		; get next character
	cmpa	#'0'		; < '0'?  Done parsing.
	blt	4F
	cmpa	#'9'		; > '9'?  Check letters
	bgt	2F
	suba	#'0'		; convert ASCII to value
	bra	3F		; go add it in.

2	lbsr	toupper		; normalize letter to upper-case
	cmpa	#'A'		; < 'A'?  Done parsing.
	blt	4F
	cmpa	#'F'		; > 'F'?  Done parsing.
	bgt	4F
	suba	#('A' - 10)	; convert ASCII to value

3	inc	,S		; increment digit count
	cmpy	#$1000		; check for overflow
	bhs	7F		; Yup, no room to shift left by 4

	exg	D,Y		; running total into D for multiply

	M_asld			; Shift left by 1...
	M_asld			; ...2...
	M_asld			; ...3...
	M_asld			; ...4.

	exg	D,Y		; running total back into Y
	leay	A,Y		; Y = Y + A
	bra	1B		; get another digit

4	;
	; Ok at this point we have consumed all of the digits.
	; X now points 1 byte past the last thing that wasn't
	; a digit, so walk it back now to point at that item.
	;
	leax	-1,X

	;
	; Check to see if we got a valid result.
	; N.B. TST conveniently clears V for us.
	;
	tst	,S+		; Set CC based on count and pop it from stack.
	beq	5F		; Z set, no number, just get out.
	bmi	6F		; N set, it's an overflow.
5	tfr	Y,D		; Put running total into D
	puls	Y,PC		; Restore and return

6	ldd	#$FFFF		; Saturate result
	orcc	#CC_V		; Set V flag
	puls	Y,PC		; Restore and return

7	;
	; We have a value >= $1000 in the running total.  There is no
	; room to multiply by 16.
	;
	lda	#$80		; make the digit count negative
	sta	,S
	bra	1B		; keep consuming digits until done.
