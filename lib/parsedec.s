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
; parsedec
;	Parses an unsigned 16-bit decimal number in ASCII
;
; Arguments --
;	X - pointer to the buffer to be parsed
;
; Returns --
;	D - value of the parsed number.
;
;	X - Updated to point to the first non-numeric character following
;	the number.
;
;	CC_Z is clear if any valid digits were parsed, CC_Z set and the
;	value in D is 0 if no valid digits were found.
;
;	CC_V is set if the number overlowed 16 bits.  The value
;	$FFFF is loaded into D in that case.  If the value fits in
;	16 bits, then CC_V is cleared.
;
; Clobbers --
;	None.
;
parsedec
	;
	; We are going to loop through the digits, one by one,
	; multiply the running total by 10 and adding the new
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
	cmpa	#'0'		; Between '0' and '9'?
	blt	3F
	cmpa	#'9'
	bgt	3F		; nope.
	inc	,S		; increment digit count
	cmpy	#6553		; check for overflow
	bhs	6F
2	suba	#'0'		; convert ASCII to value
	exg	D,Y		; running total into D for multiply
	lbsr	mulDx10		; D = D * 10
	exg	D,Y		; running total back into Y
	leay	A,Y		; Y = Y + A
	bra	1B		; get another digit

3	;
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
	beq	4F		; Z set, no number, just get out.
	bmi	5F		; N set, it's an overflow.
4	tfr	Y,D		; Put running total into D
	puls	Y,PC		; Restore and return

5	ldd	#$FFFF		; Saturate result
	orcc	#CC_V		; Set V flag
	puls	Y,PC		; Restore and return

6	;
	; We have a value >= 6553 in the running total.  We need to
	; check for 3 cases:
	;
	; 1- Value is > 6553, we have no space to multiply by 10, so
	;    there is an overflow.
	;
	; 2- Value is == 6553 and incoming digit is > 5, so there is
	;    an overflow.
	;
	; 3- Value is == 6553 and incoming digit is <= 5, this fits.
	;
	; So we test for the third case and handle it, and just flag
	; an overflow otherwise.  CC is still valid from previous
	; comparison.
	;
	bne	7F		; Not 6553, must be > 6553.  OVERFLOW
	cmpa	#'5'		; Digit <= 5?
	ble	2B		; Yes, go handle it.

7	lda	#$80		; make the digit count negative
	sta	,S
	bra	1B		; keep consuming digits until done.
