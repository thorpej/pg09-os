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
; s19_load
;	Load an S-Record file comprised of S1 and S9 (16-bit) records.
;
; Arguments --
;	U - s19ctx.  The s19ctx_getc must be filled in by the caller.
;
; Returns --
;	CC_Z is set if the error is 0.  The start address will be in the
;	s19ctx_addr field.
;
; Clobbers --
;	Amazingly, none.
;
; Notes --
;	We jump around to handle errors, so Y is used to preserve
;	our "at rest" stack pointer so it can be restored in the
;	error recovery path.
;
s19_load
	pshs	A,B,X,Y		; Save registers.
	tfr	S,Y		; Save our stack pointer in Y
	tfr	U,X		; zero out the context
	lda	#s19ctx_getc	; (except for s19ctx_getc at the end)
	lbsr	memzero8
	;
	; Fall into s19_get_record.  Tail of this function is at
	; s19_load_done below.
	;
s19_get_record
	tfr	Y,S		; Restore stack pointer
	lbsr	s19_getc	; wait for the start-of-record
	cmpa	#'S'
	bne	s19_get_record	; nope, still waiting
	ldd	s19ctx_nrecs,U	; increment record count
	addd	#1
	std	s19ctx_nrecs,U
	lbsr	s19_getc	; now get the type
	cmpa	#'1'		; Check for supported record types
	beq	1F
	cmpa	#'9'
	beq	1F
	ldd	s19ctx_ignrecs,U ; increment ignored record count
	addd	#1
	std	s19ctx_ignrecs,U
	bra	s19_get_record	; wait for the next record

1	sta	s19ctx_rectype,U ; stash the record type
	clr	s19ctx_sum,U	; zero out the checksum
	bsr	s19_get_byte	; get length byte
	cmpa	#3		; 3 is the minimum message length for S19
	blo	s19_error	; it's an error.
	sta	s19ctx_len,U	; store length

	bsr	s19_get_byte	; first byte of address
	sta	s19ctx_addr+0,U
	bsr	s19_get_byte	; second byte of address
	sta	s19ctx_addr+1,U

	lda	s19ctx_rectype,U ; check record type
	cmpa	#'9'		; S9 (termination, 16-bit start address)
	beq	s19_get_s9
	;
	; We are guaranteed above that it's only going to be an S1 or S9
	; record, so we can just fall into S1 handling (which will be the
	; common case).
	;
	ldx	s19ctx_addr,U	; X = destination address
1	bsr	s19_get_byte	; get payload byte
	tst	s19ctx_len,U	; check length
	beq	1F		; 0 -> we just loaded the checksum byte
	tst	s19ctx_error,U	; check for error
	bne	1B		; don't store data if there's been an error
	sta	,X+		; store it in the destination buffer
	bra	1B		; go back around for more

1	bsr	s19_check_sum	; CC_Z is set if checksum is OK
	bne	s19_error	; nope, error
	bra	s19_get_record	; keep getting records

s19_get_s9
	;
	; This is the termination record.  The address is already
	; stashed in the context.  We just need to consume the record
	; and verify the checksum (flagging an error if needed) and
	; then we're done.
	;
1	bsr	s19_get_byte	; get payload byte
	tst	s19ctx_len,U	; check length
	bne	1B		; keep looping until we get to 0
	bsr	s19_check_sum	; CC_Z is set if checksum is OK
	beq	s19_load_done	; Ta-da!
	lda	#s19_error_data	; Boo, error.
	sta	s19ctx_error,U
s19_load_done
	tfr	Y,S		; Restore stack pointer
	tst	s19ctx_error,U	; Set Z if no error.
	puls	A,B,X,Y,PC	; Restore and return

s19_check_sum
	lda	s19ctx_sum,U	; A = sum
	coma			; complement A, CC_Z is set if OK.
	rts

s19_error
	lda	#s19_error_data
	sta	s19ctx_error,U
	bra	s19_get_record	; consume the rest of the data

s19_abort
	lda	#s19_error_abort
	sta	s19ctx_error,U
	bra	s19_load_done

s19_get_byte
	clr	,-S		; make a spot for the result
	bsr	s19_get_nybble	; get the first nybble
	asla
	asla
	asla
	asla			; shift it into place.
	sta	,S		; stash it into temp slot
	bsr	s19_get_nybble	; get the second nybble
	ora	,S		; or in the upper half
	sta	,S		; save it off
	adda	s19ctx_sum,U	; add to running sum
	sta	s19ctx_sum,U
	dec	s19ctx_len,U	; length--
	puls	A,PC		; get result and and return

s19_get_nybble
	bsr	s19_getc	; get the character
	suba	#'0'		; subtract the '0' character
	cmpa	#9		; if it's <= 9, we're done.
	bls	1F
	suba	#('A'-('9'+1))	; adjust for A-F
	cmpa	#$F		; if it's <= $F, we're done.
	bls	1F
	bra	s19_error
1	rts

;
; s19_getc
;	Get a character for the S-Record loader.
;
; Arguments --
;	U - s19ctx
;
; Returns --
;	A - fetched character.
;
; Clobbers --
;	None.
;
; Notes --
;	Only valid S-Record characters are returned.  We toss characters
;	that don't belong in an S-Record stream and keep waiting.  If we
;	get a CTRL-C or a NUL, we abort.
;
s19_getc
	jsr	[s19ctx_getc,U]
	jsr	toupper

	cmpa	#0		; NUL?
	beq	2F		; abort.
	cmpa	#ASCII_ETX	; CTRL-C?
	beq	2F		; abort.

	cmpa	#'S'		; 'S' -> yes!
	beq	1F
	cmpa	#'0'		; < '0', go back around.
	blt	s19_getc
	cmpa	#'9'		; <= '9', yes!
	ble	1F
	cmpa	#'A'		; < 'A', go back around.
	blt	s19_getc
	cmpa	#'F'		; <= 'F', yes!
	ble	1F
	bra	s19_getc	; Anything else, go back around.
1	rts
2	bra	s19_abort
