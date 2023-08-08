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
; cons_getline --
;	Get a NUL-terminated line of input from the console.
;
; Arguments --
;	None.
;
; Returns --
;	A - Length of the string in the input buffer, not including the
;	terminating NUL.
;
;	U - Address of the buffer containing the line.  The caller may
;	continue to use this buffer until the next call to cons_getline().
;
; Clobbers --
;	None.
;
cons_getline
	pshs	B		; save B

	; Clear the counter (also serves as our index).
	; Set U to point at the line buffer.
	clr	getline_cnt
	ldu	#getline_buf

cons_getline_continue
	; Get a character from the console.
	bsr	cons_getc

	; If we got BS or DEL, then backspace-and-erase.
	cmpa	#ASCII_BS
	beq	cons_getline_bs
	cmpa	#ASCII_DEL
	beq	cons_getline_bs

	; If we got a CTRL-U, then erase to the beginning of the line.
	cmpa	#ASCII_NAK
	beq	cons_getline_nak

	; If we get a CTRL-C, then visibly abort the line.
	cmpa	#ASCII_ETX
	beq	cons_getline_etx

	; If we got CR ($0D), then echo out CR+LF, and the line is done.
	cmpa	#ASCII_CR
	beq	cons_getline_done

	; Check to see if we are at the character limit.  We drop the
	; character if so.
	ldb	getline_cnt
	cmpb	#getline_maxcnt
	beq	cons_getline_continue ; Yup, just drop it.

	; Ok, we can store the character in the line buffer and echo
	; it back.
	sta	,U+
	inc	getline_cnt
	bsr	cons_putc
	bra	cons_getline_continue ; ...and go get another one.

cons_getline_done
	; Line is done.  Emit a CR+LF, get the character count into A,
	; and return.
	clr	,U		; NUL-terminate the string
	lbsr	puts_crlf
	lda	getline_cnt
	ldu	#getline_buf	; Return line buffer in U.
	puls	B,PC		; Restore and return.

cons_getline_bs
	lda	#1		; Deleting 1 character
	pshs	A
	bra	1F

cons_getline_nak
	lda	getline_cnt	; Deleting all characters
	pshs	A

1	; If we're already at the beginning of the line, just go
	; around again.
	tst	getline_cnt
	beq	2F
	; Decrement the byte count, scoot back our index register,
	; and erase the character on the line.
	dec	getline_cnt
	leau	-1,U
	lbsr	iputs
	fcb	ASCII_BS,ASCII_SPACE,ASCII_BS,0
	dec	,S		; decrement delete count
	bne	1B		; Keep going if there's more to do.
2
	leas	1,S		; pop the delete count slot
	bra	cons_getline_continue

cons_getline_etx
	clr	getline_cnt	; Clear out the line buffer.
	ldu	#getline_buf
	jsr	iputs
	fcn	"^C"
	bra	cons_getline_done
