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
; puts
;	Like C's puts(3), except this one doesn't automatically put the
;	\r\n at the end.
;
; Arguments --
;	X - Pointer to a NUL-terminated string.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
puts
	pshs	X		; Save X
	bsr	puts_adv	; go do the work
	puls	X,PC		; Restore and return

;
; puts_adv
;	Like puts(), bit advances X past the end of the string to
;	the byte following the NUL-terminator.
;
; Arguments --
;	X - Pointer to a NUL-terminated string.
;
; Returns --
;	X - Advanced past the string.
;
; Clobbers --
;	None.
;
puts_adv
	pshs	A		; Save A
1	lda	,X+		; A = *X++
	beq	1F		; Get out if we hit the NUL.
	jsr	cons_putc	; print the character
	bra	1B		; go back around
1	puls	A,PC		; Restore and return

;
; puts_crlf
;	Puts a "\r\n" on the console.  This is just a convenience routine.
;
; Arguments --
;	None.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
puts_crlf
	bsr	iputs		; Use iputs() to do the work.
	fcn	"\r\n"
	rts

;
; iputs
;	"Immediate" puts().  The string immediately follows the call
;	to iputs(), and the return address is fixed up to advance it
;	beyond the string.
;
; Arguments --
;	[S] contains the return address, i.e. the pointer to
;	the immediate NUL-terminated string in the insn stream.
;
; Returns --
;	The return address is advanced beyond the end of the string.
;
; Clobbers --
;	None.
;
iputs
	pshs	X		; Save X
	ldx	2,S		; Get pointer to string into X.
	bsr	puts_adv	; Go do the work.
	stx	2,S		; Put advanced pointer into return address.
	puls	X,PC		; Restore and return.
