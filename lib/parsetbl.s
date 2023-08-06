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
; Parse table subroutines.
;

;
; parsetbl_lookup
;	Look up an entry in a parse table.  This can also be used as
;	a strcmp()-like routine, by scanning a table with one entry
;	and checking for a result of 0.
;
;	Each table entry is terminated by its final character having
;	the most-significant bit set, e.g. 'c'+$80.  The table itself
;	is terminated with a NUL.
;
;	Table lookups are case-insensitive; all input characters are
;	normalized to upper-case.  Obviously this means the table
;	entries themselves must be in upper-case.
;
;	Example table:
;
;	Keywords
;		fcc	"FO",'O'+$80		; FOO
;		fcc	"BA",'R'+$80		; BAR
;		fcc	"OIN",'K'+$80		; OINK
;		fcc	0			; <eot>
;
; Arguments --
;	X - Pointer to the string used for lookup.
;
;	Y - Address of the parse table.
;
; Returns --
;	A - Lookup result, either the index of the (first) matching entry
;	    or 1 beyond the last entry (this is guaranteed -- you can rely
;	    on this for having e.g. an error routine jump table slot).
;
;	X - If a match was found, X is updated to point to the character
;	    after the matching string.
;
;	CC_Z is cleared upon success, set upon failure ("zero entries
;	matched").
;
; Notes --
;	B is the offset from the cursor base while parsing.
;
parsetbl_lookup
	pshs	A,B,Y		; save registers
	;
	; S,3	Y (lsb)
	; S,2	Y (msb)		saved
	; S,1	B		saved
	; S,0	A		returned index
	;
	clr	,S		; Initialize result
	clrb			; Initialize cursor offset

1	tst	,Y		; End of table?
	beq	6F		; yes, get out.

	lda	B,X		; A = input byte
	lbsr	toupper		; Normalize to upper-case
	cmpa	,Y		; A == table byte?
	bne	2F		; No...
	incb			; Yes, advance cursor...
	leay	1,Y		; ...and table pointer
	bra	1B		; Keep checking.

2	adda	#$80		; Set upper bit of A
	cmpa	,Y		; Does it match now?
	beq	4F		; Yes! We have a match!

3	tst	,Y+		; skip the rest of this table entry
	bpl	3B

	inc	,S		; increment the table result index
	clrb			; reset cursor offset
	bra	1B		; Try the next table entry

4	incb			; Advance cursor offset past last matching...
	leax	B,X		; ...character and update cursor.

	andcc	#~CC_Z		; match -- clear Z

5	puls	A,B,Y,PC	; Restore and return

6	orcc	#CC_Z		; no match -- set Z
	bra	5B
