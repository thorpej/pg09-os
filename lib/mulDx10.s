;
; This is the result of an ad-hoc collaboration between Jason Thorpe
; (@thorpej@mastodon.sdf.org) and Michael Engel (@me_@sueden.social)
; on Mastodon on 1 Aug 2023.  Hooray for the Internet!
;
; This work is in the public domain.
;

;
; mulDx10
;	Multiply the unsigned 16-bit value in the D register by 10.
;	This is an optimized routine specifically tailored for parsing
;	decimal numbers represented in ASCII.
;
; Arguments --
;	D -- the value to be multiplied by 10
;
; Returns -
;	D -- the value, um, multiplied by 10
;
; Clobbers --
;	None.
;
; Notes --
;	This routine uses the distributive property of multiplication:
;
;		X*10 == X*(8 + 2) == X*8 + X*2
;
;	Multiplying by 8 and 2 is, of course, very easy!
;
;	Michael's original version worked by shifting the input value left
;	3 bits and then adding it back in twice.  This original version used
;	ADCA to add in the carry, but I shaved off off a few cycles by using
;	ROLA instead of ADCA.
;
;	Michael's version had faster memory accesses by using DP for
;	accessing scratch space.  However, in the interest of keeping
;	library routines reentrant per Ritter and Boney's original intent,
;	I'm using the stack for that space instead, even though it's a
;	teensy bit slower.
;
;	I then saved a few additional cycles by performing the first
;	shift (multiply by 2, which we were doing anyway), and then
;	stashing that away, so we only have to add in once.  Dave Warker
;	(@dwgumby@gardenstate.social) further suggested another opportunity
;	to save a few bytes and cycles.
;
;	N.B. This routine could be further optimized on the 6309 by
;	using some of the additional instructions in native mode.
;
mulDx10
	aslb			; shift D left one bit...
	rola			; once...

	pshs	D		; (stash it away for later...)

	aslb
	rola			; twice...

	aslb
	rola			; thrice.

	addd	,S++		; Add in the x2 value we stashed earlier
				; and pop it off the stack.

	rts			; done!
