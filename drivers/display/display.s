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
; Abstract display driver for the 6809 Playground.
;
; Applications that use grpahics modes will, by in large, still have to
; know what kind of display they're talking to, but this abstraction
; gives them a way to ask what kind it is in a generic way and get a
; pointer to a table of the various routines exported by that display
; driver (each of which has its own ABI).
;

;
; display_init --
;	Initialize the display driver.
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
display_init
	pshs	D,X,Y		; save registers

	clr	display_count	; display_count = 0
	clr	display_default
	dec	display_default	; display_default = -1

	ldx	#display_desctab
1	ldy	,X++		; look at next table slot
	beq	2F		; 0? Done counting.
	inc	display_count
	jsr	[disp_init,Y]	; Initialize the display.
	bra	1B		; keep counting

2	; If there is only one display, then it is the default.
	lda	display_count
	cmpa	#1
	bne	99F		; != 1, punt!
	clr	display_default	; display_default = 0

99	puls	D,X,Y,PC	; restore and return

display_desctab
	if	CONFIG_DISPLAY_TMS9918A
	fdb	tms9918a_display_descriptor
	endif
	fdb	$0000		; NULL-terminate

;
; display_get_count --
;	Get the number of display controllers connected to the system.
;
; Arguments --
;	None.
;
; Returns --
;	A - number of connected displays.
;
; Clobbers --
;	None.
;
display_get_count
	lda	display_count
	rts

;
; display_get_default
;	Get the index of the default display.  The default display
;	is where console output goes (in addition to the console UART).
;
; Arguments --
;	None.
;
; Returns --
;	A - index of the default display, -1 if there is no default display.
;
; Clobbers --
;	None.
;
display_get_default
	lda	display_default
	rts

;
; display_get_descriptor
;	Get the display descriptor for the specified display.
;
; Arguments --
;	A - display index
;
; Returns --
;	X - pointer to the display descriptor, $0000 if index is out of
;	range.
;
; Clobbers --
;	None.
;
display_get_descriptor
	cmpa	display_count	; index within range?
	blo	1F		; yes.
	ldx	#$0000		; nope.
	rts

1	pshs	A		 ; save registers
	asla			 ; index to table offset
	ldx	#display_desctab ; X = address of display descriptor table
	ldx	A,X		 ; X = pointer to descriptor from table
	puls	A,PC		 ; restore and return

;
; display_acquire --
;	Acquire a display device for use by an application.
;
; Arguments --
;	A - display index
;
; Returns --
;	Z flag is cleared if successful, set if not successful.
;
; Clobbers --
;	None.
;
display_acquire
	pshs	X		; save registers
	bsr	display_get_descriptor
	beq	99F		; get out if NULL
	jsr	[disp_acquire,X] ; call the acquire routine
99	puls	X,PC		; restore and return

;
; display_release --
;	Release a display that's been in use by an application.
;
; Arguments --
;	A - display index
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
display_release
	pshs	X		; save registers
	bsr	display_get_descriptor
	beq	99F		; get out if NULL
	jsr	[disp_release,X] ; call the release routine
99	puls	X,PC		; restore and return
