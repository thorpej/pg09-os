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
; Base routines for the TMS9918.
;

;
; Display descriptor for the TMS9918.
;
tms9918a_display_descriptor
	;
	; generic fields start here
	;
	fcc	DISPLAY_TYPE_TMS9918A
	fdb	VDP_init
	fdb	VDP_get_tty_info
	fdb	VDP_acquire
	fdb	VDP_release
	;
	; driver-specific fields start here
	;
	fdb	VDP_get_vram_addr	; get VRAM address for table
	fdb	VDP_get_status		; get STATUS register
	fdb	VDP_set_mode		; set video mode
	fdb	VDP_set_vsync_handler	; set VSYNC handler
	fdb	VDP_set_address		; set VRAM write pointer
	fdb	VDP_set_color		; set COLOR register
	fdb	VDP_set_register	; set arbitrary register
	fdb	VDP_screen_enable	; enable the screen
	fdb	VDP_screen_disable	; disable the screen
	fdb	VDP_vram_put		; put a byte to VRAM
	fdb	VDP_clear		; clear all of VRAM
	fdb	VDP_memset		; set a region of VRAM to a value
	fdb	VDP_copyin		; copy data to VRAM
	fdb	VDP_copyin_inv		; copy inverted data to VRAM
	fdb	VDP_load_tiles		; load tile data

; Default to text mode
VDP_default_mode_desc
	fcc	VDP_TXT_R0
	fcc	VDP_TXT_R1
	fcc	VDP_TXT_R2_DEFAULT
	fcc	VDP_TXT_R3_DEFAULT
	fcc	VDP_TXT_R4_DEFAULT
	fcc	VDP_TXT_R5_DEFAULT
	fcc	VDP_TXT_R6_DEFAULT
	fcc	VDP_TXT_R7_DEFAULT
	fdb	VDP_TXT_NTBA_DEFAULT
	fdb	VDP_TXT_CTBA_DEFAULT
	fdb	VDP_TXT_PGBA_DEFAULT
	fdb	VDP_TXT_SATBA_DEFAULT
	fdb	VDP_TXT_SPGBA_DEFAULT

;
; VDP_init
;	Basic initialization of the VDP driver and chip.
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
VDP_init
	pshs	A,B,X,Y		; save registers

	;
	; Set the default VSYNC interrupt handler.
	;
	ldx	#VDP_vsync_handler_default
	stx	VDP_vsync_handler

	;
	; Initialize the RAM vars to have the correct register
	; indices.
	;
	ldx	#VDP_reg_r0+1	; First address (2nd byte of R0's slot)
	lda	#VDP_REG_WRITE	; value to store
1	sta	,X++		; store value, advance to next slot
	inca			; next register index
	cmpa	#VDP_REG_WRITE+8
	bne	1B		; continue if not done

	; Load the default config.
	ldx	#VDP_default_mode_desc
	lbsr	VDP_set_mode

	; Clear out VRAM.
	lbsr	VDP_clear

	; XXX interrupts, bruh

	puls	X,Y,PC		; restore and return

;
; VDP_get_tty_info
;	Get the TTY info array for the VDP.
;
; Arguments --
;	None.
;
; Returns --
;	A - number of entries in array
;	X - pointer to array, $0000 if none
;
; Clobbers --
;	None.
;
VDP_get_tty_info
	clra		; none, for now.
	ldx	#0
	rts

;
; VDP_acquire
;	Acquire the VDP for an application.
;
; Arguments --
;	None.
;
; Returns --
;	Z flag is cleared if acquisition succeeded, set if failed.
;
; Clobbers --
;	None.
;
VDP_acquire
	tst	VDP_acquired	; already busy?
	bne	99F		; yes, get out

	lbsr	VDP_screen_disable ; disable the screen
	pshs	X
	ldx	#$0000		; clear existing VSYNC handler
	bsr	VDP_set_vsync_handler
	puls	X

	inc	VDP_acquired	; set acquired flag, 0 -> 1 clears CC_Z
	rts

99	orcc	#CC_Z		; Z set -> acquisition failed
	rts

;
; VDP_release
;	Release the VDP from an application's use.
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
VDP_release
	clr	VDP_acquired	; just clear the flag unconditionally
	rts

;
; VDP_get_vram_addr
;	Get the VRAM address for the specified table.
;
; Arguments --
;	A - TMS9918_VRAM_ADDR_* constant specifying the table
;
; Returns --
;	X - VRAM address of the specified table.  If the table is
;	unknown, we just return $0000.
;
; Clobbers --
;	A
;
VDP_get_vram_addr
	cmpa	#TMS9918_VRAM_ADDR_SPT
	bls	1F	; valid table specifier
	ldx	#0	; return NULL bogus table
	rts

1	asla		; index to table offset
	ldx	#VDP_Name_Table ; pointer to first VRAM addr var
	ldx	A,X	; X = value in var array
	rts

;
; VDP_get_status
;	Get the VDP status register.
;
; Arguments --
;	None.
;
; Returns --
;	A - status register value
;
; Clobbers --
;	None.
;
VDP_get_status
	lda	VDP_REG_MODE1
	rts

;
; VDP_vsync_handler_default
;	Default NIL handler for the VDP interrupt that does
;	nothing.
;
VDP_vsync_handler_default
	rts

;
; VDP_set_vsync_handler
;	Set the VSYNC interrupt handler.
;
; Arguments --
;	X - address of handler routine.  If X == $0000, then the
;	default routine is installed.
;
; Returns --
;	X - address of previous handler routine
;
; Clobbers --
;	None.
;
VDP_set_vsync_handler
	pshs	CC,Y		; save registers
	cmpx	#$0000		; X == NULL?
	bne	1F		; If yes, use the default handler.
	ldx	#VDP_vsync_handler_default
1	orcc	#CC_I		; dislable IRQ
	ldy	VDP_vsync_handler
	stx	VDP_vsync_handler
	tfr	Y,X		; old handler into X
	puls	CC,Y,PC		; restore and return

;
; VDP_intr
;	VDP interrupt handler.  We call the application-specific
;	display redraw routine, and then handle tasks registered
;	with the 60Hz timer.
;
VDP_intr
	jsr	[VDP_vsync_handler]

	; XXX timer stuff

	rti

;
; VDP_set_address
;	Set the VRAM write pointer.
;
; Arguments --
;	D - VRAM address
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
VDP_set_address
	pshs	A		; save registers
	anda	#$3F		; make sure addrH is tidy
	ora	#VDP_VRAM_WRITE	; set command bit
	stb	VDP_REG_MODE1	; MODE1 <- addrL
	sta	VDP_REG_MODE1	; MODE1 <- addrH + cmd
	puls	A,PC		; restore and return

;
; VDP_vram_put
;	Put a single byte into VRAM.  This isn't done inline because
;	the VRAM is slow-ish, so burning the cycles isn't really a
;	problem.
;
; Arguments --
;	A - Byte to put into VRAM
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
VDP_vram_put
	sta	VDP_REG_MODE0
	rts

;
; VDP_clear
;	Clear all of VRAM.
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
VDP_clear
	pshs	D,Y		; save registers
	M_clrd			; VRAM address = $0000
	bsr	VDP_set_address
				; A is alreaady 0
	ldy	#16384		; length
	bsr	VDP_memset
	puls	D,Y,PC		; restore and return

;
; VDP_memset
;	Set a region of VRAM to a value.  The VRAM address must already
;	be set.
;
;	If you're not doing this in the blanking interval, you
;	should be doing it with the screen disabled.
;
; Arguments --
;	A - the value to set
;	Y - the length
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
; Notes --
;	We assume that we've been called with length > 0 and <= 16384.
;
VDP_memset
	pshs	Y		; save registers
1	bsr	VDP_vram_put	; go put a byte
	leay	-1,Y		; decrement Y
	bne	1B		; not 0, go do more work
	puls	Y		; restore and return

;
; VDP_copyin
;	Copy data into VRAM.  The VRAM address must already be set.
;
; Arguments --
;	X - source buffer
;	Y - length
;
; Returns --
;	None.
;
; Clobbers --
;	X - will point to the byte following the source buffer
;
; Notes --
;	We assume that we've been called with length > 0 and <= 16384.
;
VDP_copyin
	pshs	A,Y		; save registers
1	lda	,X+		; get byte from source buffer
	bsr	VDP_vram_put	; put it into VRAM
	leay	-1,Y		; decrement length
	bne	1B		; != 0, go do more work
	puls	A,Y,PC		; restore and return

;
; VDP_copyin_inv
;	Copy data into VRAM, inverting each byte that is copied.
;	The VRAM address must already be set.
;
; Arguments --
;	X - source buffer
;	Y - length
;
; Returns --
;	None.
;
; Clobbers --
;	X - will point to the byte following the source buffer
;
; Notes --
;	We assume that we've been called with length > 0 and <= 16384.
;
VDP_copyin_inv
	pshs	A,Y		; save registers
1	lda	,X+		; get byte from source buffer
	coma			; invert it
	bsr	VDP_vram_put	; put it into VRAM
	leay	-1,Y		; decrement length
	bne	1B		; != 0, go do more work
	puls	A,Y,PC		; restore and return

;
; VDP_load_tiles
;	Load the pattern table in a sparse fashion.  This is
;	particularly useful for loading ASCII font data or
;	hot-patching sprite tiles.
;
; Arguments --
;	X - sparse tile data
;	Y - pattern table base address
;	A - pattern count (0 == 256)
;
;	The sparse tile data is like so:
;
;	fcc	$20		; tile number
;	fcc	%00000000	; 8 bytes of tile data
;	.
;	.
;	.
;	fcc	%00000000
;
; Returns --
;	None.
;
; Clobbers --
;	X
;
VDP_load_tiles
	pshs	A,B,Y		; save registers

	;
	; 3,S	pattern table address (lsb)
	; 2,S	pattern table address (msb)
	; 1,S	saved B
	; 0,S	pattern count working copy
	;

	;
	; Compute VRAM address of tile and do an
	; inline VDP_set_address().
	;
1	lda	#(VDP_VRAM_WRITE >> 3)
	ldb	,X+		; tile number
	M_asld			; tile number to byte offset
	M_asld
	M_asld
	addd	2,S		; add pattern table address

	stb	VDP_REG_MODE1	; MODE1 <- addrL
	sta	VDP_REG_MODE1	; MODE1 <- addrH + cmd

2	ldb	#8		; put 8 bytes
	lda	,X+		; A = next byte
	bsr	VDP_vram_put	; put byte into VRAM
	decb			; decrement B
	bne	2B		; != 0, copy more pattern bytes

	dec	0,S		; decrement pattern count
	bne	1B		; != 0, load another pattern

	puls	A,B,Y,PC	; restore and return

;
; VDP_screen_enable
;	Enable the screen.
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
VDP_screen_enable
	pshs	A,X		; save registers
	ldx	#VDP_reg_r1	; X = R1 shadow
	lda	#VDP_R1_SCREEN	; A = screen enable bit
	ora	,X		; combine them

VDP_write_register_common
	;
	; We get here with:
	;	A = value to set
	;	X = pointer to the shadow
	; ...and A and X pushed onto the stack.
	;
	sta	,X
	sta	VDP_REG_MODE1
	lda	1,X
	sta	VDP_REG_MODE1
	puls	A,X,PC		; restore and return

;
; VDP_screen_disable
;	Disable the screen.
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
VDP_screen_disable
	pshs	A,X		; save registers
	ldx	#VDP_reg_r1	; X = R1 shadow
	lda	#~VDP_R1_SCREEN	; A = mask out screen enable bit
	anda	,X		; combine them
	bra	VDP_write_register_common

;
; VDP_set_color
;	Set the COLOR register.
;
; Arguments --
;	A - color register value
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
VDP_set_color
	pshs	A,X		; save registers
	ldx	#VDP_reg_color	; X = COLOR shadow
	bra	VDP_write_register_common

;
; VDP_set_register
;	Set an arbitrary VDP register.  Use with extreme
;	caution as there are no guard rails here.
;
; Arguments --
;	A - value to program into register
;	B - register number
;
; Returns --
;	None.
;
; Clobbers --
;	B
;
VDP_set_register
	pshs	A,X		; save registers
	andb	#7		; make sure it's a valid register number
	aslb			; number to shadow table offset
	ldx	#VDP_reg_r0	; X = base of shadow register table
	leax	B,X		; add in offset
	bra	VDP_write_register_common

;
; VDP_set_mode
;	Set the video mode described by the video mode descriptor.
;
; Arguments --
;	X - pointer to mode descriptor
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
; Notes --
;	The screen will be disabled and the VSYNC handler reset after
;	switching modes.  The caller must explicitly re-register the
;	VSYNC handler and re-enable the screen.
;
VDP_set_mode
	pshs	A,B,X,Y,U	; save registers

	; Before we switch modes, reset the VSYNC handler.
	; Each handler is, by definition, mode-specific.
	ldx	#$0000		; indicate the default handler
	lbsr	VDP_set_vsync_handler
	ldx	2,S		; get X argument back

	ldy	#VDP_reg_r0	; Y = pointer to shadows

	lda	,X+		 ; A = R0 from mode descriptor
	anda	#VDP_R0_MODEMASK ; mask out invalid bits
	sta	,Y++		 ; store in the shadow R0

	lda	,X+		 ; A = R1 from the mode descriptor
	anda	#VDP_R1_MODEMASK ; mask out invalid bits
	ora	#VDP_R1_IE+VDP_R1_16K ; set mandatory bits
	sta	,Y++		 ; store in the shadow R1

	ldb	#6		; Now copy the remaining 6 directly.
1	lda	,X+
	sta	,Y++
	decb
	bne	1B

	ldb	#5		; Now copy the 5 VRAM addresses
1	ldu	,X++
	stu	,Y++
	decb
	bne	1B

1	ldb	#16		; Now push the shadow registers into
	ldx	#VDP_reg_r0	; the hardware.
	lda	,X+
	sta	VDP_REG_MODE1
	decb
	bne	1B

	puls	A,B,X,Y,U,PC	; restore and return
