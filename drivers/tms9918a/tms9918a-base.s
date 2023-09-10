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
	fcc	DISPLAY_TYPE_TMS9918A	; generic field
	fdb	VDP_init		; generic field
	fdb	VDP_get_tty_info	; generic field
	fdb	VDP_get_vram_addr	; driver-specific fields start here
	fdb	VDP_mode_switch
	fdb	VDP_set_vsync_handler
	fdb	VDP_set_address
	fdb	VDP_set_color
	fdb	VDP_screen_enable
	fdb	VDP_screen_disable
	fdb	VDP_vram_put
	fdb	VDP_memset
	fdb	VDP_copyin
	fdb	VDP_nt_copyin
	fdb	VDP_ct_copyin
	fdb	VDP_pt_copyin
	fdb	VDP_sat_copyin
	fdb	VDP_spt_copyin

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
	ldb	8		; count
1	sta	,X++		; store value, advance to next slot
	inca			; next register index
	decb			; decrement count
	bne	1B		; continue if not done

	; Load the default config.
	ldx	#VDP_default_mode_desc
	lbsr	VDP_mode_switch

	; Clear out VRAM.
	ldx	#0		; VRAM address
	ldy	#16384		; length
	clra			; value to set
	bsr	VDP_memset

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
;	Set the VRAM address.
;
; Arguments --
;	A - read or write command
;	X - VRAM address
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
VDP_set_address
	pshs	A,B		; save registers
	;
	; 1,S	saved B
	; 0,S	saved A (and also command)
	;
	tfr	X,D		; get address into D
	stb	VDP_REG_MODE1	; MODE1 <- addrL
	anda	#$3F		; make sure addrH is tidy
	ora	0,S		; add in command bit
	sta	VDP_REG_MODE1	; MODE1 <- addrH + cmd
	puls	A,B,PC		; restore and return

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
; VDP_memset
;	Set a region of VRAM to a value.
;
;	If you're not doing this in the blanking interval, you
;	should be doing it with the screen disabled.
;
; Arguments --
;	X - the VRAM address
;	Y - the length
;	A - the value to set
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
	pshs	A,B,Y		; save registers
	lda	#VDP_VRAM_WRITE
	bsr	VDP_set_address	; set write pointer in VDP
	puls	A		; pop A off stack
1	bsr	VDP_vram_put	; go put a byte
	leay	-1,Y		; decrement Y
	bne	1B		; not 0, go do more work
	puls	B,Y		; restore and return

;
; VDP_copyin
;	Copy data into VRAM.
;
; Arguments --
;	X - VRAM destination address
;	Y - source buffer
;	D - length
;
; Returns --
;	None.
;
; Clobbers --
;	X, Y
;
; Notes --
;	We assume that we've been called with length > 0 and <= 16384.
;
VDP_copyin
	pshs	D		; push length onto stack
VDP_copyin_common
	lda	#VDP_VRAM_WRITE	; write command
	bsr	VDP_set_address	; set the VRAM address
	ldx	0,S		; length into X
1	lda	,Y+		; get byte from source buffer
	bsr	VDP_vram_put	; put it into VRAM
	leax	-1,X		; decrement X
	bne	1B		; not 0, go do more work
2	puls	D,PC		; restore and return

;
; VDP_nt_copyin
;	Convenience routine that copies data to the Name Table.
;
; Arguments --
;	X - Name Table offset
;	Y - source buffer
;	D - length
;
; Returns --
;	None.
;
; Clobbers --
;	X, Y
;
VDP_nt_copyin
	pshs	D		; push length onto stack
	ldd	VDP_Name_Table	; D = VRAM address of Name Table
	leax	D,X		; compute absolute VRAM address
	bra	VDP_copyin_common

;
; VDP_ct_copyin
;	Convenience routine that copies data to the Color Table.
;
; Arguments --
;	X - Color Table offset
;	Y - source buffer
;	D - length
;
; Returns --
;	None.
;
; Clobbers --
;	X, Y
;
VDP_ct_copyin
	pshs	D		; push length onto stack
	ldd	VDP_Color_Table	; D = VRAM address of Color Table
	leax	D,X		; compute absolute VRAM address
	bra	VDP_copyin_common

;
; VDP_pt_copyin
;	Convenience routine that copies data to the Pattern Table.
;
; Arguments --
;	X - Pattern Table offset
;	Y - source buffer
;	D - length
;
; Returns --
;	None.
;
; Clobbers --
;	X, Y
;
VDP_pt_copyin
	pshs	D		; push length onto stack
	ldd	VDP_Pattern_Table ; D = VRAM address of Pattern Table
	leax	D,X		; compute absolute VRAM address
	bra	VDP_copyin_common

;
; VDP_sat_copyin
;	Convenience routine that copies data to the Sprite Attribute Table.
;
; Arguments --
;	X - Sprite Attribute Table offset
;	Y - source buffer
;	D - length
;
; Returns --
;	None.
;
; Clobbers --
;	X, Y
;
VDP_sat_copyin
	pshs	D		; push length onto stack
	ldd	VDP_Sprite_Attribute_Table ; D = VRAM address of SAT
	leax	D,X		; compute absolute VRAM address
	bra	VDP_copyin_common

;
; VDP_spt_copyin
;	Convenience routine that copies data to the Sprite Pattern Table.
;
; Arguments --
;	X - Sprite Pattern Table offset
;	Y - source buffer
;	D - length
;
; Returns --
;	None.
;
; Clobbers --
;	X, Y
;
VDP_spt_copyin
	pshs	D		; save D temporarily
	ldd	VDP_Sprite_Pattern_Table ; D = VRAM address of SPT
	leax	D,X		; compute absolute VRAM address
	bra	VDP_copyin_common

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
; VDP_mode_switch
;	Switch to the video mode described by the video mode descriptor.
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
VDP_mode_switch
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
	sta	,X+
	decb
	bne	1B

	puls	A,B,X,Y,U,PC	; restore and return
