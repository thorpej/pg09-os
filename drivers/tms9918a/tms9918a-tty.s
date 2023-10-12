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
; Terminal emulation routines for the TMS9918.
;

VDP_TTY_LOCK	macro
		inc	VDP_tty_buf_locked
		endm

VDP_TTY_UNLOCK_REPAINT	macro
		inc	VDP_tty_buf_dirty
		dec	VDP_tty_buf_locked
		endm

;
; The "phantom" row is the row that does not exist just below the bottom
; of the screen.  We allow the row pointer to advance there so we have
; a place to "park" when we fill the final character cell at the bottom
; right -- we don't actually want to scroll in that case until we try to
; put another character into the phantom row.
;
VDP_tty_last_row	equ	(VDP_tty_rows - 1)
VDP_tty_last_col	equ	(VDP_tty_cols - 1)

; Pre-computed table of row addresses inside the character cell buffer.
VDP_tty_rowaddrs
	fdb	VDP_tty_buf +  0*VDP_tty_cols
	fdb	VDP_tty_buf +  1*VDP_tty_cols
	fdb	VDP_tty_buf +  2*VDP_tty_cols
	fdb	VDP_tty_buf +  3*VDP_tty_cols
	fdb	VDP_tty_buf +  4*VDP_tty_cols
	fdb	VDP_tty_buf +  5*VDP_tty_cols
	fdb	VDP_tty_buf +  6*VDP_tty_cols
	fdb	VDP_tty_buf +  7*VDP_tty_cols
	fdb	VDP_tty_buf +  8*VDP_tty_cols
	fdb	VDP_tty_buf +  9*VDP_tty_cols
	fdb	VDP_tty_buf + 10*VDP_tty_cols
	fdb	VDP_tty_buf + 11*VDP_tty_cols
	fdb	VDP_tty_buf + 12*VDP_tty_cols
	fdb	VDP_tty_buf + 13*VDP_tty_cols
	fdb	VDP_tty_buf + 14*VDP_tty_cols
	fdb	VDP_tty_buf + 15*VDP_tty_cols
	fdb	VDP_tty_buf + 16*VDP_tty_cols
	fdb	VDP_tty_buf + 17*VDP_tty_cols
	fdb	VDP_tty_buf + 18*VDP_tty_cols
	fdb	VDP_tty_buf + 19*VDP_tty_cols
	fdb	VDP_tty_buf + 20*VDP_tty_cols
	fdb	VDP_tty_buf + 21*VDP_tty_cols
	fdb	VDP_tty_buf + 22*VDP_tty_cols
	fdb	VDP_tty_buf + 23*VDP_tty_cols
	fdb	FROM_START	; Phantom row - writes here are discarded

VDP_tty_mode_desc
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
; VDP_tty_init
;	Initialize the VDP TTY.
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
VDP_tty_init
	pshs	A,B,X,Y		; Save registers

	; Clear the TTY shadow buffer with spaces.
	ldx	#VDP_tty_buf
	ldy	#VDP_tty_buf_size
	lda	#' '
	jsr	memset16

	; Clear the TTY the state variables.
	ldx	#VDP_tty_zerostart
	lda	#VDP_tty_zerosize
	jsr	memzero8

	; Go do the common bits.
	bra	VDP_tty_init_common

VDP_tty_reinit
	pshs	A,B,X,Y		; Save registers

VDP_tty_init_common
	;
	; Set the mode to our text mode.  This disables the screen and
	; clears the VSYNC handler.
	;
	ldx	#VDP_tty_mode_desc
	jsr	VDP_set_mode

	VDP_TTY_LOCK

	;
	; Zero out the Pattern Table and load our sparse font data.
	;
	ldd	#VDP_TXT_PGBA_DEFAULT
	jsr	VDP_set_address
	ldy	#(256 * 8)
	clra
	jsr	VDP_memset

	ldx	#VDP_tty_font_tiles
	ldy	#VDP_TXT_PGBA_DEFAULT
	lda	#VDP_tty_font_ntiles
	jsr	VDP_load_tiles

	; Set the display position for the current row/column.
	jsr	VDP_tty_setpos

	; Set the current cursor character to the cell character.
	lda	VDP_tty_cursor_savechar
	sta	VDP_tty_cursor_curchar

	;
	; Force a repaint of the display by calling the VSYNC handler.
	;
	VDP_TTY_UNLOCK_REPAINT
	bsr	VDP_tty_vsync

	; Register the VSYNC handler.
	ldx	#VDP_tty_vsync
	jsr	VDP_set_vsync_handler

	; Enable the screen.
	jsr	VDP_screen_enable

	puls	A,B,X,Y,PC	; restore and return

; Macro to erase the cursor and replace it with the character
; below.  Clobbers B.  Assumes VSYNC lock is held.
VDP_TTY_ERASE_CURSOR	macro
			ldb	VDP_tty_cursor_savechar
			stb	[VDP_tty_pos]
			endm

;
; VDP_tty_setpos --
;	Set the TTY buffer pointer for the current row/column.
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
VDP_tty_setpos
	pshs	A,X		; save registers

	ldx	#VDP_tty_rowaddrs ; X = pre-computed row table
	lda	VDP_tty_row	  ; A = row
	asla			  ; table index to offset
	ldx	A,X		  ; load row address from table
	lda	VDP_tty_col	  ; A = column
	leax	A,X		  ; add in column offset
	stx	VDP_tty_pos	  ; store updated position

	; Cache the character at this position now in case the
	; cursor is drawn in this cell.
	lda	,X
	sta	VDP_tty_cursor_savechar

	puls	A,X,PC		; restore and return

;
; VDP_tty_vsync
;	VSYNC handler for text mode.
;
; Arguments --
;	None.
;
; Returns --
;	None.
;
; Clobbers --
;	Yes.
;
VDP_tty_vsync
	;
	; First, check to see if the frame buffer is locked due to
	; being in-flux.  If so, we just get out now and check again
	; on the next VSYNC interrupt.
	;
	lda	VDP_tty_buf_locked
	bne	99F

	;
	; Next, process the blinking cursor.
	;
	; XXX

10	
	;
	; Now check to see if VRAM needs to be updated from the frame
	; buffer.
	;
	lda	VDP_tty_buf_dirty
	beq	99F		; No work to do this frame.

20
	;
	; XXX We could potentially optimize this to only copy a minimal
	; XXX rectangle, but it's kind of pointless once the display
	; XXX starts scrolling.
	;

	; Set the internal pointer to the Name Table.
	ldd	#VDP_TXT_NTBA_DEFAULT
	jsr	VDP_set_address

	; Copy in the current frame buffer image to the Name Table.
	ldx	#VDP_tty_buf
	ldy	#VDP_tty_buf_size
	jsr	VDP_copyin

	clr	VDP_tty_buf_dirty
99	rts

;
; VDP_tty_scroll
;	Scroll down one line.  We assume the VSYNC lock is already held.
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
VDP_tty_scroll
	pshs	A,B,X,Y		; save registers

	; Copy rows 1-23 up one row.
	ldx	#VDP_tty_buf			   ; destination = row 0
	ldy	#(VDP_tty_buf + VDP_tty_cols)	   ; source = row 1
	ldd	#(VDP_tty_buf_size - VDP_tty_cols) ; length = size - 1 row
	jsr	memcpy16

	; Clear row 23.
	ldx	#(VDP_tty_buf + 23*VDP_tty_cols)  ; destination = row 23
	lda	#' '				  ; value = ASCII space
	ldb	#VDP_tty_cols			  ; count = 40
	jsr	memset8

	puls	A,B,X,Y,PC	; restore and return

VDP_tty_putc_jmptab
	fdb	VDP_tty_putc_done	; NUL (CTRL-@)
	fdb	VDP_tty_putc_done	; SOH (CTRL-A)
	fdb	VDP_tty_putc_done	; STX (CTRL-B)
	fdb	VDP_tty_putc_done	; ETX (CTRL-C)
	fdb	VDP_tty_putc_done	; EOT (CTRL-D)
	fdb	VDP_tty_putc_done	; ENQ (CTRL-E)
	fdb	VDP_tty_putc_done	; ACK (CTRL-F)
	fdb	VDP_tty_putc_BEL	; BEL (CTRL-G)
	fdb	VDP_tty_putc_BS		; BS  (CTRL-H)
	fdb	VDP_tty_putc_done	; HT  (CTRL-I)
	fdb	VDP_tty_putc_LF		; LF  (CTRL-J)
	fdb	VDP_tty_putc_done	; VT  (CTRL-K)
	fdb	VDP_tty_putc_done	; FF  (CTRL-L)
	fdb	VDP_tty_putc_CR		; CR  (CTRL-M)
	fdb	VDP_tty_putc_done	; SO  (CTRL-N)
	fdb	VDP_tty_putc_done	; SI  (CTRL-O)
	fdb	VDP_tty_putc_done	; DLE (CTRL-P)
	fdb	VDP_tty_putc_done	; DC1 (CTRL-Q)
	fdb	VDP_tty_putc_done	; DC2 (CTRL-R)
	fdb	VDP_tty_putc_done	; DC3 (CTRL-S)
	fdb	VDP_tty_putc_done	; DC4 (CTRL-T)
	fdb	VDP_tty_putc_done	; NAK (CTRL-U)
	fdb	VDP_tty_putc_done	; SYN (CTRL-V)
	fdb	VDP_tty_putc_done	; ETB (CTRL-W)
	fdb	VDP_tty_putc_done	; CAN (CTRL-X)
	fdb	VDP_tty_putc_done	; EM  (CTRL-Y)
	fdb	VDP_tty_putc_done	; SUB (CTRL-Z)
	fdb	VDP_tty_putc_done	; ESC (CTRL-[)
	fdb	VDP_tty_putc_done	; FS  (CTRL-|)
	fdb	VDP_tty_putc_done	; GS  (CTRL-])
	fdb	VDP_tty_putc_done	; RS  (CTRL-^)
	fdb	VDP_tty_putc_done	; US  (CTRL-_)

;
; VDP_tty_putc
;	Put a character onto the screen.
;
; Arguments --
;	A - the character
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
VDP_tty_putc
	pshs	A,B,X		; save registers

	;
	; Lock out the VSYNC handler and clear the cursor from the
	; frame buffer while we manipulate the display.
	;
	VDP_TTY_LOCK
	VDP_TTY_ERASE_CURSOR	; clobbers B

	;
	; Common case is a printable ASCII character.
	;
	cmpa	#ASCII_SPACE
	bhs	VDP_tty_putc_vis

	;
	; Character value is < $20.  Convert to an offset into the
	; jump table to handle the control character.
	;
	asla
	ldx	#VDP_tty_putc_jmptab
	jmp	[A,X]

VDP_tty_putc_BEL
	; XXX Nothing yet.
	bra	VDP_tty_putc_done

VDP_tty_putc_BS
	;
	; Backspace is pretty straight-forward, with just a couple of
	; checks we need to make:
	;
	; If we're at a column > 0, we just decrement the column and
	; we out.
	;
	; If we're at column 0, we may need to underflow.  If we're
	; already on row 0, do nothing.  Otherwise, we underflow to
	; column 39 and decrement our row.
	;
	lda	VDP_tty_col
	beq	1F		; at column 0, check for underflow

	dec	VDP_tty_col	; decrement column
	jsr	VDP_tty_setpos
	bra	VDP_tty_putc_done

1	lda	VDP_tty_row
	beq	VDP_tty_putc_done ; at row 0, just get out

	lda	#VDP_tty_last_col ; set column to 39
	sta	VDP_tty_col
	dec	VDP_tty_row	  ; decrement row
	jsr	VDP_tty_setpos
	bra	VDP_tty_putc_done

VDP_tty_putc_LF
	;
	; Line Feed is pretty straight-forward, with just one special
	; case we need to check for:
	;
	; If we're in the last row -or- the phantom row, we need to
	; scroll and set ourselves to the last row.  Otherwise, we
	; simply increment the row.
	;
	lda	#VDP_tty_last_row
	cmpa	VDP_tty_row
	bhi	1F		; last_row > row, do the easy thing

	jsr	VDP_tty_scroll
	sta	VDP_tty_row	; A already contains #VDP_tty_last_row
	bra	99F

1	inc	VDP_tty_row
99	jsr	VDP_tty_setpos
	bra	VDP_tty_putc_done

VDP_tty_putc_CR
	;
	; Carriage Return is really easy ... we just reset the column
	; pointer to 0.  But we do need to check if we're on the phantom
	; row; if we are, we go back to row 23.
	;
	lda	#VDP_tty_last_row
	cmpa	VDP_tty_row
	bhs	1F		; last_row >= row, all good
	sta	VDP_tty_row	; reset to last row
1	clr	VDP_tty_col
	jsr	VDP_tty_setpos
	bra	VDP_tty_putc_done

VDP_tty_putc_vis
	;
	; First we need to check if we're in the phantom row.  If so,
	; we need to scroll the display.
	;
	ldb	#VDP_tty_last_row
	cmpb	VDP_tty_row
	bhs	1F		; last_row >= row, all good
	jsr	VDP_tty_scroll
	stb	VDP_tty_row	; B already contains #VDP_tty_last_row
	jsr	VDP_tty_setpos

1	;
	; Store the character at the current position and advance the
	; cursor.
	;
	sta	[VDP_tty_pos]
	lda	VDP_tty_col	; A = current column
	cmpa	#VDP_tty_last_col ; Did we just place into the last column?
	blo	1F		; No, just go increment it.
	clr	VDP_tty_col	; New column = 0
	inc	VDP_tty_row	; Increment row (might now be in phantom row)
	bra	2F
1
	inc	VDP_tty_col
2
	jsr	VDP_tty_setpos	; update cursor position
	;
	; FALLTHROUGH
	;
VDP_tty_putc_done
	VDP_TTY_UNLOCK_REPAINT
	puls	A,B,X,PC	; restore and return
