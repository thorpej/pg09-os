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
; NHACP driver for the W65C51 Asynchronous Communications Interface
; Adapter (ACIA).
;

DACIA_REG_DATA	equ	(UART1_BASE + 0)	; data register
DACIA_REG_SR	equ	(UART1_BASE + 1)	; status register
DACIA_REG_CMD	equ	(UART1_BASE + 2)	; command register
DACIA_REG_CTRL	equ	(UART1_BASE + 3)	; control register

;
; dacia_init --
;	Initialize the ACIA UART.
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
dacia_init
	;
	; We run the ACIA at 115.2K 8N1.  There is an auto-RTS
	; circuit connected to the /IRQ line, so we enable the
	; IRQ output even though we poll for input.
	;
	clr	DACIA_REG_SR		; programmed reset
	lda	#ACIA_CTRL_115200+ACIA_CTRL_RCS+ACIA_CTRL_WL8+ACIA_CTRL_SB1
	sta	DACIA_REG_CTRL
	clr	DACIA_REG_CMD
	rts

;
; dacia_putchar --
;	Output a character on the UART.
;
; Arguments --
;	A - character to the output.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
dacia_putchar
	sta	DACIA_REG_DATA
	;
	; We need to delay to allow the character to drain completely
	; out of the shift register (this is a requirement for the W65C51N
	; parts; the TDRE bit doesn't work and writes to the data register
	; are immediately tranferred to the Tx shift register).
	;
	; We have configured the ACIA for 115.2K bits/sec, which is
	; 14400 bytes/sec, which is .0000694444 seconds/byte, which
	; is 69444.4 nsec/byte, or 69.4444 usec/byte.
	;
	; We have 4 possible CPU speeds:
	; 1MHz	- 1000 nsec cycle
	; 2MHz	- 500 nsec cycle
	; 4MHz	- 250 nsec cycle
	;
	; This means then that we have to burn 70 cycles, 139 cycles,
	; or 278 cycles waiting for the byte to drain.  Based on the
	; following timings:
	;
	;	pshs	A,X		8 cycles
	;	ldx	#dacia_delay_tab	3 cycles
	;	lda	CLOCK_SPEED_REG	5 cycles (extended addressing)
	;	lda	A,X		5 cycles (,R + A indexed addressing)
	; 1	deca			2 cycles
	;	bne	1B		3 cycles
	;	puls	A,X,PC		9 cycles
	;
	; Subtracting the fixed overhead (30 cycles), we have 40, 109,
	; or 248 cycles we need to burn on the inner loop.  The inner
	; loop is 5 cycles long, so that means 8, 22, or 50 loop
	; iterations.
	;
	pshs	A,X			; Save A and X
	ldx	#acia_delay_tab		; X <-- address of dacia_delay_tab
	lda	CLOCK_SPEED_REG		; A <-- cpu speed
	lda	A,X			; loop count from table
1	deca
	bne	1B			; waste some cycles
	puls	A,X,PC			; Restore and return

;
; dacia_pollchar --
;	Poll for a character from the UART.
;
; Arguments --
;	None.
;
; Returns --
;	CC_Z is set if there is no character available, and clear if
;	a character was read from the UART.
;
;	A - Character received from the UART, if available.
;
; Clobbers --
;	None.
;
dacia_pollchar
	lda	DACIA_REG_SR	; clears IRQ (asserts auto-/RTS)
	bita	#ACIA_SR_RDRF	; test Receive Data Register Full
	beq	1F		; not set, return
	lda	DACIA_REG_DATA	; set, return character
	andcc	#~CC_Z		; make sure Z is clear
1	rts

;
; dacia_getchar --
;	Get an input character from the UART.  Blocks until a character
;	is available.
;
; Arguments --
;	None.
;
; Returns --
;	A - Character received from the UART.
;
; Clobbers --
;	None.
;
dacia_getchar
1	lda	DACIA_REG_SR	; clears IRQ (asserts auto-/RTS)
	bita	#ACIA_SR_RDRF	; test Receive Data Register Full
	beq	1B		; not set, check again
	lda	DACIA_REG_DATA	; set, return character
	rts

;
; dacia_fs_mount --
;	Initialize the Data ACIA UART and start an NHACP system session.
;
; Arguments --
;	None.
;
; Returns --
;	A - error code if mount fails (0 == no error, mount succeeded)
;
; Clobbers --
;	None.
;
dacia_fs_mount
	pshs	B,X,Y,U

	; Initialize the UART.
	bsr	dacia_init

	; Initialize our NHACP context.
	ldu	#dacia_nhacp_context
	ldd	#dacia_pollchar
	std	nhctx_pollc,U
	ldd	#dacia_putchar
	std	nhctx_putc,U

	; Start a system session with the server.
	; A contains the error code upon return.
	BCall	"nhacp_start_system_session"

99	puls	B,X,Y,U,PC

;
; dacia_fs_unmount --
;	Shut down the Data ACIA NHACP system session.
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
dacia_fs_unmount
	pshs	A,B,X,Y,U
	ldu	#dacia_nhacp_context
	BCall	"nhacp_end_session"
	lda	#ACIA_CMD_IRDQ		; disable IRQ -> de-assert /RTS
	sta	DACIA_REG_CMD
	puls	A,B,X,Y,U,PC

;
; dacia_file_open --
;	Data ACIA file open routine.
;
; Arguments --
;	X - file_open args
;
; Returns --
;	Error status in file_open args.
;
; Clobbers --
;	None.
;
dacia_file_open
	pshs	U
	ldu	#dacia_nhacp_context
	BCall	"file_nhacp_open"
	puls	U,PC

;
; dacia_file_open --
;	Data ACIA file I/O routine.
;
; Arguments --
;	X - file_io args
;
; Returns --
;	Error status in file_io args.
;
; Clobbers --
;	None.
;
dacia_file_io
	pshs	U
	ldu	#dacia_nhacp_context
	BCall	"file_nhacp_io"
	puls	U,PC

;
; dacia_file_close --
;	Data ACIA file close routine.
;
; Arguments --
;	X - file_close args
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
dacia_file_close
	pshs	U
	ldu	#dacia_nhacp_context
	BCall	"file_nhacp_close"
	puls	U,PC

dacia_fileops
	fdb	dacia_file_open
	fdb	dacia_file_io
	fdb	dacia_file_close

dacia_devname
	fcn	"UART1"

dacia_fsname
	fcn	"NHACP"

dacia_fsops
	fdb	dacia_devname
	fdb	dacia_fsname
	fdb	dacia_fileops
	fdb	dacia_fs_mount
	fdb	dacia_fs_unmount
