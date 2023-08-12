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
; Console driver for the W65C51 Asynchronous Communications Interface
; Adapter (ACIA).
;

ACIA_REG_DATA	equ	(UART0_BASE + 0)	; data register
ACIA_REG_SR	equ	(UART0_BASE + 1)	; status register
ACIA_REG_CMD	equ	(UART0_BASE + 2)	; command register
ACIA_REG_CTRL	equ	(UART0_BASE + 3)	; control register

; Status register
; N.B. any write to the status register performs a programmed reset.
; Also, the TDRE bit is completly useless on the W65C51N.  See the
; data sheet for more information.
ACIA_SR_PE	equ	(1 << 0)		; parity error
ACIA_SR_FE	equ	(1 << 1)		; framing error
ACIA_SR_OVR	equ	(1 << 2)		; overrun occurred
ACIA_SR_RDRF	equ	(1 << 3)		; Rx data register full
ACIA_SR_TDRE	equ	(1 << 4)		; Tx data register empty
ACIA_SR_DCDB	equ	(1 << 5)		; 0 = DCD asserted
ACIA_SR_DSRB	equ	(1 << 6)		; 0 = DSR asserted
ACIA_SR_IRQ	equ	(1 << 7)		; IRQ pending

; Command register
; The upper 3 bits of the command register should always be zero
; (parity mode selection on older parts, not supported on W65C51N).
ACIA_CMD_DTR		equ	(1 << 0)	; 1 = assert DTR (DTRB low)
ACIA_CMD_IRDQ		equ	(1 << 1)	; 1 = IRQ disabled
ACIA_CMD_RTSB_H		equ	(0 << 2)	; de-assert RTS
;				(1 << 2)	; do not use
ACIA_CMD_RTSB_L		equ	(2 << 2)	; assert RTS
ACIA_CMD_RTSB_L_BRK	equ	(3 << 2)	; assert RTS, transmit BREAK
ACIA_CMD_REM		equ	(1 << 4)	; Receiver echo mode

; Control register
ACIA_CTRL_115200	equ	0		; 115.2K baud
ACIA_CTRL_50		equ	1		; 50 baud
ACIA_CTRL_75		equ	2		; 75 baud
ACIA_CTRL_109_92	equ	3		; 109.92 baud
ACIA_CTRL_134_51	equ	4		; 134.51 baud
ACIA_CTRL_150		equ	5		; 150 baud
ACIA_CTRL_300		equ	6		; 300 baud
ACIA_CTRL_600		equ	7		; 600 baud
ACIA_CTRL_1200		equ	8		; 1200 baud
ACIA_CTRL_1800		equ	9		; 1800 baud
ACIA_CTRL_2400		equ	10		; 2400 baud
ACIA_CTRL_3600		equ	11		; 3600 baud
ACIA_CTRL_4800		equ	12		; 4800 baud
ACIA_CTRL_7200		equ	13		; 7200 baud
ACIA_CTRL_9600		equ	14		; 9600 baud
ACIA_CTRL_19200		equ	15		; 19200 baud
ACIA_CTRL_RCS		equ	(1 << 4)	; 1 = Baud rate, 0 = RxC
ACIA_CTRL_WL8		equ	(0 << 5)	; 8-bit word
ACIA_CTRL_WL7		equ	(1 << 5)	; 7-bit word
ACIA_CTRL_WL6		equ	(2 << 5)	; 6-bit word
ACIA_CTRL_WL5		equ	(3 << 5)	; 5-bit word
ACIA_CTRL_SB1		equ	(0 << 7)	; 1 stop bit
ACIA_CTRL_SB2		equ	(1 << 7)	; 2 stop bits (1.5 for WL5)

;
; acia_init --
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
acia_init
	;
	; We run the ACIA at 115.2K 8N1.  There is an auto-RTS
	; circuit connected to the /IRQ line, so we enable the
	; IRQ output even though we poll for input.
	;
	clr	ACIA_REG_SR		; programmed reset
	lda	#ACIA_CTRL_115200+ACIA_CTRL_RCS+ACIA_CTRL_WL8+ACIA_CTRL_SB1
	sta	ACIA_REG_CTRL
	clr	ACIA_REG_CMD
	rts

;
; acia_reinit --
;	Re-initialize the ACIA UART.  This is intended to bring the
;	console back on-line after a program runs and maybe screws
;	with it.  It's not needed for a serial console.
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
acia_reinit
	rts

;
; acia_putchar --
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
acia_putchar
	sta	ACIA_REG_DATA
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
	;	ldx	#acia_delay_tab	3 cycles
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
	ldx	#acia_delay_tab		; X <-- address of acia_delay_tab
	lda	CLOCK_SPEED_REG		; A <-- cpu speed
	lda	A,X			; loop count from table
1	deca
	bne	1B			; waste some cycles
	puls	A,X,PC			; Restore and return

acia_delay_tab
	;	0, 1MHz, 2MHz, x, 4MHz
	fcb	0, 8,    22,   0, 50

;
; acia_pollchar --
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
acia_pollchar
	lda	ACIA_REG_SR	; clears IRQ (asserts auto-/RTS)
	bita	#ACIA_SR_RDRF	; test Receive Data Register Full
	beq	1F		; not set, return
	lda	ACIA_REG_DATA	; set, return character
1	rts

;
; acia_getchar --
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
acia_getchar
1	lda	ACIA_REG_SR	; clears IRQ (asserts auto-/RTS)
	bita	#ACIA_SR_RDRF	; test Receive Data Register Full
	beq	1B		; not set, check again
	lda	ACIA_REG_DATA	; set, return character
	rts
