;
; Copyright (c) 2024 Jason R. Thorpe.
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

	if	tl15c550_driver_included
	else
tl15c550_driver_included	equ	1

;
; Driver for the TL16C550 Asynchronous Communications Element (ACE)
; Operates purely in polled mode, and uses the auto-RTS-CTS feature
; provided by the chip.
;

ACE_REG_RBR	equ	0		; receive buffer register
ACE_REG_THR	equ	0		; transmit holding register
ACE_REG_IER	equ	1		; interrupt enable register
ACE_REG_IIR	equ	2		; interrupt ident register
ACE_REG_FCR	equ	2		; FIFO control register
ACE_REG_LCR	equ	3		; line control register
ACE_REG_MCR	equ	4		; modem control register
ACE_REG_LSR	equ	5		; line status register
ACE_REG_MSR	equ	6		; modem status register
ACE_REG_SCR	equ	7		; scratch register

; DLAB=1 registers
ACE_REG_DLL	equ	0		; divisor latch (LSB)
ACE_REG_DLM	equ	1		; divisor latch (MSB)
ACE_REG_AFR	equ	2		; alternate function register

; Interrupt Enable Register
ACE_IER_ERBI	equ	(1 << 0)	; receive data available
ACE_IER_ETBEI	equ	(1 << 1)	; transmit holding register empty
ACE_IER_ELSI	equ	(1 << 2)	; line status
ACE_IER_EDSSI	equ	(1 << 3)	; modem status

; Interrupt Ident Register
ACE_IIR_MASK	equ	$0F
ACE_IIR_NOPEND	equ	$01		; set if no interrupt pending
ACE_IIR_MLS	equ	$00		; modem status
ACE_IIR_TXRDY	equ	$02		; transmitter ready
ACE_IIR_RXRDY	equ	$04		; receiver ready
ACE_IIR_RLS	equ	$06		; line status
ACE_IIR_RXTOUT	equ	$0c		; receiver timeout
ACE_IIR_FIFOEN	equ	$c0		; set when FIFOs enabled

; FIFO Control Register
ACE_FCR_FIFOEN	equ	(1 << 0)	; FIFO enable
ACE_FCR_RFR	equ	(1 << 1)	; Rx FIFO reset
ACE_FCR_TFR	equ	(1 << 2)	; Tx FIFO reset
ACE_FCR_DMA	equ	(1 << 4)	; DMA mode select
ACE_FCR_RXT_1	equ	(0 << 6)	; Rx FIFO trigger: 1 byte
ACE_FCR_RXT_4	equ	(1 << 6)	; Rx FIFO trigger: 4 bytes
ACE_FCR_RXT_8	equ	(2 << 6)	; Rx FIFO trigger: 8 bytes
ACE_FCR_RXT_14	equ	(3 << 6)	; Rx FIFO trigger: 14 bytes
ACE_FCR_RXT_MASK equ	(3 << 6)

; Line Control Register
ACE_LCR_D5	equ	$00		; 5 data bits
ACE_LCR_D6	equ	$01		; 6 data bits
ACE_LCR_D7	equ	$02		; 7 data bits
ACE_LCR_D8	equ	$03		; 8 data bits
ACE_LCR_S1	equ	0		; 1 stop bit
ACE_LCR_S2	equ	(1 << 2)	; 2 stop bits (1.5 for D5)
ACE_LCR_NP	equ	0		; no parity
ACE_LCR_PEN	equ	(1 << 3)	; parity enable
ACE_LCR_PODD	equ	0
ACE_LCR_PEVEN	equ	(1 << 4)	; even parity
ACE_LCR_PSTICK	equ	(1 << 5)	; stick party (see data sheet)
ACE_LCR_BREAK	equ	(1 << 6)	; send BREAK
ACE_LCR_DLAB	equ	(1 << 7)	; divisor latch access bit

; Line Status Register
ACE_LSR_DR	equ	(1 << 0)	; data ready
ACE_LSR_OE	equ	(1 << 1)	; overrun error
ACE_LSR_PE	equ	(1 << 2)	; parity error
ACE_LSR_FE	equ	(1 << 3)	; framing error
ACE_LSR_BI	equ	(1 << 4)	; break interrupt
ACE_LSR_THRE	equ	(1 << 5)	; transmit holding register empty
ACE_LSR_TEMT	equ	(1 << 6)	; transmitter empty (FIFO + SR)
ACE_LSR_FIFO_ERR equ	(1 << 7)	; at least one error in Rx FIFO

; Modem Control Register
ACE_MCR_DTR	equ	(1 << 0)	; assert DTR
ACE_MCR_RTS	equ	(1 << 1)	; assert RTS
ACE_MCR_OUT1	equ	(1 << 2)	; see data sheet
ACE_MCR_OUT2	equ	(1 << 3)	; see data sheet
ACE_MCR_LOOP	equ	(1 << 4)	; loopback test mode
ACE_MCR_AFE	equ	(1 << 5)	; auto-flow control enable

; Modem Status Register
ACE_MSR_DCTS	equ	(1 << 0)	; CTS changed
ACE_MSR_DDSR	equ	(1 << 1)	; DSR changed
ACE_MSR_TERI	equ	(1 << 2)	; RI toggled low -> high
ACE_MSR_DDCD	equ	(1 << 3)	; DCD changed
ACE_MSR_CTS	equ	(1 << 4)	; CTS asserted
ACE_MSR_DSR	equ	(1 << 5)	; DSR asserted
ACE_MSR_RI	equ	(1 << 6)	; RI asserted
ACE_MSR_DCD	equ	(1 << 7)	; DCD asserted

; Baud rate generator divisors for a 1.8432Mhz clock source
ACE_BAUD_115200	equ	1
ACE_BAUD_56000	equ	2
ACE_BAUD_38400	equ	3
ACE_BAUD_19200	equ	6
ACE_BAUD_9600	equ	12
; (no one really wants to go any slower than 115.2K anyway...)

; Alternate Function Register
ACE_AFR_CWM	   equ	(1 << 0)	; concurrent write mode
ACE_AFR_MF_OP	   equ	(0 << 1)	; /MF == /OP
ACE_AFR_MF_BAUDOUT equ	(1 << 1)	; /MF == /BAUDOUT
ACE_AFR_MF_RXRDY   equ	(2 << 1)	; /MF == /RDRDY
;			(3 << 1)	; Reserved

;
; ace_init --
;	Initialize an ACE UART.
;
; Arguments --
;	X - address of ACE softc.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
ace_init
	pshs	A,B,Y			; save registers
	ldy	ace_sc_addr,X		; Y = UART base address

	clr	ACE_REG_IER,Y		; disable all interrupts

	lda	#ACE_LCR_DLAB		; enable divisor latch access
	sta	ACE_REG_LCR,Y

	; Set for 115.2K baud
	ldd	#ACE_BAUD_115200
	sta	ACE_REG_DLM,Y
	stb	ACE_REG_DLL,Y

	; Set for 8N1
	lda	#ACE_LCR_D8+ACE_LCR_NP+ACE_LCR_S1
	sta	ACE_REG_LCR,Y

	; Enable the FIFOs.
	lda	ace_sc_fcr,X		; get desired FIFO trigger threshold
	anda	#ACE_FCR_RXT_MASK
	ora	#ACE_FCR_FIFOEN+ACE_FCR_RFR+ACE_FCR_TFR
	sta	ACE_REG_FCR,Y

	; Enable auto-RTS-CTS.
	lda	#ACE_LSR_AFE+ACE_LSR_RTS
	sta	ACE_REG_MCR,Y

	puls	A,B,Y,PC		; restore and return

;
; ace_reinit --
;	Re-initialize the ACIA UART.  This is intended to bring the
;	console back on-line after a program runs and maybe screws
;	with it.
;
; Arguments --
;	X - address of ACE softc.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
ace_reinit
	rts

;
; ace_putchar --
;	Output a character on the UART.
;
; Arguments --
;	A - character to output.
;	X - address of ACE softc.
;
; Returns --
;	None.
;
; Clobbers --
;	X
;
ace_putchar
	pshs	B			; save registers
	ldx	ace_sc_addr,X		; X = UART base address

	; Wait for the Tx FIFO to be available.
1	ldb	ACE_REG_LSR,X
	bitb	#ACE_LSR_THRE		; THRE set?
	beq	1B			; no, loop

	sta	ACE_REG_THR,X		; put the byte in the Tx register

	puls	B,PC			; restore and return

;
; ace_pollchar --
;	Poll for a character from the UART.
;
; Arguments --
;	X - address of ACE softc.
;
; Returns --
;	CC_Z is set if there is no character available, and clear if
;	a character was read from the UART.
;
;	A - Character received from the UART, if available.
;
; Clobbers --
;	X
;
ace_pollchar
	ldx	ace_sc_addr,X		; X = UART base address

	lda	ACE_REG_LSR,X
	bita	#ACE_LSR_DR		; DR set?
	beq	1F			; not set, return
	lda	ACE_REG_RBR,X		; set, return character
	andcc	#~CC_Z			; make sure Z is clear
1	rts

;
; ace_getchar --
;	Get an input character from the UART.  Blocks until a character
;	is available.
;
; Arguments --
;	X - address of ACE softc.
;
; Returns --
;	A - Character received from the UART.
;
; Clobbers --
;	X
;
ace_getchar
	ldx	ace_sc_addr,X		; X = UART base address

1	lda	ACE_REG_LSR,X
	bita	#ACE_LSR_DR		; DR set?
	beq	1B			; not set, loop
	lda	ACE_REG_RBR,X		; set, return character
	rts

	endif	; tl15c550_driver_included
