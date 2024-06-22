;
; Copyright (c) 2022, 2024 Jason R. Thorpe.
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
; I2C driver for the NXP PCF8584 I2C controller found on the Playground'09.
;
; Register bit names derived from a public domain header file written
; by Tobias Nygren.
;

PCF8584_REG_DATA	equ	(I2C_BASE + 0)	; S0 / S0'
PCF8584_REG_CSR		equ	(I2C_BASE + 1)	; S1
PCF8584_REG_IVEC	equ	(I2C_BASE + 0)	; S2
PCF8584_REG_CLOCK	equ	(I2C_BASE + 0)	; S3

; Bits in the control/status register
; (Control - write only)
PCF8584_CTRL_ACK	equ	$01	; send ACK
PCF8584_CTRL_STO	equ	$02	; send STOP
PCF8584_CTRL_STA	equ	$04	; send START
PCF8584_CTRL_ENI	equ	$08	; enable interrupt
PCF8584_CTRL_ES2	equ	$10	; alternate register select
PCF8584_CTRL_ES1	equ	$20	; alternate register select
PCF8584_CTRL_ESO	equ	$40	; enable serial output
PCF8584_CTRL_PIN	equ	$80	; Pending Interrupt Not

PCF8584_CTRL_CLOCK_REG	equ	PCF8584_CTRL_ES1
PCF8584_CTRL_IVEC_REG	equ	PCF8584_CTRL_ES2

; (Status - read only)
PCF8584_STATUS_BBN	equ	$01	; Bus Busy Not
PCF8584_STATUS_LAB	equ	$02	; lost arbitration
PCF8584_STATUS_AAS	equ	$04	; addressed as slave
PCF8584_STATUS_LRB	equ	$08	; last received bit (NACK+bcast det.)
PCF8584_STATUS_BER	equ	$10	; bus error
PCF8584_STATUS_STS	equ	$20	; external stop condition detected
PCF8584_STATUS_INI	equ	$40	; 0 if initialized
PCF8584_STATUS_PIN	equ	$80	; pending interrupt not

; Short-hand for serial I/O commands.
PCF8584_CMD_START	equ	(PCF8584_CTRL_PIN + PCF8584_CTRL_ESO + PCF8584_CTRL_STA + PCF8584_CTRL_ACK)
PCF8584_CMD_STOP	equ	(PCF8584_CTRL_PIN + PCF8584_CTRL_ESO + PCF8584_CTRL_STO + PCF8584_CTRL_ACK)
PCF8584_CMD_REPSTART	equ	(PCF8584_CTRL_ESO + PCF8584_CTRL_STA + PCF8584_CTRL_ACK)
PCF8584_CMD_IDLE	equ	(PCF8584_CTRL_PIN + PCF8584_CTRL_ESO + PCF8584_CTRL_ACK)
PCF8584_CMD_NACK	equ	(PCF8584_CTRL_ESO)

; Clock register fields.
PCF8584_SCL_90		equ	0	; 90 KHz
PCF8584_SCL_45		equ	1	; 45 KHz
PCF8584_SCL_11		equ	2	; 11 KHz
PCF8584_SCL_1_5		equ	3	; 1.5 KHz
PCF8584_CLK_3		equ	0	; 3MHz clock source
PCF8584_CLK_4_43	equ	$10	; 4.43MHz clock source
PCF8584_CLK_6		equ	$14	; 6MHz clock source
PCF8584_CLK_8		equ	$18	; 8MHz clock source
PCF8584_CLK_12		equ	$1c	; 12MHz clock source

;
; pcf8584_send_bytes --
;	Send bytes to the I2C bus.  This is an internal function
;	used by pcf8584_exec().
;
; Arguments --
;	X -- points to the buffer to send.
;	B -- buffer length (1-255).
;
; Returns --
;	CC_C is clear upon success, set if an error occurs.
;
; Clobbers --
;	A, B, X
;
pcf8584_send_bytes
1	lda	PCF8584_REG_CSR
	bita	#PCF8584_STATUS_PIN	; PIN set?
	bne	1B			; Yup, loop.
	bita	#PCF8584_STATUS_LRB	; LRB set?
	bne	2F			; Yes, device failed to ACK.
	lda	,X+			; A = *X++
	sta	PCF8584_REG_DATA	; write out the data byte
	decb				; B--
	bne	1B			; Loop again if more data.

	; Wait for the final byte to finish.
1	lda	PCF8584_REG_CSR
	bita	#PCF8584_STATUS_PIN	; PIN set?
	bne	1B			; Yup, loop.
	bita	#PCF8584_STATUS_LRB	; LRB set?
	bne	2F			; Yes, device failed to ACK.

	andcc	#~CC_C			; clear CC
	rts
2
	orcc	#CC_C			; set CC
	rts

;
; pcf8584_exec --
;	Execute an I2C command block.
;
; Arguments --
;	U -- Address of I2C command block.
;
; Returns --
;	CC_C is clear upon success, set if an error occurs.
;
; Clobbers --
;	None.
;
pcf8584_exec
	pshs	A,B,X			; Save registers
	;
	; Stack:
	;
	; 3
	; 2	Saved X
	; 1	Saved B
	; 0	Saved A
	;

	; Wait for Bus-Busy-Not.
1	lda	PCF8584_REG_CSR
	bita	#PCF8584_STATUS_BBN	; BBN set?
	beq	1B			; Nope, loop.

	; First, check if there are any command bytes to send.  If
	; there are, then we always send the initial START condition
	; as a write.
	ldb	i2c_cb_cmdlen,U		; B <-- command len
	beq	pcf8584_exec_no_cmd

	; Send the START condition for a write.
	lda	i2c_cb_devaddr,U	; A <-- device address
	asla				; Make room for R/~W bit
	sta	PCF8584_REG_DATA
	lda	#PCF8584_CMD_START
	sta	PCF8584_REG_CSR

	; Send the command bytes.
	ldx	i2c_cb_cmdptr,U		; X <-- command pointer
	bsr	pcf8584_send_bytes
	bcs	2F			; Get out if that failed.

	; Check to see if this is a read.  If so, we have to send a
	; REPEATED START to turn the bus around.
	lda	i2c_cb_opcode,U		; A <-- opcode
	bita	#i2c_opbit_read		; READ set?
	bne	pcf8584_exec_turnaround

pcf8584_exec_send_data
	; Now check for any data to send after the command.
	ldb	i2c_cb_datalen,U	; B <-- data len
	beq	1F			; Done if no data.
	ldx	i2c_cb_dataptr,U	; X <-- data pointer
	bsr	pcf8584_send_bytes
	bcs	2F			; Get out if that failed.
1
	andcc	#~CC_C			; Clear CC_C -- success!
2
	; Send the STOP condition.
	lda	#PCF8584_CMD_STOP
	sta	PCF8584_REG_CSR

	puls	A,B,X,PC		; Restore and return

pcf8584_exec_no_cmd
	; Compute and send the appropriate START condition.
	lda	i2c_cb_devaddr,U	; A <-- device address
	asla				; Make room for R/~W bit
	ldb	i2c_cb_opcode,U		; B <-- opcode
	lsrb				; CC_C <-- R/~W bit
	adca	#0			; mix it in
	sta	PCF8584_REG_DATA
	ldb	#PCF8584_CMD_START
	stb	PCF8584_REG_CSR

	bita	#i2c_opbit_read		; Doing a read?
	beq	pcf8584_exec_send_data	; Nope, send any data payload.
	bra	pcf8584_exec_recv_data	; Yup, go receive it!

pcf8584_exec_turnaround
	;
	; The flowchart in the the data sheet says to write the
	; REPEATED START command to the CSR and *then* write the
	; device address + R/~W bit to the DATA register.
	;
	lda	#PCF8584_CMD_REPSTART
	sta	PCF8584_REG_CSR
	lda	i2c_cb_devaddr,U	; A <-- device address
	asla				; Make room for R/~W bit
	inca				; Set READ bit
	sta	PCF8584_REG_DATA

pcf8584_exec_recv_data
	;
	; The chip is a little tricky with how it handles signalling
	; as a receiver.  The first read of the data register is a
	; dummy read and needs to be discarded (this read actually
	; triggers the state machine to clock in the first valid byte
	; of data).
	;
	; Furthermore, we need to NACK the last byte we read, which
	; means that we need to write the command to do that before
	; reading the next-to-last-byte, and we need to write the
	; commmand to set the STOP condition before we read the last
	; byte.
	;
	; To make this easier, we're going to pre-compute datalen - 1
	; and push it onto the stack for quick access later.
	;
	ldb	i2c_cb_datalen,U	; B <-- datalen
	pshs	B			; Push it onto stack

	; If the data length is 0, point the data buffer at the
	; scratch space we just pushed onto the stack; this simplifies
	; the last-byte handling.
	tfr	S,X			; X <-- scratch space on stack
	beq	1F			; B == 0? Skip...
	ldx	i2c_cb_dataptr,U	; X <-- data pointer
	dec	,S			; scratch value now datalen - 1
	clrb				; B <-- 0

1	lda	PCF8584_REG_CSR
	bita	#PCF8584_STATUS_PIN	; PIN set?
	bne	1B			; Yup, loop.
	cmpb	i2c_cb_datalen,U	; B == datalen?
	beq	4F			; Yes, go read the last byte
	bita	#PCF8584_STATUS_LRB	; LRB set?
	bne	6F			; Yes, device failed to ACK.

	cmpb	0,S			; B == datalen - 1?
	bne	2F			; No, skip sending NACK
	lda	#PCF8584_CMD_NACK
	sta	PCF8584_REG_CSR		; Send NACK
2
	lda	PCF8584_REG_DATA	; Read a byte of data.
	tstb				; B == 0?
	beq	3F			; Yup, dummy read.
	sta	,X+
3	incb				; B++
	bra	1B			; Continue loop.
4
	lda	#PCF8584_CMD_STOP
	sta	PCF8584_REG_CSR		; Send STOP condition
	lda	PCF8584_REG_DATA	; Read the last byte of data.
	sta	,X			; *X = A
	andcc	#~CC_C			; Clear CC_C -- success!
5
	leas	1,S			; Pop scratch space.
	puls	A,B,X,PC		; Restore and return
6
	; Send the STOP condition.
	lda	#PCF8584_CMD_STOP
	sta	PCF8584_REG_CSR

	orcc	#CC_C			; Set CC_C -- failed.
	bra	5B

;
; pcf8584_init --
;	Initialize the I2C controller.
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
pcf8584_init
	pshs	A			; Save registers

	; Initalize Own Address to 0x02 (something that will never be used
	; by a real I2C device -- "Reserved for Different Bus Formats",
	; and also the lowest bit is not set.)
	lda	#PCF8584_CTRL_PIN
	sta	PCF8584_REG_CSR
	lda	#$02
	sta	PCF8584_REG_DATA

	; Initialize the I2C clock generator.  We're connected to
	; an 8MHz clock source, and we'll use the standard 90KHz
	; I2C clock rate.
	lda	#(PCF8584_CTRL_PIN + PCF8584_CTRL_CLOCK_REG)
	sta	PCF8584_REG_CSR
	lda	#(PCF8584_SCL_90 + PCF8584_CLK_8)
	sta	PCF8584_REG_DATA

	; Put the controller into idle state.
	lda	#PCF8584_CMD_IDLE
	sta	PCF8584_REG_CSR

	puls	A,PC			; Restore and return
