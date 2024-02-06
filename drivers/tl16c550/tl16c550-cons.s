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

;
; TL16C550 console driver.
;

;
; ace_cons_init --
;	Initialize the console ACE UART.
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
ace_cons_init
	pshs	A,X,Y			; save registers
	ldx	#ace_cons_softc		; X = console softc
	ldy	#UART0_BASE
	sty	ace_sc_addr,X		; set UART base address
	lda	#ACE_FCR_RXT_4
	sta	ace_sc_fcr,X		; set prototype FCR
	jsr	ace_init		; Do the initialization.
	puls	A,X,Y,PC		; restore and return

;
; ace_cons_reinit --
;	Re-initialize the console UART.  This is intended to bring the
;	console back on-line after a program runs and maybe screws
;	with it.
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
ace_cons_reinit
	pshs	X			; save registers
	ldx	#ace_cons_softc		; X = console softc
	jsr	ace_reinit		; Do it.
	puls	X,PC			; restore and return

;
; ace_cons_putchar --
;	Output a character on the console UART.
;
; Arguments --
;	A - character to output.
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
ace_cons_putchar
	pshs	X			; save registers
	ldx	#ace_cons_softc		; X = console softc
	jsr	ace_putchar		; Do it.
	puls	X,PC			; restore and return

;
; ace_cons_pollchar --
;	Poll for a character from the console UART.
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
ace_cons_pollchar
	pshs	X			; save registers
	ldx	#ace_cons_softc		; X = console softc
	jsr	ace_pollchar		; Do it.
	puls	X,PC			; restore and return

;
; ace_cons_getchar --
;	Get an input character from the console UART.  Blocks until a
;	character is available.
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
ace_cons_getchar
	pshs	X			; save registers
	ldx	#ace_cons_softc		; X = console softc
	jsr	ace_getchar		; Do it.
	puls	X,PC			; restore and return
