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
; Abstract console driver for the 6809 Playground.
;

;
; cons_init --
;	Initialize the console.
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
cons_init
	if CONFIG_CONSOLE_TL16C550
	jsr	ace_cons_init
	elsif CONFIG_CONSOLE_W65C51
	jsr	acia_init
	endif
	if CONFIG_DISPLAY_TMS9918A
	jsr	VDP_tty_init
	endif
	rts

;
; cons_reinit --
;	Re-initialize the console after a program has run.
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
cons_reinit
	if CONFIG_CONSOLE_TL16C550
	jsr	ace_cons_reinit
	elsif CONFIG_CONSOLE_W65C51
	jsr	acia_reinit
	endif
	if CONFIG_DISPLAY_TMS9918A
	jsr	VDP_tty_reinit
	endif
	rts

;
; cons_putc --
;	Output a character on the console.
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
cons_putc
	if CONFIG_DISPLAY_TMS9918A
	jsr	VDP_tty_putc
	endif
	if CONFIG_CONSOLE_TL16C550
	jmp	ace_cons_putchar
	elsif CONFIG_CONSOLE_W65C51
	jmp	acia_putchar
	else
	rts
	endif

;
; cons_pollc --
;	Poll for a character from the console.
;
; Arguments --
;	None.
;
; Returns --
;       CC_Z is set if there is no character available, and clear if
;       a character was read from the console.
;
;       A - Character received from the console, if available.
;
; Clobbers --
;	None.
;
cons_pollc
	if CONFIG_CONSOLE_TL16C550
	jmp	ace_cons_pollchar
	elsif CONFIG_CONSOLE_W65C51
	jmp	acia_pollchar
	else
	orcc	#CC_Z
	rts
	endif

;
; cons_getc --
;	Get an input character from the console.  Blocks until a character
;	is available.
;
; Arguments --
;	None.
;
; Returns --
;	A - Character received from the console.
;
; Clobbers --
;	None.
;
cons_getc
	if CONFIG_CONSOLE_TL16C550
	jmp	ace_cons_getchar
	elsif CONFIG_CONSOLE_W65C51
	jmp	acia_getchar
	else
	bra	cons_getc		; not ideal.
	endif
