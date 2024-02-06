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
; TL16C550 NHACP driver.
;

;
; ace_nhacp_init --
;	Initialize the NHACP ACE UART.
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
ace_nhacp_init
	pshs	A,X,Y			; save registers
	ldx	#ace_nhacp_softc	; X = NHACP softc
	ldy	#UART1_BASE
	sty	ace_sc_addr,X		; set UART base address
	lda	#ACE_FCR_RXT_14
	sta	ace_sc_fcr,X		; set prototype FCR
	jsr	ace_init		; Do the initialization.
	puls	A,X,Y,PC		; restore and return

;
; ace_nhacp_putchar --
;	Output a character on the NHACP UART.
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
ace_nhacp_putchar
	pshs	X			; save registers
	ldx	#ace_nhacp_softc	; X = NHACP softc
	jsr	ace_putchar		; Do it.
	puls	X,PC			; restore and return

;
; ace_nhacp_pollchar --
;	Poll for a character from the NHACP UART.
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
ace_nhacp_pollchar
	pshs	X			; save registers
	ldx	#ace_nhacp_softc	; X = NHACP softc
	jsr	ace_pollchar		; Do it.
	puls	X,PC			; restore and return

;
; ace_nhacp_fs_mount --
;	Initialize the Data ACE UART and start an NHACP system session.
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
ace_nhacp_fs_mount
	pshs	B,X,Y,U

	; Initialize the UART.
	bsr	ace_nhacp_init

	; Initialize our NHACP context.
	ldu	#ace_nhacp_context
	ldd	#ace_nhacp_pollchar
	std	nhctx_pollc,U
	ldd	#ace_nhacp_putchar
	std	nhctx_putc,U

	; Start a system session with the server.
	; A contains the error code upon return.
	jsr	nhacp_start_system_session

	puls	B,X,Y,U,PC		; restore and return

;
; ace_nhacp_fs_unmount --
;	Shut down the Data ACE NHACP system session.
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
ace_nhacp_fs_unmount
	pshs	A,B,X,Y,U
	ldu	#ace_nhacp_context
	jsr	nhacp_end_session
	clr	UART1_BASE+ACE_REG_MCR	; de-assert RTS
	puls	A,B,X,Y,U,PC		; restore and return

;
; ace_nhacp_file_open --
;	Data ACE file open routine.
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
ace_nhacp_file_open
	pshs	U			; save registers
	ldu	#ace_nhacp_context
	jsr	file_nhacp_open
	puls	U,PC			; restore and return

;
; ace_nhacp_file_io --
;	Data ACE file I/O routine.
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
ace_nhacp_file_io
	pshs	U			; save registers
	ldu	#ace_nhacp_context
	jsr	file_nhacp_io
	puls	U,PC			; restore and return

;
; ace_nhacp_file_close --
;	Data ACE file close routine.
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
ace_nhacp_file_close
	pshs	U			; save registers
	ldu	#ace_nhacp_context
	jsr	file_nhacp_close
	puls	U,PC			; restore and return

ace_nhacp_fileops
	fdb	ace_nhacp_file_open
	fdb	ace_nhacp_file_io
	fdb	ace_nhacp_file_close

ace_nhacp_devname
	fcn	"UART1"

ace_nhacp_fsops
	fdb	ace_nhacp_devname
	fdb	nhacp_fsname
	fdb	ace_nhacp_fileops
	fdb	ace_nhacp_fs_mount
	fdb	ace_nhacp_fs_unmount
