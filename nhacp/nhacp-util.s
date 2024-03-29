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
; Utility routines for a client implementation of the NABU HCCA Application
; Communication Protocol.
;

nhacp_fsname
	fcn	"NHACP"

;
; nhacp_copyin
;	Copy data in from the NHACP interface.
;
; Arguments --
;	U - NHACP context
;	X - destination buffer
;	D - byte count
;
; Returns --
;	CC_Z is set if a timeout occurs, clear if OK.
;
; Clobbers --
;	D, X, Y
;
nhacp_copyin
	leay	D,X		; Y = end pointer
	pshs	Y		; push it onto stack
1	jsr	nhacp_get_reply_byte ; get byte from server
	beq	99F		; handle timeout
	sta	,X+		; store byte, advance buffer pointer
	cmpx	,S		; At the end?
	bne	1B		; Nope, go get more data
	andcc	#~CC_Z		; did not time out
98	puls	Y,PC		; pop Y (clobber) and return
99
	jsr	nhacp_invalidate_session
	bra	98B

;
; nhacp_copyout
;	Copy data out to the NHACP interface.
;
; Arguments --
;	U - NHACP context
;	X - source buffer
;	D - byte count
;
; Returns --
;	None.
;
; Clobbers --
;	D, X, Y
;	
nhacp_copyout
	leay	D,X		; Y = end pointer
	pshs	Y		; push it onto stack
	lda	nhctx_session,U	; check for dead session
	beq	99F
1	lda	,X+		; get a byte, advance buffer pointer
	jsr	[nhctx_putc,U]	; push the byte to the server
	cmpx	,S		; At the end?
	bne	1B		; Nope, go get more data
99	puls	Y,PC		; pop Y (clobber) and return

;
; nhacp_drain
;	Drain the residual data from an NHACP reply.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	CC_Z is set if a timeout occurs, clear otherwize.
;
; Clobbers --
;	D
;
nhacp_drain
	ldd	nhctx_reply_len,U
	beq	98F
	subd	#1
	std	nhctx_reply_len,U
	bsr	nhacp_getc
	beq	99F
	bra	nhacp_drain
98	andcc	#~CC_Z
99	rts

;
; nhacp_get_reply_byte
;	Get a single reply byte and decrement the residual count.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	A - fetched byte
;
;	CC_Z is set if a timeout occurs, cleared if OK.
;
; Clobbers --
;	A, B
;
nhacp_get_reply_byte
	ldd	nhctx_reply_len,U
	beq	99F		; already at 0?
	bmi	99F		; or, gasp, negative?
	subd	#1		; decrement length
	std	nhctx_reply_len,U
	beq	1F		; if last byte, disarm timer after receiving it
	bsr	nhacp_getc	; get the byte, handles CC_Z for us.
	rts
1
	bsr	nhacp_getc	; get the last byte, handles CC_Z for us.
	beq	1F		; :-( timed out receiving last byte
	nhacp_timer_disarm
	andcc	#~CC_Z		; successful receive
1	rts
99
	clra			; return 0s if we're over.
	andcc	#~CC_Z		; but don't treat this as a framing error
	rts

;
; nhacp_getc
;	Get a byte from the NHACP interface.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	A - the byte
;
;	CC_Z is set if no byte was available before the timeout expired,
;	clear if a byte was successfully received.
;
; Clobbers --
;	None.
;
nhacp_getc
	jsr	[nhctx_pollc,U]
	bne	99F
	nhacp_timer_expired
	beq	99F
	bra	nhacp_getc
99	rts

;
; nhacp_get_reply_hdr
;	The the reply header (frame length + msg type) from the NHACP server.
;	This routine also arms the receive timer.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	Z is set if no reply was received (either timeout / dead session or
;	zero length frame).
;
; Clobbers --
;	A, B
;
nhacp_get_reply_hdr
	lda	nhctx_session,U	     ; check for dead session
	beq	99F
	nhacp_timer_arm
	bsr	nhacp_getc	     ; get LSB of reply length
	beq	98F		     ; check for timeout
	sta	nhctx_reply_len+1,U  ; store it in big-endian order
	bsr	nhacp_getc	     ; get MSB if reply length
	beq	98F		     ; check for timeout
	sta	nhctx_reply_len,U
	ora	nhctx_reply_len+1,U  ; check for zero length
	beq	99F
	lbsr	nhacp_get_reply_byte ; get msg type
	beq	98F		     ; check for timeout
	sta	nhctx_reply_type,U
	andcc	#~CC_Z		     ; make sure Z is not set
	rts
98
	; Timeout occurred, invalidate session.
	jsr	nhacp_invalidate_session
99
	nhacp_timer_disarm
	clr	nhctx_reply_len,U   ; zero reply length
	clr	nhctx_reply_len+1,U ; sets Z
	rts

;
; nhacp_start_system_session
;	Start an NHACP system session.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	A - error code if session setup fails (0 == no error)
;
; Clobbers --
;	A, B, X, Y
;
nhacp_start_system_session
	;
	; We don't use the normal nhacp_req_send() routine because
	; we have to manupulate the session ID field directly.
	;
	clrb			; system session
	;
	; FALLTHROUGH
	;
nhacp_start_session_common
	;
	; B contains session request argument:
	; $00 -> start system session
	; $FF -> allocate non-system session
	;
	leax	nhctx_req,U
	ldy	#nhacp_HELLO_template
	lda	#nhacp_HELLO_template_len
	jsr	memcpy8
	stb	nhctx_req_session,U

	; Store a dummy session ID in the context while we
	; process the request.
	lda	#$FF
	sta	nhctx_session,U

	; Add this context's timer to the system.
	leax	nhctx_timer,U
	clr	tmr_callout,X		; no callout, we just check for
	clr	tmr_callout+1,X		; expiration
	jsr	[SysSubr_timer_add]

	; Send request.
	leax	nhctx_req,U
	ldd	#nhacp_HELLO_template_len
	jsr	nhacp_copyout

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	beq	nhacp_start_session_eagain ; timed out

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	beq	nhacp_start_session_error_reply

	; Unknown error if not SESSION_STARTED.
	cmpa	#NHACP_RESP_SESSION_STARTED
	bne	nhacp_start_session_eio

	; Get the session ID.
	jsr	nhacp_get_reply_byte
	beq	nhacp_start_session_eagain ; timed out
	inca			; add 1 for dead session detection
	sta	nhctx_session,U	; store session
	jsr	nhacp_drain	; don't care about the rest of the reply
	beq	nhacp_start_session_eagain ; timed out
	clra			; return no error.
	rts

nhacp_start_session_error_reply
	; Get the error code from the reply.
	jsr	nhacp_get_reply_byte
	beq	nhacp_start_session_eagain ; timeout -> EAGAIN
	tsta
	bne	1F		; error != 0, cool cool.
				; error == 0, map to EIO
nhacp_start_session_eio
	lda	#EIO
	bra	1F

nhacp_start_session_eagain
	lda	#EAGAIN
1	pshs	A		; save error return
	jsr	nhacp_drain	; drain off any remaining reply
	clr	nhctx_session,U	; mark session dead
	leax	nhctx_timer,U	; remove the context's timer from the system
	jsr	[SysSubr_timer_remove]
	puls	A,PC		; restore error and return

;
; nhacp_req_send --
;	Send an NHACP request.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	None.
;
; Clobbers --
;	A, B, X, Y
;
nhacp_req_send
	; Initialize the frame header.
	lda	#$8f		; NHACP-REQUEST
	sta	nhctx_req,U
	clra
	ldb	nhctx_reqlen,U
	addd	nhctx_datalen,U
	stb	nhctx_req_flen,U
	sta	nhctx_req_flen+1,U
	lda	nhctx_session,U
	beq	99F		; dead session
	deca			; convert to actual dession ID
	sta	nhctx_req_session,U

	; Send request header.
	leax	nhctx_req,U
	clra
	ldb	nhctx_reqlen,U
	addb	#4		; add $8F, session ID, frame length bytes
	jsr	nhacp_copyout

	; Send data buffer.
	ldd	nhctx_datalen,U
	beq	99F		; no data to send
	ldx	nhctx_data,U
	jsr	nhacp_copyout
	;
	; If we get here because of a dead session, no worries.
	; the fetching of the reply will also detect dead session
	; and fake up an error reply.
	;
99	rts

;
; nhacp_invalidate_session --
;	Invalidate the NHACP session.  This is called when we
;	detect a framing error.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	CC_Z is set upon return.
;
; Clobbers --
;	None.
;
nhacp_invalidate_session
	clr	nhctx_session,U	; mark session dead
	rts

nhacp_HELLO_template
	fcc	$8f		; NHACP-REQUEST
	fcc	$00		; session ID
	fcc	$08,$00		; frame length
	fcc	$00		; type
	fcc	"ACP"		; magic
	fcc	$01,$00		; version 0.1
	fcc	$00,$00		; options = $0000

nhacp_HELLO_template_len	equ	12

;
; nhacp_end_session
;	End an NHACP session.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	None.
;
; Clobbers --
;	A, B, X, Y
;
nhacp_end_session
	nhacp_req_init "GOODBYE"
	jsr	nhacp_req_send
	clr	nhctx_session,U	; invalidate session.
	leax	nhctx_timer,U	; remove the context's timer from the system
	jsr	[SysSubr_timer_remove]
	rts			; no reply to a GOODBYE message

;
; nhacp_req_init0 --
;	This does the real work for nhacp_req_init().
;
; Arguments --
;	A - request type
;	B - request length
;	U - NHACP context
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
nhacp_req_init0
	; Initialize the context fields.
	sta	nhctx_req_type,U
	stb	nhctx_reqlen,U
	clr	nhctx_datalen,U
	clr	nhctx_datalen+1,U
	clr	nhctx_timer+tmr_t1,U
	clr	nhctx_timer+tmr_t0,U
	rts
