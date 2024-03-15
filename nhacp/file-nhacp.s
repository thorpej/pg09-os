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
; File I/O API implementation using NHACP.  The call stack
; looks like this:
;
;	kernel file I/O calls ->
;	interface file I/O calls [stashes NHACP context in FCB] ->
;	NHACP file I/O calls ->
;	NHACP utility routines ->
;	interface getc/putc [via NHACP context]
;

fcb_nhacp_ctx		equ	(fcb_opaque + 0)	; 2 bytes
fcb_nhacp_fd		equ	(fcb_nhacp_ctx + 2)	; 1 byte

;
; NHACP specifies a max payload of 8192 bytes, so we may need to loop
; around requests that are longer than that.  We use temporary space
; in the FCB to track the work.
;
fcb_nhacp_ptr		equ	(fcb_nhacp_fd + 1)	; 2 bytes
fcb_nhacp_resid		equ	(fcb_nhacp_ptr + 2)	; 2 bytes
fcb_nhacp_offset	equ	(fcb_nhacp_resid + 2)	; 4 bytes
fcb_nhacp_actual	equ	(fcb_nhacp_offset + 4)	; 2 bytes

;
; file_nhacp_open --
;	file_open routine using NHACP.
;
; Arguments --
;	X - pointer to file open arguments
;	U - pointer to NHACP context
;
; Returns --
;	Error status in File Control Block.
;
; Clobbers --
;	None.
;
file_nhacp_open
	pshs	A,B,X,Y,U	; save registers
	;
	; 7,S
	; 6,S	U
	; 5,S
	; 4,S	Y
	; 3,S
	; 2,S	X		; saved args pointer
	; 1,S	B
	; 0,S	A
	;
	ldy	fopen_fcb,X	; Y = FCB

	nhacp_req_init "STORAGE_OPEN"

	lda	#$FF		; server chooses fdesc
	sta	nhctx_req_args,U

	clr	fcb_nhacp_fd,Y	; default to invalid fdesc on error

	ldd	fopen_flags,X
	stb	nhctx_req_args+1,U
	sta	nhctx_req_args+2,U

	lda	fopen_namelen,X
	sta	nhctx_req_args+3,U
	clr	nhctx_datalen,U
	sta	nhctx_datalen+1,U

	ldd	fopen_name,X
	std	nhctx_data,U

	pshs	Y		; save FCB pointer
	;
	; 9,S
	; 8,S	U
	; 7,S
	; 6,S	Y
	; 5,S
	; 4,S	X		; saved args pointer
	; 3,S	B
	; 2,S	A
	; 1,S
	; 0,S			; saved FCB pointer
	;

	jsr	nhacp_req_send

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	lbeq	file_nhacp_io_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	lbeq	file_nhacp_io_error_reply

	; Unknown error if not STORAGE_LOADED.
	cmpa	#NHACP_RESP_STORAGE_LOADED
	lbne	file_nhacp_io_eio

	; Get the file descriptor.  We don't care about
	; the rest of the reply.
	jsr	nhacp_get_reply_byte
	lbeq	file_nhacp_io_eio	; handle receive timeout
	ldy	,S		; get saved FCB pointer (but don't pop)
	inca			; we save it as fdesc+1
	sta	fcb_nhacp_fd,Y	; save fdesc in FCB
	lbra	file_nhacp_io_done

file_nhacp_io_jmptab
	fdb	file_nhacp_io_read
	fdb	file_nhacp_io_pread
	fdb	file_nhacp_io_write
	fdb	file_nhacp_io_pwrite
	fdb	file_nhacp_io_seek
	fdb	file_nhacp_io_get_info
	fdb	file_nhacp_io_set_size
	fdb	file_nhacp_io_list_dir
	fdb	file_nhacp_io_get_dir_entry
file_nhacp_io_jmptab_end
file_nhacp_io_jmptab_size	equ	(file_nhacp_io_jmptab_end-file_nhacp_io_jmptab)

;
; file_nhacp_io_advance
;	Advance the I/O pointer in the FCB.
;
; Arguments --
;	D - byte count
;	Y - pointer to FCB
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
file_nhacp_io_advance
	pshs	D		; stash byte count on stack

	; First update residual count.  If we get to 0, we can
	; skip updating the data pointer, but we do need to
	; update the actual count.
	ldd	fcb_nhacp_resid,Y
	subd	,S
	std	fcb_nhacp_resid,Y
	beq	1F

	ldd	fcb_nhacp_ptr,Y
	addd	,S
	std	fcb_nhacp_ptr,Y

1	ldd	fcb_nhacp_actual,Y
	addd	,S
	std	fcb_nhacp_actual,Y

	puls	D,PC		; restore and return

;
; file_nhacp_io_advance_offset
;	Advance the I/O pointer in the FCB, including the offset.
;
; Arguments --
;	D - byte count
;	Y - pointer to FCB
;
; Returns --
;	None.
;
; Clobbers --
;	D
;
file_nhacp_io_advance_offset
	bsr	file_nhacp_io_advance

	; Add D to the 32-bit number at fcb_nhacp_offset,Y.
	addd	fcb_nhacp_offset+2,Y
	std	fcb_nhacp_offset+2,Y
	bcc	1F
	ldd	#1
	addd	fcb_nhacp_offset,Y
	std	fcb_nhacp_offset,Y
1	rts

;
; file_nhacp_io --
;	file_io routine using NHACP.
;
; Arguments --
;	X - pointer to file I/O arguments
;	U - pointer to NHACP context
;
; Returns --
;	Error status in File Control Block.
;
; Clobbers --
;	None.
;
file_nhacp_io
	pshs	A,B,X,Y,U	; save registers
	;
	; 7,S
	; 6,S	U
	; 5,S
	; 4,S	Y
	; 3,S
	; 2,S	X		; saved args pointer
	; 1,S	B
	; 0,S	A
	;
	ldy	fio_fcb,X	; Y = FCB

	;
	; Before we dispatch to the real I/O routine, stash
	; the arguments in the FCB.
	;
	ldd	fio_data,X
	std	fcb_nhacp_ptr,Y
	ldd	fio_length,X
	std	fcb_nhacp_resid,Y
	ldd	fio_offset,X
	std	fcb_nhacp_offset,Y
	ldd	fio_offset+2,X
	std	fcb_nhacp_offset+2,Y

	;
	; Clear the "actual" counts in both the args structure
	; (for return to the caller) and the running count in
	; the FCB.
	;
	M_clrd
	std	fio_actual,X
	std	fcb_nhacp_actual,Y

	pshs	Y		; stash FCB on stack
	;
	; 9,S
	; 8,S	U
	; 7,S
	; 6,S	Y
	; 5,S
	; 4,S	X		; saved args pointer
	; 3,S	B
	; 2,S	A
	; 1,S
	; 0,S			; saved FCB pointer
	;
	; Get the opcode and calculate the jump table offset.
	lda	fio_op,X	; X = opcode
	asla			; code to table offset
	cmpa	#file_nhacp_io_jmptab_size
	bhs	file_nhacp_io_einval

	; Each of the ops in question has a file descriptor at
	; the same slot in the request, so just stash it now.
	ldb	fcb_nhacp_fd,Y	; B = file descriptor
	beq	file_nhacp_io_ebadf ; 0 == invalid file descriptor
	decb			; convert to actual fdesc
	stb	nhctx_req_args,U ; stash fdesc in request

	; Jump to the handler.
	ldx	#file_nhacp_io_jmptab
	jmp	[A,X]

file_nhacp_io_einval
	lda	#EINVAL
	lbra	file_nhacp_io_error

file_nhacp_io_ebadf
	lda	#EBADF
	lbra	file_nhacp_io_error

file_nhacp_io_read
	nhacp_req_init "FILE_READ"

	ldd	fcb_nhacp_resid,Y
	lbeq	file_nhacp_io_done ; done if resid == 0
	cmpd	#NHACP_MAX_PAYLOAD
	bls	1F
	ldd	#NHACP_MAX_PAYLOAD ; clamp!
1	stb	nhctx_req_args+3
	sta	nhctx_req_args+4

	ldd	#0		; flags
	std	nhctx_req_args+1,U

	jsr	nhacp_req_send

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	lbeq	file_nhacp_io_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	lbeq	file_nhacp_io_error_reply

	; Unknown error if not DATA_BUFFER.
	cmpa	#NHACP_RESP_DATA_BUFFER
	lbne	file_nhacp_io_eio

	; Get returned length into D.
	jsr	nhacp_get_reply_byte
	lbeq	file_nhacp_io_eio	; handle receive timeout
	pshs	A		; LSB
	jsr	nhacp_get_reply_byte
	puls	B		; D now contains returned length
	lbeq	file_nhacp_io_eio	; handle receive timeout

	cmpd	#0		; actual == 0?
	lbeq	file_nhacp_io_done

	ldy	,S		; recover FCB
	ldx	fcb_nhacp_ptr,Y	; X = current data pointer
	pshs	D,Y		; preserve D,Y
	jsr	nhacp_copyin	; get data from interface
	puls	D,Y		; restore D,Y
	beq	file_nhacp_io_eio	; handle receive timeout

	lbsr	file_nhacp_io_advance
	bra	file_nhacp_io_read

file_nhacp_io_pread
	nhacp_req_init "STORAGE_GET"

	ldd	fcb_nhacp_resid,Y
	beq	file_nhacp_io_done ; done if resid == 0
	cmpd	#NHACP_MAX_PAYLOAD
	bls	1F
	ldd	#NHACP_MAX_PAYLOAD ; clamp!
1	stb	nhctx_req_args+5
	sta	nhctx_req_args+6

	; We have a 32-bit big-endian number that needs to
	; be stored in a 32-bit little-endian field.
	;
	; 0 1 2 3	0 1 2 3
	; 4 3 2 1	1 2 3 4
	ldd	fcb_nhacp_offset,Y	; A = 4, B = 3
	sta	nhctx_req_args+4
	stb	nhctx_req_args+3
	ldd	fcb_nhacp_offset+2,Y	; A = 2, B = 1
	sta	nhctx_req_args+2
	stb	nhctx_req_args+1

	jsr	nhacp_req_send

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	beq	file_nhacp_io_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	beq	file_nhacp_io_error_reply

	; Unknown error if not DATA_BUFFER.
	cmpa	#NHACP_RESP_DATA_BUFFER
	bne	file_nhacp_io_eio

	; Get returned length into D.
	jsr	nhacp_get_reply_byte
	beq	file_nhacp_io_eio	; handle receive timeout
	pshs	A		; LSB
	jsr	nhacp_get_reply_byte
	puls	B		; D now contains returned length
	beq	file_nhacp_io_eio	; handle receive timeout

	cmpd	#0		; actual == 0?
	beq	file_nhacp_io_done

	ldy	,S		; recover FCB
	ldx	fcb_nhacp_ptr,Y	; X = current data pointer
	pshs	D,Y		; preserve D,Y
	jsr	nhacp_copyin	; get data from interface
	puls	D,Y		; restore D,Y
	beq	file_nhacp_io_eio	; handle receive timeout

	lbsr	file_nhacp_io_advance_offset
	bra	file_nhacp_io_pread

;
; Tail of the I/O routines -- trying to keep them close to where
; they're referenced so that 8-bit relative branches are possible.
;
file_nhacp_io_error_reply
	; Get the error code from the reply.
	jsr	nhacp_get_reply_byte
	beq	file_nhacp_io_eio	; timeout -> EIO
	tsta
	bne	file_nhacp_io_error	; error != 0, cool cool.
					; error == 0, map to EIO
file_nhacp_io_eio
	lda	#EIO

file_nhacp_io_error
	puls	Y			; get saved FCB pointer
	bra	97F

file_nhacp_io_done
	ldx	4,S			; recover args
	puls	Y			; get saved FCB pointer
	ldd	fcb_nhacp_actual,Y	; get actual count
	std	fio_actual,X		; record it for the caller
	clra				; error = 0
97	; A = error code
	sta	fcb_error,Y
	jsr	nhacp_drain	; drain off the rest of the reply
	beq	99F		; kill session if framing error
98	puls	A,B,X,Y,U,PC	; restore and return

99	jsr	nhacp_invalidate_session ; kill session
	tst	fcb_error,Y		 ; error already set?
	bne	98B			 ; yes, just get out.
	lda	#EIO			 ; default to EIO
	sta	fcb_error,Y
	bra	98B

file_nhacp_io_write
	nhacp_req_init "FILE_WRITE"

	ldd	fcb_nhacp_resid,Y
	beq	file_nhacp_io_done ; done if resid == 0
	cmpd	#NHACP_MAX_PAYLOAD
	bls	1F
	ldd	#NHACP_MAX_PAYLOAD ; clamp!
1	stb	nhctx_req_args+3
	sta	nhctx_req_args+4
	pshs	D		; stash length on stack temporarily

	ldd	#0		; flags
	std	nhctx_req_args+1,U

	jsr	nhacp_req_send

	; send the data
	ldd	,S		; get length from stack
	ldx	fcb_nhacp_ptr,Y
	jsr	nhacp_copyout

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	beq	file_nhacp_write_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	beq	file_nhacp_write_error_reply

	; Unknown error if not OK.
	cmpa	#NHACP_RESP_OK
	bne	file_nhacp_write_eio

	; XXX NHACP's FILE-WRITE does not return the amount
	; of data actually written.  We have to assume it is
	; the full amount.
	puls	D		; pop length from stack

	lbsr	file_nhacp_io_advance
	bra	file_nhacp_io_write

file_nhacp_write_eio
	leas	1,S		; pop length off stack
	bra	file_nhacp_io_eio

file_nhacp_write_error_reply
	leas	1,S		; pop length off stack
	bra	file_nhacp_io_error_reply

file_nhacp_io_pwrite
	nhacp_req_init "STORAGE_PUT"

	ldd	fcb_nhacp_resid,Y
	beq	file_nhacp_io_done ; done if resid == 0
	cmpd	#NHACP_MAX_PAYLOAD
	bls	1F
	ldd	#NHACP_MAX_PAYLOAD ; clamp!
1	stb	nhctx_req_args+5
	sta	nhctx_req_args+6
	pshs	D		; stash length on stack temporarily

	; We have a 32-bit big-endian number that needs to
	; be stored in a 32-bit little-endian field.
	;
	; 0 1 2 3	0 1 2 3
	; 4 3 2 1	1 2 3 4
	ldd	fcb_nhacp_offset,Y	; A = 4, B = 3
	sta	nhctx_req_args+4
	stb	nhctx_req_args+3
	ldd	fcb_nhacp_offset+2,Y	; A = 2, B = 1
	sta	nhctx_req_args+2
	stb	nhctx_req_args+1

	jsr	nhacp_req_send

	; send the data
	ldd	,S		; get length from stack
	ldx	fcb_nhacp_ptr,Y
	jsr	nhacp_copyout

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	beq	file_nhacp_write_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	beq	file_nhacp_write_error_reply

	; Unknown error if not OK.
	cmpa	#NHACP_RESP_OK
	bne	file_nhacp_write_eio

	; XXX NHACP's FILE-WRITE does not return the amount
	; of data actually written.  We have to assume it is
	; the full amount.
	puls	D		; pop length from stack

	lbsr	file_nhacp_io_advance_offset
	bra	file_nhacp_io_pwrite

file_nhacp_io_seek
	nhacp_req_init "FILE_SEEK"

	ldx	4,S		; recover args pointer

	; We have a 32-bit big-endian number that needs to
	; be stored in a 32-bit little-endian field.
	;
	; 0 1 2 3	0 1 2 3
	; 4 3 2 1	1 2 3 4
	ldd	fio_offset,X	; A = 4, B = 3
	sta	nhctx_req_args+4
	stb	nhctx_req_args+3
	ldd	fio_offset+2,X	; A = 2, B = 1
	sta	nhctx_req_args+2
	stb	nhctx_req_args+1

	lda	fio_whence,X
	sta	nhctx_req_args+5

	jsr	nhacp_req_send

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	lbeq	file_nhacp_io_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	lbeq	file_nhacp_io_error_reply

	; Unknown error if not UINT32_VALUE.
	cmpa	#NHACP_RESP_UINT32_VALUE
	lbne	file_nhacp_io_eio

	; Comes back in little-endian order.  Store it back into
	; the fio_offset field in big-endian order.
	ldx	4,S		; recover args pointer
	ldb	fio_offset+3
1	jsr	nhacp_get_reply_byte
	lbeq	file_nhacp_io_eio	; handle receive timeout
	sta	B,X
	decb
	cmpb	#fio_offset
	bhs	1B

	lbra	file_nhacp_io_done

file_nhacp_io_enotsup
	lda	#ENOTSUP
	lbra	file_nhacp_io_error

file_nhacp_io_get_info
	bra	file_nhacp_io_enotsup

file_nhacp_io_set_size
	bra	file_nhacp_io_enotsup

file_nhacp_io_list_dir
	nhacp_req_init "LIST_DIR"

	ldd	fcb_nhacp_resid,Y
	tsta				; 255 is max pattern length
	lbne	file_nhacp_io_einval

	stb	nhctx_req_args+1,U	; pattern length

	jsr	nhacp_req_send

	; send pattern data, if any
	ldd	fcb_nhacp_resid,Y
	beq	1F
	ldx	fcb_nhacp_ptr,Y
	jsr	nhacp_copyout

1	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	lbeq	file_nhacp_io_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	lbeq	file_nhacp_io_error_reply

	; Unknown error if not OK.
	cmpa	#NHACP_RESP_OK
	lbne	file_nhacp_io_eio

	lbra	file_nhacp_io_done

file_nhacp_io_get_dir_entry
	nhacp_req_init "GET_DIR_ENTRY"

	ldd	fcb_nhacp_resid,Y
	cmpd	#NHACP_FILE_ATTRS_S_sz+1
	lblt	file_nhacp_io_einval	; need room for attrs + name length
					; at least
	subd	#NHACP_FILE_ATTRS_S_sz+1
	cmpd	#255			; max file name length
	bls	1F
	ldb	#255			; just saturate
1	stb	nhctx_req_args+1,U	; max file name length

	jsr	nhacp_req_send

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	lbeq	file_nhacp_io_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	lbeq	file_nhacp_io_error_reply

	; If OK, there are no more entries.  "actual" is already
	; initialized to 0.
	cmpa	#NHACP_RESP_OK
	lbeq	file_nhacp_io_done

	; Unknown error if not FILE-INFO.
	cmpa	#NHACP_RESP_FILE_INFO
	lbne	file_nhacp_io_eio

	; Check that we got a minimum sized reply (FILE-ATTRS plus
	; 1 byte of string length).
	ldd	nhctx_reply_len,U
	cmpd	#(NHACP_FILE_ATTRS_S_sz + 1)
	lblo	file_nhacp_io_eio

	; Receive the FILE-ATTRS field.
	ldy	,S		; recover FCB
	ldx	fcb_nhacp_ptr,Y	; X = current data pointer
	ldd	#NHACP_FILE_ATTRS_S_sz
	pshs	D,Y		; preserve D,Y
	jsr	nhacp_copyin	; get data from interface
	puls	D,Y		; restore D,Y
	lbeq	file_nhacp_io_eio	; handle receive timeout

	lbsr	file_nhacp_io_advance

	; Get the name length byte.
	jsr	nhacp_get_reply_byte
	lbeq	file_nhacp_io_eio	; handle receive timeout

	; Stash the name length on the stack temporarily.  We need
	; to write it to the caller's buffer and then advance the
	; pointer to be ready for the name.  But to do that, we need
	; to shuffle some registers around.
	pshs	A
	sta	[fcb_nhacp_ptr,Y]	; save name length in buffer
	ldd	#1			; D=1 (also A=0, B=1)
	jsr	file_nhacp_io_advance	; advance buffer 1 byte
	puls	B			; Now D has name length

	cmpd	nhctx_reply_len,U	; bigger than remaining reply length?
	lbhi	file_nhacp_io_eio	; yes -> I/O error

	ldx	fcb_nhacp_ptr,Y		; D already has byte count
	pshs	D,Y			; preserve D,Y
	jsr	nhacp_copyin		; get name
	puls	D,Y			; restore D,Y (CC is preserved)
	lbeq	file_nhacp_io_eio	; EIO if timed out
	jsr	file_nhacp_io_advance	; advance buffer by name length
	lbra	file_nhacp_io_done

;
; file_nhacp_close --
;	file_close routine using NHACP.
;
; Arguments --
;	X - pointer to file close arguments
;	U - pointer to NHACP context
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
file_nhacp_close
	pshs	A,B,X,Y,U	; save registers
	;
	; 7,S
	; 6,S	U
	; 5,S
	; 4,S	Y
	; 3,S
	; 2,S	X		; saved args pointer
	; 1,S	B
	; 0,S	A
	;
	ldy	fclose_fcb,X	; Y = FCB

	nhacp_req_init "FILE_CLOSE"

	lda	fcb_nhacp_fd,Y
	beq	99F		; 0 == invalid file descriptor
	deca			; convert to real fdesc
	sta	nhctx_req_args,U

	clr	fcb_nhacp_fd,Y	; invalidate the fdesc in the FCB

	jsr	nhacp_req_send

	; No reply to FILE-CLOSE.
99	puls	A,B,X,Y,U,PC	; restore and return
