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
; file_nhacp_open --
;	file_open routine using NHACP.
;
; Arguments --
;	X - pointer to file open arguments
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
	ldu	fcb_nhacp_ctx,Y	; U = NHACP context

	nhacp_req_init "STORAGE_OPEN"

	lda	#$FF		; server chooses fdesc
	sta	nhctx_req_args,U
	sta	fcb_nhacp_fd,Y	; default to invalid fdesc on error

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

	jsr	nhacp_req_send

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	beq	file_nhacp_io_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	beq	file_nhacp_io_error_reply

	; Unknown error if not STORAGE_LOADED.
	cmpa	#NHACP_RESP_STORAGE_LOADED
	bne	file_nhacp_io_eio

	; Get the file descriptor.  We don't care about
	; the rest of the reply.
	jsr	nhacp_get_reply_byte
	ldy	,S		; get saved FCB pointer (but don't pop)
	sta	fcb_nhacp_fd,Y	; save fdesc in FCB
	bra	file_nhacp_io_done

file_nhacp_io_enotsup
	lda	#ENOTSUP
	bra	file_nhacp_io_error

file_nhacp_io_einval
	lda	#EINVAL
	bra	file_nhacp_io_error

file_nhacp_io_error_reply
	; Get the error code from the reply.
	jsr	nhacp_get_reply_byte
	bne	file_nhacp_io_error	; error != 0, cool cool.
					; error == 0, map to EIO
file_nhacp_io_eio
	lda	#EIO
	bra	file_nhacp_io_error

file_nhacp_io_done
	clra			; error = 0
file_nhacp_io_error
	; A = error code
	puls	Y		; get saved FCB pointer
	sta	fcb_error,Y
	jsr	nhacp_drain	; drain off the rest of the reply
	puls	A,B,X,Y,U,PC	; restore and return

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
; file_nhacp_io --
;	file_io routine using NHACP.
;
; Arguments --
;	X - pointer to file I/O arguments
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
	ldu	fcb_nhacp_ctx,Y	; U = NHACP context

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
	lda	fio_io,X	; X = opcode
	asla			; code to table offset
	cmpa	#file_nhacp_io_jmptab_size
	bhs	file_nhacp_io_einval

	; We're going to lose track of the FCB when we do the
	; jump, but the only thing we really need from it is
	; the file descriptor, so just stash that in the request
	; structure now.
	ldb	fcb_nhacp_fd,Y	; B = file descriptor
	stb	nhctx_req_args,U ; stash fdesc in request

	; Jump to the handler.
	ldy	#file_nhacp_io_jmptab
	jsr	[A,Y]

file_nhacp_io_read
	nhacp_req_init "FILE_READ"

	ldd	#0		; flags
	std	nhctx_req_args+1

	ldd	fio_length,X
	beq	file_nhacp_io_done ; done early if length == 0
	stb	nhctx_req_args+3
	sta	nhctx_req_args+4

file_nhacp_io_read_common
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
	pshs	A		; LSB
	jsr	nhacp_get_reply_byte
	puls	B		; D now contains returned length

	; Recover arg pointer and read the data into the caller's
	; buffer.
	ldx	4,S
	std	fio_actual,X
	beq	1F		; D == 0, no data to copy in.
	ldx	fio_data,X
	jsr	nhacp_copyin
1	bra	file_nhacp_io_done

file_nhacp_io_pread
	nhacp_req_init "STORAGE_GET"

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

	ldd	fio_length,X
	beq	file_nhacp_io_done ; done early if length == 0
	stb	nhctx_req_args+5
	sta	nhctx_req_args+6

	bra	file_nhacp_io_read_common

file_nhacp_io_write
	nhacp_req_init "FILE_WRITE"

	ldd	#0		; flags
	std	nhctx_req_args+1

	ldd	fio_length,X
	beq	file_nhacp_io_done ; done early if length == 0
	stb	nhctx_req_args+3
	sta	nhctx_req_args+4

file_nhacp_io_write_common
	jsr	nhacp_req_send

	; send the data
	ldd	fio_length,X
	ldx	fio_data,X
	jsr	nhacp_copyout

	; Receive the reply.
	jsr	nhacp_get_reply_hdr
	beq	file_nhacp_io_eio

	; Check for ERROR reply.
	lda	nhctx_reply_type,U
	cmpa	#NHACP_RESP_ERROR
	beq	file_nhacp_io_error_reply

	; Unknown error if not OK.
	cmpa	#NHACP_RESP_OK
	bne	file_nhacp_io_eio

	; XXX NHACP's FILE-WRITE does not return the amount
	; of data actually written.  We have to assume it is
	; the full amount.

	; Recover arg pointer and read the data into the caller's
	; buffer.
	ldx	4,S
	ldd	fio_length,X
	std	fio_actual,X
	bra	file_nhacp_io_done

file_nhacp_io_pwrite
	nhacp_req_init "STORAGE_PUT"

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

	ldd	fio_length,X
	beq	file_nhacp_io_done ; done early if length == 0
	stb	nhctx_req_args+5
	sta	nhctx_req_args+6

	bra	file_nhacp_io_write_common

file_nhacp_io_seek
	bra	file_nhacp_io_enotsup

file_nhacp_io_get_info
	bra	file_nhacp_io_enotsup

file_nhacp_io_set_size
	bra	file_nhacp_io_enotsup

file_nhacp_io_list_dir
	bra	file_nhacp_io_enotsup

file_nhacp_io_get_dir_entry
	bra	file_nhacp_io_enotsup

;
; file_nhacp_close --
;	file_close routine using NHACP.
;
; Arguments --
;	X - pointer to file close arguments
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
	ldu	fcb_nhacp_ctx,Y	; U = NHACP context

	nhacp_req_init "FILE_CLOSE"

	lda	fcb_nhacp_fd,Y
	sta	nhctx_req_args,U

	jsr	nhacp_req_send

	; No reply to FILE-CLOSE.
	puls	A,B,X,Y,U,PC	; restore and return
