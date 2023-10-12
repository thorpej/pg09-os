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
; Fixed ram variables for the VDP TTY driver.
;

	export VDP_tty_cols, VDP_tty_rows, VDP_tty_buf_size
VDP_tty_cols			equ	40
VDP_tty_rows			equ	24
VDP_tty_buf_size		equ	(VDP_tty_cols * VDP_tty_rows)

	export VDP_tty_buf, VDP_tty_buf_dirty, VDP_tty_buf_locked
VDP_tty_buf
	rmb	VDP_tty_buf_size
VDP_tty_buf_dirty
	rmb	1
VDP_tty_buf_locked
	rmb	1

	export VDP_tty_col, VDP_tty_row, VDP_tty_pos
VDP_tty_col
	rmb	1
VDP_tty_row
	rmb	1
VDP_tty_pos
	rmb	2

	export VDP_tty_cursor_enabled, VDP_tty_cursor_savechar
	export VDP_tty_cursor_curchar
VDP_tty_cursor_enabled
	rmb	1
VDP_tty_cursor_savechar
	rmb	1
VDP_tty_cursor_curchar
	rmb	1

	export VDP_tty_zerostart, VDP_tty_zerosize
VDP_tty_zerostart		equ	VDP_tty_buf_dirty
VDP_tty_zerosize		equ	9
