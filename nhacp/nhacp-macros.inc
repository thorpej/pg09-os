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
; nhacp_req_init --
;	Macro to initialize the request scratch area in the
;	NHACP context.
;
; Arguments --
;	Quoted short name of NHACP request, e.g.:
;
;		nhacp_req_init "STORAGE_OPEN"
;
;	U - NHACP context
;
; Returns --
;	None.
;
; Clobbers --
;	A, B
;
nhacp_req_init		macro
	lda	#NHACP_MSG_\1
	ldb	#NHACP_MSG_\1_len
	jsr	nhacp_req_init0
	endm

;
; nhacp_timer_arm --
;	Arm the NHACP timeout timer.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
nhacp_timer_arm		macro
	clr	nhctx_timer+tmr_t0,U
	dec	nhctx_timer+tmr_t0,U	; 255 ticks - ~4.25s
	endm

;
; nhacp_timer_disarm --
;	Disarm the NHACP timeout timer.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	None.
;
; Clobbers --
;	None.
;
nhacp_timer_disarm	macro
	clr	nhctx_timer+tmr_t0,U
	endm

;
; nhacp_timer_expired --
;	Check if the NHACP timer has expired.
;
; Arguments --
;	U - NHACP context
;
; Returns --
;	CC_Z is set if the timer has expired (reached zero), cleared
;	if the timer is still pending.
;
; Clobbers --
;	None.
;
nhacp_timer_expired	macro
	tst	nhctx_timer+tmr_t0,U
	endm
