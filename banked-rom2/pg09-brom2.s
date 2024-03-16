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
; ROM Bank 2 image for the 6809 Playground.
;

	include "../drivers/mc6809/mc6809_defs.s"
	include "../lib/asm_macros.inc"

	include "../sys-api/pg09-os.exp"
	include "../sys-api/file-api.exp"
	include "../sys-api/timer-api.exp"

	include	"pg09-brom2-abi.s"	; sets origin

	setdp	-1	; Disable automatic direct page addressing

	CODE

	include "../lib/memcpy8.s"
	include "../lib/puts.s"
	include "../lib/printdec.s"
	include "../lib/printhex.s"
	include "../lib/udiv16.s"

	include "../nhacp/nhacp.exp"
	include "../nhacp/nhacp-proto.inc"
	include "../nhacp/nhacp-macros.inc"
	include "../nhacp/nhacp-util.s"
	include "../nhacp/file-nhacp.s"
