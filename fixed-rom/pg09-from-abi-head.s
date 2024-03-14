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

	include "../../pg09-system/asm/pg09_defs.s"
	include "../lib/asm_macros.inc"

	;
	; System SUBROUTINE jump table.  These are called like normal
	; subroutines, not with SWI as with a system call, i.e.:
	;
	;	jsr	[SysSubr_cons_getc]
	;
	if	__GENERATE_EXPORTS

SysSubr		macro
XSysSubr_\1	fdb	\2
\1
		endm

	else

SysSubr		macro
XSysSubr_\1	fdb	\2
		endm

	endif

	;
	; DO NOT CHANGE THE ORDER OR THE ORIGIN OF THESE STATEMENTS UNLESS
	; YOU KNOW EXACTLY WHAT YOU ARE DOING!  THESE ARE PART OF THE OS ABI!
	;
	org	FROM_START

	SysSubr "exit",exit

	SysSubr "brom_call",brom_call
	SysSubr "brom_switch",brom_switch
	SysSubr "lbram_switch",lbram_switch
	SysSubr "hbram_switch",hbram_switch

	SysSubr "cons_getc",cons_getc
	SysSubr "cons_pollc",cons_pollc
	SysSubr "cons_putc",cons_putc
	SysSubr "cons_getline",cons_getline

	SysSubr "display_get_count",display_get_count
	SysSubr "display_get_default",display_get_default
	SysSubr "display_get_descriptor",display_get_descriptor
	SysSubr "display_acquire",display_acquire
	SysSubr "display_release",display_release

	SysSubr "file_open",file_open
	SysSubr "file_io",file_io
	SysSubr "file_close",file_close

	SysSubr "timer_add",timer_add
	SysSubr "timer_remove",timer_remove

	;
	; System ADDRESS equates.  These are the exported names of
	; important system addresses that programs might need to
	; care about.
	;
	; Programs should not care about Fixed RAM (reserved for
	; the operating system), nor should they care about Fixed
	; ROM (only use the exported jump table slot addresses).
	;
SysAddr		macro
XSysAddr_\1	equ	\2
XSysAddr_\1_size equ	\3
		endm
	
	SysAddr "LowBankedRAM0",LBRAM0_START,LBRAM0_SIZE
	SysAddr "LowBankedRAM1",LBRAM1_START,LBRAM1_SIZE
	SysAddr "LowBankedRAM",LBRAM0_START,LBRAM0_SIZE+LBRAM1_SIZE
	SysAddr "HighBankedRAM",HBRAM_START,HBRAM_SIZE
	SysAddr "BankedROM",BROM_START,BROM_SIZE
