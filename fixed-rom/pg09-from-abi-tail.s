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
	; System DATA linkage table.  This provides an indirect
	; reference to exported system data.  The addresses can
	; be fetched as so:
	;
	;	ldx	SysData_fs_avail
	;
	; ...and the data at that address as so:
	;
	;	ldx	[SysData_fs_avail]
	;
	if	__GENERATE_EXPORTS

SysData		macro
XSysData_\1	fdb	\2
\1
		endm

	else

SysData		macro
XSysData_\1	fdb	\2
		endm

	endif

	;
	; NOTE: THIS ORG STATEMENT MUST BE ADJUSTED EACH TIME A
	; SysData LINKAGE IS ADDED.  EACH SysData LINKAGE IS 2
	; BYTES.
	;
	org	$FFF0-(2 * 2)

	;
	; DO NOT CHANGE THE ORDER OR OF THESE STATEMENTS UNLESS YOU KNOW
	; EXACTLY WHAT YOU ARE DOING!  THESE ARE PART OF THE OS ABI!
	;
	; NOTE THAT NEW SysData STATEMENTS MUST BE ADDED TO THE BEGINNING
	; OF THE LIST (GROW AWAY FROM THE VECTOR TABLE)!
	;

	SysData "fs_avail",fs_avail
	SysData "fs_avail_end",fs_avail_end
