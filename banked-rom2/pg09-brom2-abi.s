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

	include "../lib/asm_macros.inc"
	include "../sys-api/pg09-os.exp"

	org	SysAddr_BankedROM

;
; Banked call jump table.  The order of this table defines the ABI
; of the module in this ROM bank.
;
	BCall_decl "fs_mount",fs_mount
	BCall_decl "fs_unmount",fs_unmount
	BCall_decl "fs_chdir",fs_chdir

	BCall_decl "nhacp_start_system_session",nhacp_start_system_session
	BCall_decl "nhacp_end_session",nhacp_end_session

	BCall_decl "file_nhacp_open",file_nhacp_open
	BCall_decl "file_nhacp_io",file_nhacp_io
	BCall_decl "file_nhacp_close",file_nhacp_close
