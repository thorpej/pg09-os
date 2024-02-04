;
; Copyright (c) 2022, 2023 Jason R. Thorpe.
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
; General assembly macros
;

;
; Declare a Banked Call jump table entry.  These entries will then
; be used by gen-bcalls.sh to generate the Banked Call Descriptors
; that can be used by consumers of the banked call routine.  The
; order and location of these entries in a source code file will
; determine the ABI for that banked ROM module.  It is recommended
; that BCall_decl entries start at the beginning or end of the
; Banked ROM address range and grow towards the middle.
;
BCall_decl	macro
BCall_\1	fdb     \1
		endm

;
; Invoke a Banked ROM Call.
;
BCall		macro
		jsr	[SysSubr_brom_call]
		fcc	BCall_\1_bank
		fdb	BCall_\1_slot
		endm

;
; Macros for some 6309 instructions that we can emulate on the 6809
; with multi-instruction sequences.
;
	if	CONFIG_6309

M_asld		macro
		asld
		endm

M_clrd		macro
		clrd
		endm

	else	; 6809

M_asld		macro
		aslb
		rola
		endm

M_clrd		macro
		clra		; Could use LDD #0, but that's 1 byte
		clrb		; longer despite being 1 cycle faster
		endm

	endif	; CONFIG_6309
