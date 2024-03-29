;
; Copyright (c) 2022 Jason R. Thorpe.
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
; Some basic 6809 definitions.
;

	if	mc6809_defs_included
	else
mc6809_defs_included	equ	1

;
; Condition codes
;
CC_C		equ	(1 << 0)	; carry
CC_V		equ	(1 << 1)	; overflow
CC_Z		equ	(1 << 2)	; zero
CC_N		equ	(1 << 3)	; negative
CC_I		equ	(1 << 4)	; IRQ mask
CC_H		equ	(1 << 5)	; half-carry
CC_F		equ	(1 << 6)	; FIRQ mask
CC_E		equ	(1 << 7)	; entire

;
; Interrupt frame offsets
;
; IFE_ -> Interrupt Frame Entire
;
;    [higest address]
;	PC
;	U
;	Y
;	X
;	DP
;	B
;	A
;	CCR
;    [lowest address]
;
IFE_CCR		equ	0
IFE_A		equ	1
IFE_B		equ	2
IFE_DP		equ	3
IFE_X		equ	4
IFE_Y		equ	6
IFE_U		equ	8
IFE_PC		equ	10
IFE_SIZE	equ	(IFE_PC + 2)

;
; IFF_ -> Interrupt Frame Fast
;
;    [highest address]
;
;	PC
;	CCR
;
;    [lowest address]
;
IFF_CCR		equ	0
IFF_PC		equ	1
IFF_SIZE	equ	(IFF_PC + 2)

	endif	; mc6809_defs_included
