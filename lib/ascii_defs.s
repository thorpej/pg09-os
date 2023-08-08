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
; ASCII definitions
;

ASCII_NUL	equ	$00		; CTRL-@
ASCII_SOH	equ	$01		; CTRL-A -- Start of Header
ASCII_STX	equ	$02		; CTRL-B -- Start of Text
ASCII_ETX	equ	$03		; CTRL-C -- End of Text
ASCII_EOT	equ	$04		; CTRL-D -- End of Transmission
ASCII_ENQ	equ	$05		; CTRL-E -- Enquire
ASCII_ACK	equ	$06		; CTRL-F -- Acknowledge
ASCII_BEL	equ	$07		; CTRL-G -- Bell
ASCII_BS	equ	$08		; CTRL-H -- Backspace
ASCII_HT	equ	$09		; CTRL-I -- (Horizontal) Tab
ASCII_LF	equ	$0a		; CTRL-J -- Line Feed
ASCII_VT	equ	$0b		; CTRL-K -- Vertical Tab
ASCII_FF	equ	$0c		; CTRL-L -- Form Feed
ASCII_CR	equ	$0d		; CTRL-M -- Carraige Return
ASCII_SO	equ	$0e		; CTRL-N -- Shift Out
ASCII_SI	equ	$0f		; CTRL-O -- Shift In
ASCII_DLE	equ	$10		; CTRL-P -- Data Link Escape
ASCII_DC1	equ	$11		; CTRL-Q -- Device Control 1 - resume
ASCII_XON	equ	ASCII_DC1
ASCII_DC2	equ	$12		; CTRL-R -- Device Control 2
ASCII_DC3	equ	$13		; CTRL-S -- Device Control 3 - pause
ASCII_XOFF	equ	ASCII_DC3
ASCII_DC4	equ	$14		; CTRL-T -- Device Control 4
ASCII_NAK	equ	$15		; CTRL-U -- Negative Acknowledge
ASCII_SYN	equ	$16		; CTRL-V -- Sync
ASCII_ETB	equ	$17		; CTRL-W -- End of Text Block
ASCII_CAN	equ	$18		; CTRL-X -- Cancel
ASCII_EM	equ	$19		; CTRL-Y -- End of Message
ASCII_SUB	equ	$1a		; CTRL-Z -- Substitute
ASCII_ESC	equ	$1b		; CTRL-[ -- Escape
ASCII_FS	equ	$1c		; CTRL-| -- Field Separator
ASCII_GS	equ	$1d		; CTRL-] -- Group Separator
ASCII_RS	equ	$1e		; CTRL-^ -- Record Separator
ASCII_US	equ	$1f		; CTRL-_ -- Unit Separator
ASCII_SPACE	equ	$20
ASCII_SQUOTE	equ	$27
ASCII_DQUOTE	equ	$22
