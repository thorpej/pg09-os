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
; Client implementation of the NABU HCCA Application Communication Protocol.
;
; We're not running on a NABU, obviously, and don't have a "Home Cable
; Computer Adaptor", but we're not going to let that stop us!
;
; Refer to:
; https://github.com/NHACP-IF/NHACP-specification/blob/main/nhacp-specification.md
;

NHACP_MSG_GOODBYE		equ	$ef
NHACP_MSG_GOODBYE_len		equ	1

NHACP_MSG_STORAGE_OPEN		equ	$01
NHACP_MSG_STORAGE_OPEN_len	equ	5

NHACP_STORAGE_GET		equ	$02
NHACP_STORAGE_GET_len		equ	8

NHACP_STORAGE_PUT		equ	$03
NHACP_STORAGE_PUT_len		equ	8

NHACP_GET_DATE_TIME		equ	$04
NHACP_GET_DATE_TIME_len		equ	1

NHACP_FILE_CLOSE		equ	$05
NHACP_FILE_CLOSE_len		equ	2

NHACP_GET_ERROR_DETAILS		equ	$06
NHACP_GET_ERROR_DETAILS_len	equ	4

NHACP_STORAGE_GET_BLOCK		equ	$07
NHACP_STORAGE_GET_BLOCK_len	equ	8

NHACP_STORAGE_PUT_BLOCK		equ	$08
NHACP_STORAGE_PUT_BLOCK_len	equ	8

NHACP_FILE_READ			equ	$09
NHACP_FILE_READ_len		equ	6

NHACP_FILE_WRITE		equ	$0a
NHACP_FILE_WRITE_len		equ	6

NHACP_FILE_SEEK			equ	$0b
NHACP_FILE_SEEK_len		equ	7

NHACP_FILE_GET_INFO		equ	$0c
NHACP_FILE_GET_INFO_len		equ	2

NHACP_FILE_SET_SIZE		equ	$0d
NHACP_FILE_SET_SIZE_len		equ	6