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
;	https://github.com/NHACP-IF/NHACP-specification
;

NHACP_MAX_PAYLOAD		equ	8192

NHACP_MSG_GOODBYE		equ	$ef
NHACP_MSG_GOODBYE_len		equ	1

NHACP_MSG_STORAGE_OPEN		equ	$01
NHACP_MSG_STORAGE_OPEN_len	equ	5

NHACP_MSG_STORAGE_GET		equ	$02
NHACP_MSG_STORAGE_GET_len	equ	8

NHACP_MSG_STORAGE_PUT		equ	$03
NHACP_MSG_STORAGE_PUT_len	equ	8

NHACP_MSG_GET_DATE_TIME		equ	$04
NHACP_MSG_GET_DATE_TIME_len	equ	1

NHACP_MSG_FILE_CLOSE		equ	$05
NHACP_MSG_FILE_CLOSE_len	equ	2

NHACP_MSG_GET_ERROR_DETAILS	equ	$06
NHACP_MSG_GET_ERROR_DETAILS_len	equ	4

NHACP_MSG_STORAGE_GET_BLOCK	equ	$07
NHACP_MSG_STORAGE_GET_BLOCK_len	equ	8

NHACP_MSG_STORAGE_PUT_BLOCK	equ	$08
NHACP_MSG_STORAGE_PUT_BLOCK_len	equ	8

NHACP_MSG_FILE_READ		equ	$09
NHACP_MSG_FILE_READ_len		equ	6

NHACP_MSG_FILE_WRITE		equ	$0a
NHACP_MSG_FILE_WRITE_len	equ	6

NHACP_MSG_FILE_SEEK		equ	$0b
NHACP_MSG_FILE_SEEK_len		equ	7

NHACP_MSG_FILE_GET_INFO		equ	$0c
NHACP_MSG_FILE_GET_INFO_len	equ	2

NHACP_MSG_FILE_SET_SIZE		equ	$0d
NHACP_MSG_FILE_SET_SIZE_len	equ	6

NHACP_MSG_LIST_DIR		equ	$0e
NHACP_MSG_LIST_DIR_len		equ	3

NHACP_MSG_GET_DIR_ENTRY		equ	$0f
NHACP_MSG_GET_DIR_ENTRY_len	equ	3

;
; REMOVE, RENAME, and MKDIR are namespace ops, and they're handled
; differently.  These request lengths do not include the STRING
; fields.
;

NHACP_MSG_REMOVE		equ	$10
NHACP_MSG_REMOVE_len		equ	3

NHACP_MSG_RENAME		equ	$11
NHACP_MSG_RENAME_len		equ	1

NHACP_MSG_MKDIR			equ	$12
NHACP_MSG_MKDIR_len		equ	1

NHACP_RESP_SESSION_STARTED	equ	$80

NHACP_RESP_OK			equ	$81

NHACP_RESP_ERROR		equ	$82

NHACP_RESP_STORAGE_LOADED	equ	$83

NHACP_RESP_DATA_BUFFER		equ	$84

NHACP_RESP_DATE_TIME		equ	$85

NHACP_RESP_FILE_INFO		equ	$86

NHACP_RESP_UINT8_VALUE		equ	$87

NHACP_RESP_UINT16_VALUE		equ	$88

NHACP_RESP_UINT32_VALUE		equ	$89

NHACP_RESP_FILE_ATTRS		equ	$8a
