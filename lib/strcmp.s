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
; strcmp
;	Compare two NUL-terminated strings.
;
; Arguments --
;	X - string1
;	Y - string2
;
; Returns --
;	CC_Z set if strings are equal, cleared if strings are not equal.
;	The arithmetic difference between the first un-matching character
;	is returned in A, or zero if the strings are equal.
;
; Clobbers --
;	None.
;
strcmp
	pshs	X,Y		; Save registers

	;
	; while (*s1 == *s2++)
	;	if (*s1++ == 0)
	;		return (0);
	; return (*(const unsigned char *)s1 - *(const unsigned char *)--s2);
	;
1	lda	,X
	suba	,Y+
	bne	99F
	lda	,X+
	bne	1B
99	puls	X,Y,PC		; restore and return
