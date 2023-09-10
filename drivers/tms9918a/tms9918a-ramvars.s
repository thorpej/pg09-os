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
; Here we declare RAM variables for the VDP driver that need to be declared
; /somewhere/.
;

;
; Shadow copies of the write-only registers.  Each is followed by
; the value that needs to be written to the VDP to actually write
; the register, making it easy to blast the value into the chip.
;
	export	VDP_reg_r0, VDP_reg_r1, VDP_reg_ntba, VDP_reg_ctba
	export	VDP_reg_pgba, VDP_reg_staba, VDP_reg_spgba, VDP_reg_color
VDP_reg_r0			rmb	2
VDP_reg_r1			rmb	2
VDP_reg_ntba			rmb	2
VDP_reg_ctba			rmb	2
VDP_reg_pgba			rmb	2
VDP_reg_staba			rmb	2
VDP_reg_spgba			rmb	2
VDP_reg_color			rmb	2

;
; VRAM addresses of the various tables, for reference later.
; N.B. VDP_mode_switch() relies on these immediately following
; the shadow registers.
;
	export	VDP_Name_Table, VDP_Color_Table, VDP_Pattern_Table
	export	VDP_Sprite_Attribute_Table, VDP_Sprite_Pattern_Table
VDP_Name_Table			rmb	2
VDP_Color_Table			rmb	2
VDP_Pattern_Table		rmb	2
VDP_Sprite_Attribute_Table	rmb	2
VDP_Sprite_Pattern_Table	rmb	2

;
; Application-specific display re-draw handler, called from the
; VDP interrupt handler.
;
	export	VDP_vsync_handler
VDP_vsync_handler		rmb	2

;
; Flag indicating if the VDP has been aquired by an application.
;
	export	VDP_acquired
VDP_acquired			rmb	1
