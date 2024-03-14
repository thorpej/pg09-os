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
; Fixed RAM locations used by 6809 Playground operating system.
;

	include "../../pg09-system/asm/pg09_defs.s"
	include "../drivers/mc6809/mc6809_defs.s"
	include "../sys-api/file-api.exp"
	include "../sys-api/timer-api.exp"

	org	FRAM_START

;
; getline() input buffer:
;
;	uint8_t		len
;	char		bytes[255]
;
; Note these must not cross a 256 byte boundary as getline() uses
; Direct Page addressing.
;
	export	getline_maxcnt,getline_cnt,getline_buf
getline_maxcnt	equ	254	; leaves room for terminating NUL
getline_cnt	rmb	1
getline_buf	rmb	getline_maxcnt+1

;
; IRQ Vector Table
;
; This contains the addresses if 16 IRQ handlers, one for each possible IRQ
; signal.  The interrupt controller hardware automatically loads the handler
; from this table when the CPU performs the IRQ vector fetch.
;
; N.B. THIS MUST BE ALIGNED TO A 32-BYTE BOUNDARY!
;
	export	irq_vectab
irq_vectab	rmb	32

	export	mem_access_addr,mem_access_len
mem_access_addr	rmb	2
mem_access_len	rmb	2

	export	jump_addr
jump_addr	rmb	2

;
; current_iframe points to the current interrupt frame.  This variable
; is maintained by the warm boot routine and the NMI and SWI* handlers.
;
	export	current_iframe
current_iframe	rmb	2
	export	can_continue
can_continue	rmb	1

;
; Number of low RAM and high RAM chips.
;
	export	lbram_nchips,hbram_nchips
lbram_nchips	rmb	1
hbram_nchips	rmb	1

;
; File system "drive" letters A-H (8 total file system mounts).
; Each slot is a pointer to the fsops for the mounted file system,
; which in turn points to the device name, file system name, and
; the fileops.
;
; "curdrive" is a 1-based index into the table; 0 means "no drive
; is currently selected".
;
	export	fs_drives, fs_curdrive, fs_maxdrives
fs_maxdrives	equ	8
fs_drives	rmb	(fs_maxdrives * 2)
fs_curdrive	rmb	1

;
; Timer API maintains a list of timers registered with the system.
;
	export	timer_list
timer_list
	rmb	2

;
; Pull in RAM variables used by various device drivers.
;
	if CONFIG_CONSOLE_TL16C550
	include "../drivers/tl16c550/tl16c550.exp"
	include "../drivers/tl16c550/tl16c550-cons-ramvars.s"
	endif

	include "../drivers/display/display-ramvars.s"
	if CONFIG_DISPLAY_TMS9918A
	include "../drivers/tms9918a/tms9918a-ramvars.s"
	include "../drivers/tms9918a/tms9918a-tty-ramvars.s"
	endif

	if CONFIG_NHACP
	include "../nhacp/nhacp.exp"
	endif
	if CONFIG_NHACP_TL16C550
	include "../drivers/tl16c550/tl16c550.exp"
	include "../drivers/tl16c550/tl16c550-nhacp-ramvars.s"
	elsif CONFIG_NHACP_W65C51
	include "../drivers/w65c51/w65c51-nhacp-ramvars.s"
	endif

;
; The monitor keeps an FCB and a file I/O args block for its own usage.
;
	export	monitor_fcb, monitor_fargs
monitor_fcb	rmb	fcb_fcbsz
monitor_fargs	rmb	fio_argsz

;
; This FCB represents the current working directory.
;
	export	cwd_fcb
cwd_fcb		rmb	fcb_fcbsz

;
; The monitor keeps a 512 byte scratch buffer for its own usage.
;
	export	monitor_scratchbuf
monitor_scratchbuf rmb	512

; Put the monitor's interrupt frame at the top of FRAM.  This
; interrupt frame has an extra 2 bytes at the top that point
; to warm_boot() (as a return address).
	export	MONITOR_IFRAME
MONITOR_IFRAME	equ	((FRAM_START+FRAM_SIZE)-(IFE_SIZE+2))

; Put the standard kernel stack just below the monitor iframe.
	export	KSTACK_TOP
KSTACK_TOP	equ	MONITOR_IFRAME
