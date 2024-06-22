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

;
; ROM Bank 2 image for the 6809 Playground.
;

	include "../drivers/mc6809/mc6809_defs.s"
	include "../lib/asm_macros.inc"

	include "../fixed-ram/pg09-fram.exp"

	include "../sys-api/pg09-os.exp"
	include "../sys-api/file-api.exp"
	include "../sys-api/timer-api.exp"

	include "../banked-rom1/pg09-brom1.exp"

	include	"pg09-brom2-abi.s"	; sets origin

	setdp	-1	; Disable automatic direct page addressing

	CODE

	include "../lib/memcpy8.s"
	include "../lib/memzero8.s"
	include "../lib/puts.s"
	include "../lib/printdec.s"
	include "../lib/printhex.s"
	include "../lib/strcmp.s"
	include "../lib/udiv16.s"

	include "../nhacp/nhacp.exp"
	include "../nhacp/nhacp-proto.inc"
	include "../nhacp/nhacp-macros.inc"
	include "../nhacp/nhacp-util.s"
	include "../nhacp/file-nhacp.s"

;
; fs_atindex
;	Look up and return the file system at the specified
;	drive index.
;
; Arguments --
;	A - drive specifier for the mount (A: == 1)
;
; Returns --
;	X - pointer to fsops for mounted file system, NULL if none at
;	that drive index.
;
; Clobbers --
;	A
;
fs_atindex
	tsta
	beq	99F		; invalid drive specifier
	cmpa	#fs_maxdrives
	bhi	99F		; invalid drive specifier
	deca			; convert to 0-based index
	asla			; index to table offset
	ldx	#fs_drives
	ldx	A,X		; X = ops at drive slot
	rts
99
	ldx	#0
	rts

;
; fs_getcur
;	Look up and return the current file system (drive).
;
; Arguments --
;	None.
;
; Returns --
;	X - pointer to fsops for the current drive, NULL if no drive
;	currently selected.
;
; Clobbers --
;	None.
;
fs_getcur
	pshs	A
	lda	fs_curdrive
	bsr	fs_atindex
	puls	A,PC

;
; fs_setcur
;	Set the current file system (drive).
;
; Arguments --
;	A - drive specifier for the mount (A: == 1)
;
; Returns --
;	Sets Z if there is no file system at the specified slot,
;	or if the slot is invalid.  Clears Z upon success.
;
; Clobbers --
;	None.
;
fs_setcur
	pshs	A,X

	tsta
	beq	99F		; invalid drive specifier
	cmpa	#fs_maxdrives
	bhi	99F		; invalid drive specifier

	deca			; convert to 0-based index
	asla			; index to table offset
	ldx	#fs_drives
	ldx	A,X		; X = fsops pointer at specified slot
	beq	99F		; no FS at this slot

	lda	,S		; recover argument
	sta	fs_curdrive	; set as current drive
	andcc	#~CC_Z		; clear Z to indicate success
98	puls	A,X,PC
99
	orcc	#CC_Z		; set Z to indicate failure
	bra	98B

;
; fs_mount
;	Mount a file system.
;
; Arguments --
;	X - pointer to NUL-terminated device name of fsops representing
;	the file system to mount.
;
;	A - drive specifier for the mount (A: == 1)
;
; Returns --
;	A - error code if mount fails (0 == no error, mount succeeded)
;
; Clobbers --
;	None.
;
fs_mount
	pshs	A,B,X,Y

	; Look up the fsops by the provided name.
	ldx	SysData_fs_avail
	pshs	X
	;
	; 4,S	device name pointer argument
	; 3,S	saved B
	; 2,S	saved A
	; 0,S	pointer to current slot in fs_avail
	;
1	cmpx	SysData_fs_avail_end
	lbeq	fs_mount_esrch	; file system not found

	; Get the name pointer for the next candidate.  Since fsov_devname
	; is the first field in the struct, we can use indirect addressing
	; to load it.
	ldy	[,X++]		; also advance to next slot
	stx	,S		; remember next slot
	ldx	4,S		; recover devname argument
	jsr	strcmp		; compare names
	beq	1F		; found a match!
	ldx	,S		; recover next slot
	bra	1B		; go try again

1	; Pull the next slot back into X, and walk back to the slot
	; that matched and load that pointer into X.
	puls	X
	ldx	,--X

	; X now points to the matching fsops.

	; Now, check to see if the fsops provided is currently
	; mounted somewhere else.
	ldy	#fs_drives
	clrb
1	cmpx	B,Y		; X same as fs_drives[idx]?
	beq	fs_mount_ebusy
	addb	#2		; next table offset
	cmpb	#(fs_maxdrives * 2)
	beq	1F		; finished scanning
	bra	1B
1
	; Next, check to see if something is already mounted in this slot.
	ldb	,S		; Get drive specifier
	beq	fs_mount_einval	; invalid drive specifier
	cmpb	#fs_maxdrives
	bhi	fs_mount_einval	; invalid drive specifier
	decb			; make 0-based index
	aslb			; convert to table offset
	tst	B,Y		; check the slot
	bne	fs_mount_ebusy	; something already there.

	; Ok, everything looks good.  Try to mount the file system.
	jsr	[fsov_mount,X]
	tsta			; error return?
	bne	fs_mount_error

	; Success!  Record the mount in the table.
	stx	B,Y

	; If no drive is currently selected, then select this one we
	; just mounted.
	lda	fs_curdrive
	bne	98F
	lda	,S		; Get drive specifier
	sta	fs_curdrive

	; Since we just selected this as the current drive, change the
	; current working directory to "/" on that drive.
	lda	#'/'
	sta	,-S		; S now points to the string "/"
	lda	#1		; A = length of string (1)
	tfr	S,X		; X = "/"
	jsr	fs_chdir	; Go do it.
	leas	1,S		; pop the 1 byte path.
	tsta			; Did an error occur?
	beq	98F		; nope, we are all set.
	tfr	A,B		; save error code

	lda	#'@'
	adda	fs_curdrive
	jsr	[SysSubr_cons_putc]
	jsr	iputs
	fcn	":/ - "
	tfr	B,A		; recover error code
	BCall	"errorstr_print"
	jsr	puts_crlf
	clr	fs_curdrive	; clear the current drive on error.

98	clr	,S		; return 0 (no error) in A
99	puls	A,B,X,Y,PC	; restore and return

fs_mount_error
	sta	,S		; error into A stack slot
	bra	99B

fs_mount_ebusy
	lda	#EBUSY
	bra	fs_mount_error

fs_mount_einval
	lda	#EINVAL
	bra	fs_mount_error

fs_mount_esrch
	leas	2,S		; pop temp from stack
	lda	#ESRCH
	bra	fs_mount_error

;
; fs_ummount
;	Unmount a file system.
;
; Arguments --
;	A - drive specifier for the mount (A: == 1)
;
; Returns --
;	A - error code if unmount fails (0 == no error, unmount succeeded)
;
; Clobbers --
;	None.
;
fs_unmount
	pshs	A,X,Y

	tst	A
	beq	fs_unmount_einval
	cmpa	#fs_maxdrives
	bhi	fs_unmount_einval

	deca			; convert to 0-based index
	asla			; index to table offset
	ldx	#fs_drives
	leax	A,X		; X points to drive slot
	ldy	,X		; Y = fsops of file system
	beq	fs_unmount_esrch

	jsr	[fsov_unmount,Y]
	clr	,X+		; zero out the drive slot
	clr	,X+

	; If we just unmounted the current drive, clear the current
	; drive and invalidate the cwd_fcb.
	lda	fs_curdrive
	cmpa	,S
	bne	1F
	clr	fs_curdrive
	lda	#fcb_fcbsz
	ldx	#cwd_fcb
	jsr	memzero8

1	clr	,S		; return 0 (no error) in A
99	puls	A,X,Y,PC	; restore and return

fs_unmount_error
	sta	,S
	bra	99B

fs_unmount_einval
	lda	#EINVAL
	bra	fs_unmount_error

fs_unmount_esrch
	lda	#ESRCH
	bra	fs_unmount_error

;
; fs_chdir
;	Change the current working directory.
;
; Arguments --
;	A - length of path string.
;	X - pointer to path string.
;
; Returns --
;	A - error code
;
; Clobbers --
;	None.
;
fs_chdir
	pshs	B,X,Y
	;
	; 4,S
	; 3,S	saved Y
	; 2,S
	; 1,S	saved X
	; 0,S	saved B
	;
	; XXX No drive letter processing yet.
	;
	ldx	#monitor_fargs
	sta	fopen_namelen,X	; stash the name length, D now free
	ldy	#monitor_fcb	; Y = FCB
	sty	fopen_fcb,X	; FCB always at offset 0

	; Initialize the open arguments.
	ldd	1,S		; pointer to path
	std	fopen_name,X
	ldd	#O_RDONLY+O_DIRECTORY
	std	fopen_flags,X

	; Open the directory.
	jsr	[SysSubr_file_open]
	lda	fcb_error,Y
	bne	99F		; get out now if error occurred.

	; Now that the new CWD is open, close the existing one.
	; X still points to the arguments buffer.
	ldy	#cwd_fcb
	sty	fclose_fcb,X
	jsr	[SysSubr_file_close]

	; Now copy monitor FCB to the CWD FCB, and zap the monitor FCB.
	ldx	#cwd_fcb
	ldy	#monitor_fcb
	lda	#fcb_fcbsz
	jsr	memcpy8
	ldx	#monitor_fcb
	jsr	memzero8

	clra			; success!

99	puls	B,X,Y,PC	; restore and return
