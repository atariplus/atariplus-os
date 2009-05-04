;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: diskinterf.asm,v 1.3 2003/04/02 21:15:07 thor Exp $		**
;;; **									**
;;; ** In this module:	 Resident disk handler				**
;;; **********************************************************************

	.include "diskinterf.i"
	.include "kernel.i"
	.include "sio.i"

	.segment  "OsHi"
	
;;; *** DiskInit
;;; *** Setup the resident disk handler interface
	.global DiskInit
.proc	DiskInit
	lda #$a0		; initialize very conservative disk timeout
	sta DiskTimeOut
	lda #$80		; setup the sector size to 128 bytes
	sta DiskSectorSzLo
	lda #$00
	sta DiskSectorSzHi
	rts
.endproc
;;; *** DiskInterf
;;; *** This is the so called "resident disk handler",
;;; *** though there is very litte additional support
	.global DiskInterf
.proc DiskInterf
	lda #'1'
	sta SIODeviceId		; setup the device ID to disk
	ldx SIOCommand          ; get command id
	lda DiskTimeOut		; default timeout for format
	cpx #$21		; check for all types of format commands
	beq format
	cpx #$22		; also Happy fast, single, enhanced
	beq format
	lda #$07		; otherwise, seven seconds
format:
	sta SIOTimeout		; setup the timeout
	lda #$40		; direction:	from drive into ram
	cpx #'P'		; put?
	beq writecmd
	cpx #'W'		; or with verify?
	bne readcmd
writecmd:
	asl a			; is a write command
readcmd:
	sta SIOStatus		; setup the write command
	ldy DiskSectorSzLo	; sector size low
	lda DiskSectorSzHi	; sector size hi
	cpx #'S'		; status command?
	bne notstatus
	lda #<DiskStatus	; hijack the buffer towards the disk status command
	sta SIOBufferLo
	lda #>DiskStatus
	sta SIOBufferHi
	ldy #$04		; transfer size is four bytes
	lda #$00		; hi
notstatus:
	sty SIOSizeLo		; size low
	sta SIOSizeHi		; size hi
	jsr SIOVector		; run SIO now
	bmi exit		; on error, bail out immediately
	ldx SIOCommand		; check for the command now
	cpx #'S'		; was a status command?
	bne noreadstatus
	lda DiskStatus+2	; read the device timeout
	sta DiskTimeOut		; reset the timeout
noreadstatus:
	cpx #$21		; format?
	beq readformat
	cpx #$22		; or enhanced format?
	bne exit
readformat:
	lda SIOBufferLo
	sta DiskBufferLo
	lda SIOBufferHi
	sta DiskBufferHi
	ldy #$00		; start in front of buffer
checkbad:			; check for bad sectors
	lda (DiskBufferLo),y
	iny
	and (DiskBufferLo),y
	iny			; check whether this is the last bad sector?
	cmp #$ff		; if not, continue
	bne checkbad
	dey
	dey
	sty SIOSizeLo		; keep twice # of bad sectors here
	lda #$00
	sta SIOSizeHi
exit:
	ldy SIOStatus		; return status and set N flag
	rts
.endproc
	
