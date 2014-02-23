;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: menuconfig.asm,v 1.4 2013/06/02 20:41:05 thor Exp $		**
;;; **									**
;;; ** In this module:	 Configuration menu for DUP 2.++		**
;;; **********************************************************************

	.include "menujmptab.i"
	.include "fms.i"
	.include "mathpack.i"
	.include "errors.i"
	.include "reset.i"
	.segment "menufunction"

;;; Init and entry points
	rts
	jmp SetConfig
	jmp ShowConfig
;;; *** Set the DOS Configuration menus
.proc	SetConfig
	jsr BlockDisplay
	ldx #<ConfigTitle
	ldy #>ConfigTitle
	lda #ConfigTitleL
	jsr Print
	lda FmsBuffers
	sta Z2Ptr
	lda #1
	sta Z3Ptr
	lda #8
	sta Z3Ptr+1
	ldx #<FileBuffers
	ldy #>FileBuffers
	lda #FileBuffersL
	jsr ConfigureNumericOption
	sta NewBuffers

	ldx #1
	lda FmsDriveMask
count:	lsr a
	beq alldrives
	inx
	bne count
alldrives:
	stx Z2Ptr
	lda #1
	sta Z3Ptr
	lda #8
	sta Z3Ptr+1
	ldx #<DiskDrives
	ldy #>DiskDrives
	lda #DiskDrivesL
	jsr ConfigureNumericOption
	tax
	lda #0
setloop:
	sec
	rol a
	dex
	bne setloop
	sta NewDriveMask

	ldx #<Verify
	ldy #>Verify
	lda #VerifyL
	jsr Print
	lda #'N'
	ldx WriteCommand
	cpx #'P'
	beq isoff
	lda #'Y'
isoff:
	sta Z2Ptr
	ldx #<Z2Ptr
	ldy #>Z2Ptr
	lda #1
	jsr Print
	lda #30
	sta Z2Ptr
	ldx #<Z2Ptr
	ldy #>Z2Ptr
	lda #1
	jsr Print
	jsr CursorOn
	jsr YesNo
	lda #'P'
	ldx #'N'
	bcc turnoff
	lda #'W'
	ldx #'Y'
turnoff:
	sta NewWriteCommand
	stx Z2Ptr
	jsr CursorOff
	ldx #<Z2Ptr
	ldy #>Z2Ptr
	lda #1
	jsr Print
	jsr PrintEOL
	jsr PrintEOL
	;; Write the configuration back to disk.
	jsr WriteConfigBack
exit:
	rts
ConfigTitle:	.byte "DOS 2.++ Configuration Options",155,155
ConfigTitleL	=	*-ConfigTitle
FileBuffers:	.byte "File buffers     "
FileBuffersL	=	*-FileBuffers
DiskDrives:	.byte "Disk drives      "
DiskDrivesL	=	*-DiskDrives
Verify:		.byte "Write with verify (Y/N):"
VerifyL		=	*-Verify
.endproc
;;; *** Read a numeric option from the user, first print the
;;; *** Title in (X,Y,A), then get a (one-digit) number between
;;; *** Z3Ptr and Z3Ptr+1 (inclusive), current setting in
;;; *** Z2Ptr (which is printed). Return result in A.
.proc	ConfigureNumericOption
	jsr Print
	ldx Z3Ptr
	ldy #0
	jsr ToDecimal
	jsr LoadBuf3Ptr
	jsr SetZPtr
	ldy #0
	lda (ZPtr),y
	sta RangeBuf+2
	ldx Z3Ptr+1
	ldy #0
	jsr ToDecimal
	jsr LoadBuf3Ptr
	jsr SetZPtr
	ldy #0
	lda (ZPtr),y
	sta RangeBuf+4
	ldx Z2Ptr
	ldy #0
	jsr ToDecimal
	jsr LoadBuf3Ptr
	ldy #0
	lda (ZPtr),y
	sta RangeBuf+7
	ldx #<RangeBuf
	ldy #>RangeBuf
	lda #RangeBufL
	jsr Print
	jsr CursorOn
	jsr GetKey
	pha
	jsr CursorOff
	pla
	cmp #$9b		;EOL -> keep old
	bne noteol
	lda Z2Ptr
	bcs exit
noteol:	
	cmp #'0'
	bcc error
	cmp #'9'+1
	bcs error
	and #$f
	cmp Z3Ptr
	bcc error
	cmp Z3Ptr+1
	bcc exit
	beq exit
error:
	jmp ParameterError
exit:
	pha
	tax
	ldy #0
	jsr ToDecimal
	jsr LoadBuf3Ptr
	lda #3
	jsr Print
	jsr PrintEOL
	pla
	rts
RangeBuf:	.byte " (x-x):x",30
RangeBufL	=	*-RangeBuf
.endproc
;;; *** Write the configured options back to the disk.
;;; *** WriteConfigBack
.proc	WriteConfigBack
	ldx #<EnterDisk
	ldy #>EnterDisk
	lda #EnterDiskL
	jsr Print
	ldx #<DefaultDevice
	ldy #>DefaultDevice
	lda DefaultDeviceLen
	jsr Print
	jsr PrintEOL
	ldx #<PressEnter
	ldy #>PressEnter
	lda #PressEnterL
	jsr Print
	jsr SetRedColor
	jsr CursorOff
	jsr GetKey
	jsr DisableBREAK
	
	jsr LoadBuf1Ptr
	stx fr0
	sty fr0+1
	ldx #<ConfigName
	ldy #>ConfigName
	lda #ConfigNameL
	jsr Copy
	jsr LoadBuf1Ptr
	jsr AddDevice
	ldx #$10
	jsr SetIOCB
	jsr LoadBuf1Ptr
	lda #4
	jsr Open
	tya
	pha
	jsr Close
	pla
	tay
	ldx #8
	cpy #FileNotFound
	beq createnew
	
	jsr CheckIOError
	
	ldx #<Exists
	ldy #>Exists
	lda #ExistsL
	jsr Print
	jsr CursorOn
	jsr EnableBREAK
	jsr GetKey
	pha
	jsr DisableBREAK
	pla
	ldx #8		;overwrite
	cmp #'O'
	beq createnewp
	inx
	cmp #'A'
	beq createnewp
	cmp #$9b
	bne error
	lda #'O'
	bne createnewp
error:	
	jmp ParameterError
createnewp:
	stx Z2Ptr+1
	sta Z2Ptr
	ldx #<Z2Ptr
	ldy #>Z2Ptr
	lda #1
	jsr Print
	jsr PrintEOL
	ldx Z2Ptr+1
createnew:
	txa
	and #1
	sta Z2Ptr		;store as flag whether a header needs to be written (0=yes)
	lda #0
	sta Z2Ptr+1		;store as a flag whether the file needs to be opened.
	ldx #$10
	jsr SetIOCB

	lda FmsBuffers
	cmp NewBuffers
	beq nowritebuf
	jsr WriteHeader
	ldx #<FmsBuffers
	ldy #>FmsBuffers
	jsr WriteOneByteTarget
	lda NewBuffers
	jsr Put
nowritebuf:	
	lda FmsDriveMask
	cmp NewDriveMask
	beq nowritemask
	jsr WriteHeader
	ldx #<FmsDriveMask
	ldy #>FmsDriveMask
	jsr WriteOneByteTarget
	lda NewDriveMask
	jsr Put
nowritemask:
	lda WriteCommand
	cmp NewWriteCommand
	beq nowritecmd
	jsr WriteHeader
	ldx #<WriteCommand
	ldy #>WriteCommand
	jsr WriteOneByteTarget
	lda NewWriteCommand
	jsr Put
nowritecmd:
	jsr Close
exit:
	rts
EnterDisk:	.byte "Enter boot disk in drive "
EnterDiskL	=	*-EnterDisk
PressEnter:	.byte "then press "
		.byte 'R'+$80,'E'+$80,'T'+$80,'U'+$80,'R'+$80,'N'+$80,155
PressEnterL	=	*-PressEnter
Exists:		.byte 155,"CONFIG.SYS already exists.",155
		.byte 'A'+$80,"ppend to or ",'O'+$80,"verwrite with new",155
		.byte "configuration (A/O):A",30
ExistsL		=	*-Exists
ConfigName:	.byte "CONFIG.SYS",155
ConfigNameL	=	*-ConfigName
.endproc
;;; *** WriteHeader
;;; *** Check whether the file still needs to be opened (Z2Ptr+1 == 0)
;;; *** and whether the binary header needs to be written (Z2Ptr == 0)
;;; *** Open Mode is 8 if header is required, otherwise 9 (Append)
.proc	WriteHeader
	lda Z2Ptr+1
	bne isopen
	jsr LoadBuf1Ptr
	lda Z2Ptr
	ora #8
	jsr Open
	jsr CheckIOError
	inc Z2Ptr+1		;is open
	
	lda Z2Ptr
	bne nohdr
	lda #$ff
	jsr Put
	lda #$ff
	jsr Put
	inc Z2Ptr		;header is written
nohdr:
isopen:
	rts
.endproc
;;; *** WriteOneByteTarget
;;; *** Write a binary load header sufficient for the one-byte
;;; *** target in X,Y
.proc	WriteOneByteTarget
	stx Z3Ptr
	sty Z3Ptr+1
	ldx #<2
	ldy #>2
	jsr SetLength
	ldx #<Z3Ptr
	ldy #>Z3Ptr
	jsr BPut
	jsr CheckIOError
	ldx #<Z3Ptr
	ldy #>Z3Ptr
	jsr BPut
	jsr CheckIOError
	rts
.endproc
;;; *** Write the one byte in A
;;; *** Put
.proc	Put
	sta Z3Ptr
	ldx #<1
	ldy #>1
	jsr SetLength
	ldx #<Z3Ptr
	ldy #>Z3Ptr
	jsr BPut
	jsr CheckIOError
	rts
.endproc
NewBuffers:		.byte 0
NewDriveMask:		.byte 0
NewWriteCommand:	.byte 0
;;; *** ShowConfig
;;; *** Show the current FMS configuration
.proc	ShowConfig
	ldx #<ConfigHeader
	ldy #>ConfigHeader
	lda #ConfigHeaderL
	jsr Print
	ldx #<FmsBufferTxt
	ldy #>FmsBufferTxt
	lda #FmsBufferTxtL
	jsr Print
	ldx FmsBuffers
	ldy #0
	jsr ToDecimal
	jsr LoadBuf3Ptr
	lda #6
	jsr Print
	jsr PrintEOL
	ldx #<DriveTxt
	ldy #>DriveTxt
	lda #DriveTxtL
	jsr Print
	lda FmsDriveMask
	ldx #1
cv:	lsr a
	beq fnd
	inx
	bne cv
fnd:
	ldy #0
	jsr ToDecimal
	jsr LoadBuf3Ptr
	lda #6
	jsr Print
	jsr PrintEOL
	ldx #<VerifyTxt
	ldy #>VerifyTxt
	lda #VerifyTxtL
	jsr Print
	ldx #<OffTxt
	ldy #>OffTxt
	lda WriteCommand
	cmp #'P'
	beq isoff
	ldx #<OnTxt
	ldy #>OnTxt
isoff:
	lda #3
	jsr Print
	jsr PrintEOL

	ldx #<FreeMem
	ldy #>FreeMem
	lda #FreeMemL
	jsr Print
	sec
	lda MemTop
	sbc AppMemHi
	tax
	lda MemTop+1
	sbc AppMemHi+1
	tay
	jsr ToDecimal
	jsr LoadBuf3Ptr
	lda #6
	jsr Print
	jsr PrintEOL

	ldx #<AppMem
	ldy #>AppMem
	lda #AppMemL
	jsr Print
	sec
	lda MemTop
	sbc MemLo
	tax
	lda MemTop+1
	sbc MemLo+1
	tay
	jsr ToDecimal
	jsr LoadBuf3Ptr
	lda #6
	jsr Print
	jsr PrintEOL

	ldx #<BufferPos
	ldy #>BufferPos
	lda #BufferPosL
	jsr Print

	ldx DiskBufferBase
	ldy DiskBufferBase+1
	jsr ToHex
	jsr LoadBuf3Ptr
	lda #4
	jsr Print
	jsr PrintEOL
	
	rts
ConfigHeader:	.byte "System settings",155,155
ConfigHeaderL	=	*-ConfigHeader
FmsBufferTxt:	.byte "File buffers       :"
FmsBufferTxtL	=	*-FmsBufferTxt
DriveTxt:	.byte "Number of drives   :"
DriveTxtL	=	*-DriveTxt
VerifyTxt:	.byte "Write with verify  :"
VerifyTxtL	=	*-VerifyTxt
OffTxt:		.byte "off"
OnTxt:		.byte "on "
FreeMem:	.byte "Free memory        :"
FreeMemL	=	*-FreeMem
AppMem:		.byte "Application memory :"
AppMemL		=	*-AppMem
BufferPos:	.byte "Disk buffers at    :$"
BufferPosL	=	*-BufferPos
.endproc
