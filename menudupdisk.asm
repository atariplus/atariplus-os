;;; **********************************************************************
;;; ** Thor Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: menudupdisk.asm,v 1.8 2013/11/24 20:07:38 thor Exp $       **
;;; **                                                                  **
;;; ** In this module:   Duplicate disk					**
;;; **********************************************************************

	.include "menujmptab.i"
	.include "kernel.i"
	.include "sio.i"
	.include "diskinterf.i"
	.include "fms.i"
	.include "screen.i"
	.include "reset.i"
	.include "errors.i"
	.segment "menufunction"
	
.macro  Skip2
        .byte $2c
.endmacro
.macro  Skip1
        .byte $24
.endmacro

ScreenAddress	=	$d4	;storage for the screen position
CurrentSector	=	$e0	;sector being worked on
SectorsRead	=	$e2	;counts the sectors read
SectorsWritten	=	$e4	;sectors read so far
EmptySecsCount	=	$e6	;counts empty sectors...
SectorCount	=	$ea	;number of sectors (+1) to copy
FirstInSector	=	$ec	;first sector to start reading from
SourceSectors	=	$ee	;number of sectors on the source (unmodified)
BufferStart	=	$f0	;start of the disk buffer
CurBufPtr	=	$f2	;pointer to current sector buffer
ClearSecsTable	=	$f4	;table containing the bit mask indicating empty sectors
BlankBuffer	=	$f6	;pointer to the disk buffer
Flag		=	$f8	;bit 7 = 1: do not request disks. bit 6=1: use VTOC, bit 0 = 1: write empty sectors
SourceUnit	=	$fa	;where to copy from
TargetUnit	=	$fb	;where to copy to

	
;;; Init Vector
	rts
;;; Run vector
	jmp Duplicate
;;; *** AccessCurrentSectorCurrentBuffer
;;; *** Access the current sector at the current buffer pointer.
;;; *** only parameter is C
.proc	AccessCurrentSectorCurrentBuffer
	ldx CurBufPtr
	ldy CurBufPtr+1
.endproc
	;; runs into the following
;;; *** AccessCurrentSector
;;; *** Access the current sector for the buffer at X,Y
.proc	AccessCurrentSector
	stx SIOBufferLo
	sty SIOBufferHi
	ldx CurrentSector
	ldy CurrentSector+1
.endproc
	;; runs into the following
;;; *** AccessSector
;;; *** Access the sector X,Y on the current device.
;;; *** C=0: Read, C=1 write
.proc	AccessSector
	sty SIOAux2
	stx SIOAux1
	lda #'R'
	bcc isread
	lda WriteCommand
isread:
	sta SIOCommand
	jsr DiskInterfVector
	jsr CheckIOError
	rts
.endproc
;;; *** RequestDisk
;;; *** If C=1, request the destination disk, otherwise request the source disk.
;;; *** If Flag & 0x80 is set, do not ask.
.proc	RequestDisk
	php
	jsr EnableBREAK
	jsr ClearLowerScreen
	ldx #0
	ldy #22
	jsr PositionCursor
	plp
	bcs dest
	jsr SetGreenColor
	ldx #<InsertSource
	ldy #>InsertSource
	bne print
dest:
	jsr SetRedColor
	ldx #<InsertDest
	ldy #>InsertDest
print:
	lda #InsertDestL
	bit Flag
	bmi donotask
	jsr Print
	jsr PrintAndWaitForReturn
donotask:	
	rts
InsertSource:	.byte "Insert "
		.byte 's'+128,'o'+128,'u'+128,'r'+128,'c'+128,'e'+128
		.byte "     "
InsertSourceL	=	*-InsertSource
InsertDest:	.byte "Insert "
		.byte 'd'+128,'e'+128,'s'+128,'t'+128,'i'+128,'n'+128,'a'+128,'t'+128,'i'+128,'o'+128,'n'+128
InsertDestL	=	*-InsertDest
.endproc
;;; *** PrintAndWaitForReturn
;;; *** Print the second half of the request if it is enabled,
;;; *** wait for return. Returns with C=1 if the user wants to abort.
.proc	PrintAndWaitForReturn
	ldx #<PressReturn
	ldy #>PressReturn
	lda #45
	jsr PrintRecord
	jsr GetKey
	pha
	jsr DisableBREAK
	jsr ClearLowerScreen
	pla
	cmp #155
	beq ok
	sec
	rts
ok:
	clc
	rts
PressReturn:	.byte " and press "
		.byte 'R'+128,'E'+128,'T'+128,'U'+128,'R'+128,'N'+128
		.byte 155
.endproc
;;; *** RequestBothDisks
;;; *** Asks the user to insert both disks. Works otherwise similar to the above.
.proc	RequestBothDisks
	jsr EnableBREAK
	ldx #<InsertBoth
	ldy #>InsertBoth
	lda #InsertBothL
	jsr Print
	jsr PrintAndWaitForReturn
	rts
InsertBoth:	.byte 155,"Insert disk(s)"
InsertBothL	=	*-InsertBoth
.endproc
;;; *** PositionCursor
;;; *** Set the position of the text cursor to X,Y
.proc	PositionCursor
	stx CursorColumn
	sty CursorRow
	lda #0
	sta CursorColumn+1
	rts
.endproc
;;; *** ClearLowerScreen
;;; *** Remove all text from line 18 on
.proc	ClearLowerScreen
	ldx #0
	ldy #18
	jsr PositionCursor
	ldx #<ClearLines
	ldy #>ClearLines
	lda #ClearLinesL
	jsr Print
	rts
ClearLines:	.byte $9c,$9c,$9c,$9c,$9c,$9c,$9c,$9c
ClearLinesL	=	*-ClearLines
.endproc
;;; *** PrintMenu
;;; *** Print the menu to the screen
.proc	PrintMenu
	jsr BlockDisplay
	jsr CursorOff
	ldx #<MenuText
	ldy #>MenuText
	lda #MenuTextL
	jsr Print
	ldx #<(40*8)
	ldy #>(40*8)
	jsr OffsetScreenAddress
	ldy #40-1
	lda #$80
line:
	sta (ScreenAddress),y
	dey
	bpl line
	jsr SetGreenColor
	rts
MenuText:	.byte $7d,$9b,$9b
	.byte "accessing sector :000",155,155
	.byte "sectors read :000   sectors written:000",155,155
	.byte "empty sectors:000"
MenuTextL	=	*-MenuText
.endproc
;;; *** InsertHex
;;; *** Insert the hex number X,Y at ScreenAddress
.proc	InsertHex
	jsr ToHex
	jsr LoadBuf3Ptr
	jsr SetZPtr
	ldy #3
cplp:	lda (ZPtr),y
	sec
	sbc #$20		;to ANTIC code
	sta (ScreenAddress),y
	dey
	bne cplp
	rts
.endproc
;;; *** OffsetScreenAddress
;;; *** Set the Screen address to the base screen plus X,Y
.proc	OffsetScreenAddress
	clc
	txa
	adc GfxOrigin
	sta ScreenAddress
	tya
	adc GfxOrigin+1
	sta ScreenAddress+1
	rts
.endproc
;;; *** AllocateTables
;;; *** Get all the memory for the flags table and the
;;; *** empty sector
.proc	AllocateTables
	ldx #<$84
	ldy #>$84
	jsr Reserve
	stx ClearSecsTable
	sty ClearSecsTable+1
	lda #0
	tay
clrsec2:
	sta (ClearSecsTable),y
	iny
	cpy #$84
	bcc clrsec2
	
	ldx #<128
	ldy #>128
	jsr Reserve
	stx BlankBuffer
	sty BlankBuffer+1
	lda AppMemHi
	sta BufferStart
	lda AppMemHi+1
	sta BufferStart+1
	rts
.endproc
;;; *** ClearBlankBuffer
;;; *** Erase the dummy sector
.proc	ClearBlankBuffer
	ldy #0
	tay
clrlp:
	sta (BlankBuffer),y
	iny
	bpl clrlp
	rts
.endproc
;;; *** SetSectorFlag
;;; *** Set the flag for sector (X,Y) to A (0 or 1)
.proc	SetSectorFlag
	pha
	jsr ToByteAndMask
	pla
	beq reset
	txa
	ora (ClearSecsTable),y
	sta (ClearSecsTable),y
	rts
reset:
	txa
	eor #$ff
	and (ClearSecsTable),y
	sta (ClearSecsTable),y
	rts
.endproc
;;; *** GetSectorFlag
;;; *** Checks the flag for sector X,Y,
;;; *** returns nonzero if set (not necessarily one)
.proc	GetSectorFlag
	jsr ToByteAndMask
	txa
	and (ClearSecsTable),y
	rts
.endproc
;;; *** ToByteAndMask
;;; *** Compute from the sector (X,Y) a byte offset in Y
;;; *** and a bitmask X
.proc	ToByteAndMask
	txa
	and #7
	pha			;will be used to get the bitmask
	txa
	sty ZPtr
	lsr ZPtr
	ror a
	lsr ZPtr
	ror a
	lsr ZPtr
	ror a			;divide by eight
	tay
	pla
	tax
	lda DuBitMask,x
	tax
	rts
DuBitMask:	.byte $80,$40,$20,$10,$08,$04,$02,$01
.endproc
;;; *** MarkCurrentSectorAsEmpty
;;; *** Mark the current sector as empty in the sector table
.proc	MarkCurrentSectorAsEmpty
	ldx #<(6*40+14-1)
	ldy #>(6*40+14-1)		;screen position
	jsr OffsetScreenAddress
	lda #1
	ldx CurrentSector
	ldy CurrentSector+1
	jsr SetSectorFlag
	inc EmptySecsCount
	bne noinc
	inc EmptySecsCount+1
noinc:
	ldx EmptySecsCount
	ldy EmptySecsCount+1
	jsr InsertHex
	rts
.endproc
;;; *** IncrementReadCount
;;; *** Mark one additional sector as read
.proc	IncrementReadCount
	ldx #<(4*40+14-1)
	ldy #>(4*40+14-1)
	jsr OffsetScreenAddress
	inc SectorsRead
	bne noinc
	inc SectorsRead+1
noinc:
	ldx SectorsRead
	ldy SectorsRead+1
	jsr InsertHex
	rts
.endproc
;;; *** IncrementWriteCount
;;; *** Mark one additional sector as written
.proc	IncrementWriteCount
	ldx #<(4*40+36-1)
	ldy #>(4*40+36-1)
	jsr OffsetScreenAddress
	inc SectorsWritten
	bne noinc
	inc SectorsWritten+1
noinc:
	ldx SectorsWritten
	ldy SectorsWritten+1
	jsr InsertHex
	rts
.endproc
;;; *** MarkCurrentSectorAsDirty
;;; *** Mark the current sector as non-empty
.proc	MarkCurrentSectorAsDirty
	lda #0
	ldx CurrentSector
	ldy CurrentSector+1
	jsr SetSectorFlag
	rts
.endproc
;;; *** DisplayCurrentSector
;;; *** print the currently accessed sector on the screen
.proc	DisplayCurrentSector
	ldx #<(2*40+18-1)
	ldy #>(2*40+18-1)
	jsr OffsetScreenAddress
	ldx CurrentSector
	ldy CurrentSector+1
	jsr InsertHex
	rts
.endproc
;;; *** GetDiskDensity
;;; *** Check for the disk density of the current disk. Returns
;;; *** with N=0 (positive) for single density, N=1 for ED
.proc	CheckDiskDensity
	lda #'S'
	sta SIOCommand
	jsr DiskInterfVector
	jsr CheckIOError
	lda DiskStatus+1
	bpl nodisk
	bit DiskStatus
	rts
nodisk:
	ldy #DeviceNak
	jmp Error
.endproc
;;; *** FindSectorCount
;;; *** Find the sector index of the first sector no longer
;;; *** to touch. This is the number of sectors plus one.
.proc	FindSectorCount
	lda SourceUnit
	sta SIODeviceUnit
	jsr CheckDiskDensity
	bmi enhanced
	ldx #<$2d1
	ldy #>$2d1		;SD
	bne singledensity
enhanced:	
	ldx #<$411
	ldy #>$411
singledensity:
	stx SectorCount
	sty SectorCount+1
	rts
.endproc
;;; *** CheckTargetDensity
;;; *** Check the density of the target device. Returns C=1 if the
;;; *** user aborts because the density does not fit.
.proc	CheckTargetDensity
	lda TargetUnit
	sta SIODeviceUnit
	jsr CheckDiskDensity
	bmi enhanced
	lda SectorCount+1
	cmp #2
	bne targetissd
	beq exit
enhanced:
	lda SectorCount+1
	cmp #4
	bne targetised
exit:
	clc
	rts
targetissd:
	ldx #<TargetIsSD
	ldy #>TargetIsSD
	bne askcont
targetised:
	ldx #<TargetIsED
	ldy #>TargetIsED
askcont:
	lda #45
	jsr PrintRecord
	ldx #<DoContinue
	ldy #>DoContinue
	lda #DoContinueL
	jsr Print
	jsr CursorOn
	jsr EnableBREAK
	jsr YesNo
	php
	jsr PrintResult
	jsr DisableBREAK
	jsr CursorOff
	plp
	bcs continue
	sec
	rts
continue:
	clc
	rts
TargetIsSD:	.byte "Destination format is single density",155
TargetIsED:	.byte "Destination format is enhanced density",155
DoContinue:	.byte "which does not match the source density.",155
		.byte "Press ",34,"Y",34," to continue nevertheless :"
DoContinueL	=	*-DoContinue
.endproc
;;; *** ReadCurrentSector
;;; *** Read the current sector, at least if required.
;;; *** Checks whether it is empty.
.proc	ReadCurrentSector
	jsr DisplayCurrentSector
	bit Flag
	bvc novtoc
	ldx CurrentSector
	ldy CurrentSector+1	;check the VTOC whether the sector is in use.
	jsr GetSectorFlag
	bne isempty
novtoc:
	clc			;reading
	jsr AccessCurrentSectorCurrentBuffer
	;; now check whether it is empty
	ldy #128-1
tst:	lda (CurBufPtr),y
	bne isdirty
	dey
	bpl tst
	jsr MarkCurrentSectorAsEmpty
	clc
	bcc cont
isdirty:
	jsr MarkCurrentSectorAsDirty
	jsr IncrementBufPtr
cont:
	jsr IncrementReadCount
isempty:
	jsr IncrementCurrentSector
	rts
.endproc
;;; *** WriteCurrentSector
;;; *** Write the current sector back to disk
.proc	WriteCurrentSector
	jsr DisplayCurrentSector
	ldx CurrentSector
	ldy CurrentSector+1
	jsr GetSectorFlag	;empty?
	bne empty
	sec			;writing.
	jsr AccessCurrentSectorCurrentBuffer
	jsr IncrementBufPtr
	clc
	bcc continue
empty:
	;; even if the sector is empty and empty
	;; sectors should not be written, write it
	;; if it is the boot sector, or the directory
	;; sectors
	ldx CurrentSector
	ldy CurrentSector+1
	beq testboot
	cpy #1
	beq checkdirsector
	cpy #4
	bne normalblank
	cpx #0			;is the Dos 2.5 VTOC? Always write
	beq writeempty
	bne normalblank
checkdirsector:	
	cpx #$68		;start VTOC
	bcc normalblank
	cpx #$71
	bcc writeempty
	bcs normalblank
testboot:
	cpx #4
	bcc writeempty
normalblank:	
	lda Flag
	lsr a
	bcc continue
writeempty:
	;; empty buffer
	ldx BlankBuffer
	ldy BlankBuffer+1
	sec
	jsr AccessCurrentSector
continue:
	jsr IncrementWriteCount
	jsr IncrementCurrentSector
	rts
.endproc
;;; *** IncrementBufPtr
;;; *** Increment the buffer pointer by one sector
.proc	IncrementBufPtr
	clc
	lda CurBufPtr
	adc #128
	sta CurBufPtr
	bcc exit
	inc CurBufPtr+1
exit:
	rts
.endproc
;;; *** IncrementCurrentSector
;;; *** Increment the current sector index
.proc	IncrementCurrentSector
	inc CurrentSector
	bne exit
	inc CurrentSector+1
exit:
	rts
.endproc
;;; *** CheckAvail
;;; *** Check whether there is enough memory
;;; *** to buffer at least one sector
;;; *** returns with C=0 if this is not the case, i.e. the buffer is full
.proc	CheckAvail
	sec
	lda MemTop
	sbc CurBufPtr
	tax			;keep lo
	lda MemTop+1
	sbc CurBufPtr+1
	bcc full
	bne empty
	;; here: high is zero
	;; check the number of available bytes in LO
	cpx #128
full:	
empty:
	rts
.endproc
;;; *** CheckForLastSector
;;; *** Check whether we are at the last sector
;;; *** returns C=1 if so.
.proc	CheckForLastSector
	lda CurrentSector
	cmp SectorCount
	lda CurrentSector+1
	sbc SectorCount+1
	rts
.endproc
;;; *** ReadSourceSectors
;;; *** Read as many sectors as possible from the source
;;; *** If C=1, do not request the source.
;;; *** returns with C=0 if the buffer is full, otherwise C=1
;;; *** also returns with C=1 on error (check for current sector to distinguish)
.proc	ReadSourceSectors
	lda SourceUnit
	sta SIODeviceUnit
	bcs norequest
	;; c=0
	jsr RequestDisk
	bcs abort
norequest:
	jsr ResetToStartOfBuffer
rdloop:	
	jsr ReadCurrentSector
	jsr CheckAvail
	bcc full
	jsr CheckForLastSector
	bcc rdloop
full:
abort:	
	rts
.endproc
;;; *** WriteTargetSectors
;;; *** Write all buffered sectors, returns with C=0 if there is still something to
;;; *** be done, otherwise returns C=1 if completed or aborted.
;;; *** If c=1, the density of the target must be checked.
.proc	WriteTargetSectors
	php
	lda TargetUnit
	sta SIODeviceUnit
	sec
	jsr RequestDisk
	bcs abort
	plp
	bcc ischecked		;still need to check the target density?
	bit Flag
	bmi ischecked		;nothing to do, is already done
	jsr CheckTargetDensity
	bcs exit		;if the user chooses to abort, do now.
ischecked:
	jsr ResetToStartOfBuffer
wrloop:
	jsr WriteCurrentSector
	jsr CheckAvail
	bcc full
	jsr CheckForLastSector
	bcc wrloop
full:
	lda CurrentSector
	sta FirstInSector
	lda CurrentSector+1
	sta FirstInSector+1
exit:	
	rts
abort:
	pla			; avoid changing the status flags
	rts
.endproc
;;; *** ParseDiskUnit
;;; *** Check whether the string at X,Y len A contains anything like a
;;; *** disk device. Either a unit number, or a device number, or something
;;; *** that looks remotely like it. Return the number in A or error.
.proc	ParseDiskUnit
	sta Z2Ptr
	jsr SetZPtr
	lda Z2Ptr
	beq nounit		;no chance
	ldy #0
	lda (ZPtr),y
	cmp #'D'		;only the disk drive is allowed here
	bne testnumber
	lda Z2Ptr
	cmp #2			;must be 2 or larger
	bcc nounit
	iny
	lda #':'
	cmp (ZPtr),y		;next is either : or a number
	beq unitone		;is the first device
	ldx Z2Ptr
	cpx #3			;must be at least 3
	bcc nounit
	iny
	cmp (ZPtr),y
	bne nounit		;cannot be a unit if there is no colon
	dey
	lda (ZPtr),y
	Skip2
unitone:
	lda #'1'
	;; here: must be a number
testnumber:
	cmp #'1'
	bcc nounit
	cmp #'9'+1
	bcs nounit
	and #$0f
	rts
nounit:
	jsr PrintEOL
	ldx #<NoUnit
	ldy #>NoUnit
	lda #45
	jsr PrintRecord
	jmp ParameterError
NoUnit:		.byte "Not a valid drive specification",155
.endproc
;;; *** CheckSectorInRange
;;; *** Check whether the sector X,Y is within [FirstInSector,SectorCount)
.proc	CheckSectorInRange
	txa
	cmp FirstInSector
	tya
	sbc FirstInSector+1
	bcc badsector
	txa
	cmp SectorCount
	tya
	sbc SectorCount+1
	bcs badsector
	rts
badsector:
	jsr PrintEOL
	ldx #<BadSector
	ldy #>BadSector
	lda #45
	jsr PrintRecord
	jmp ParameterError
BadSector:	.byte "Sector range is invalid.",155
.endproc
;;; *** ResetToStartOfBuffer
;;; *** Rewind the current sector to the FirstInSector,
;;; *** and the buffer to the start
.proc	ResetToStartOfBuffer
	lda FirstInSector
	sta CurrentSector
	lda FirstInSector+1
	sta CurrentSector+1
	lda BufferStart
	sta CurBufPtr
	lda BufferStart+1
	sta CurBufPtr+1
	rts
.endproc
;;; *** ReadVTOC
;;; *** Read the VTOC into the blankbuffer (then not blank)
.proc	ReadVTOC
	lda SourceUnit
	sta SIODeviceUnit
	ldx BlankBuffer
	ldy BlankBuffer+1
	stx SIOBufferLo
	sty SIOBufferHi
	ldx #<$168
	ldy #>$168		;VTOC sector
	clc
	jsr AccessSector	;read in
	rts
.endproc
;;; *** VTOCToSectorMask
;;; *** Read the disk VTOC and set the sector flags for empty sectors.
.proc	VTOCToSectorMask
	;; bytes 10 and up mark the regular 2.0S compatible
	;; VTOC and can be copied over directly.
	ldy #0
	sty ZPtr
	ldy #10
	sty ZPtr+1
copyregular:
	lda (BlankBuffer),y
	inc ZPtr+1
	ldy ZPtr
	sta (ClearSecsTable),y
	inc ZPtr
	ldy ZPtr+1
	bpl copyregular		;up to the last byte
	;; bytes 6 and up make up the rest of the data.
	ldy #6
	sty ZPtr+1
copyextended:
	lda (BlankBuffer),y
	inc ZPtr+1
	ldy ZPtr
	sta (ClearSecsTable),y
	inc ZPtr
	ldy ZPtr+1
	cpy #10
	bcc copyextended
	;; mark all extended sectors as empty
	lda #$ff
	ldy ZPtr
empty:	
	sta (ClearSecsTable),y
	iny
	cpy #132
	bcc empty

	;; mark sector 720 as dirty, some games store data here
	ldy #90
	lda (ClearSecsTable),y
	and #$7f
	sta (ClearSecsTable),y

	;; mask sector 1024 as dirty as it may contain the VTOC of Dos 2.5
	ldy #128
	lda #$7f
	sta (ClearSecsTable),y

	;; mark sectors $3D8 to $3fd as busy as they may contain the Dos 2.XL boot-
	;; strap code
	ldy #123
	lda #0
dirty:	sta (ClearSecsTable),y
	iny
	bpl dirty
	rts
.endproc
;;; *** PrintResult
;;; *** If C=0, print yes, otherwise print no. Keep the carry.
.proc	PrintResult
	php
	bcc no
	ldx #<YesString
	ldy #>YesString
	bne printnow
no:
	ldx #<NoString
	ldy #>NoString
printnow:
	lda #45
	jsr PrintRecord
	plp
	rts
YesString:	.byte "yes",155
NoString:	.byte "no",155
.endproc
;;; *** FindLength
;;; *** Compute the length of the buffer at X,Y up to the EOL
.proc	FindLength
	jsr SetZPtr
	ldy #0
	lda #$9b
fndlp:	cmp (ZPtr),y
	beq end
	iny
	bne fndlp
end:
	tya
	rts
.endproc
;;; *** Duplicate
;;; *** Duplicate an entire disk
.proc	Duplicate
	;; reset the sector counters
	ldx #$0b
	lda #0
	sta Flag
clr:	sta CurrentSector,x
	dex
	bpl clr
	ldx #<DuplicateRequest
	ldy #>DuplicateRequest
	lda #45
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	;; get the source
	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	bcc cont
aborthere:	
	rts			;abort if nothing here.
cont:	
	bne isfilled
	ldx #<DefaultDevice
	ldy #>DefaultDevice
	lda DefaultDeviceLen
	bne parse
isfilled:
	jsr LoadBuf1Ptr
	jsr FindLength
	pha
	jsr LoadBuf1Ptr
	pla
parse:
	jsr ParseDiskUnit
	sta SourceUnit

	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	bne isfilled2
	ldx #<DefaultDevice
	ldy #>DefaultDevice
	lda DefaultDeviceLen
	bne parse2
isfilled2:
	jsr LoadBuf1Ptr
	jsr FindLength
	pha
	jsr LoadBuf1Ptr
	pla
parse2:
	jsr ParseDiskUnit
	sta TargetUnit

	cmp SourceUnit
	beq keepasking

	lda Flag		;disable asking
	ora #$80
	sta Flag
	
	jsr RequestBothDisks
	bcs aborthere
	jsr FindSectorCount
	jsr CheckTargetDensity
	bcs aborthere
	bcc founddens
keepasking:
	clc
	jsr RequestDisk		;request the source disk only
	jsr FindSectorCount
	bcs aborthere
founddens:	
	lda SectorCount
	sta SourceSectors
	lda SectorCount+1
	sta SourceSectors+1

	;; Read the VTOC to determine whether the source is
	;; a valid Dos 2.++ disk
	jsr AllocateTables
	jsr ReadVTOC
	
	;; Start with the first sector
	ldy #>1
	ldx #<1
	stx FirstInSector
	sty FirstInSector+1
	;; Check whether we have a first sector
	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	beq notfirst
	jsr LoadBuf1Ptr
	jsr FromHex
	jsr CheckSectorInRange
	stx FirstInSector
	sty FirstInSector+1
notfirst:
	;; check whether we have a last sector
	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	beq notlast
	jsr LoadBuf1Ptr
	jsr FromHex
	jsr CheckSectorInRange
	;; must keep last sector + 1
	inx
	bne noinc
	iny
noinc:	stx SectorCount
	sty SectorCount+1
notlast:
	jsr CursorOn
	jsr EnableBREAK
	
	ldy #0
	lda (BlankBuffer),y
	cmp #2
	bne novtoc
	
	ldx #<UseVTOC
	ldy #>UseVTOC
	lda #UseVTOCL
	jsr Print
	jsr YesNo
	jsr PrintResult
	bcc novtoc
	lda Flag
	ora #$40		;use VTOC
	sta Flag
novtoc:
	ldx #<WriteEmpty
	ldy #>WriteEmpty
	lda #WriteEmptyL
	jsr Print
	jsr YesNo
	jsr PrintResult
	bcc notemptyfg
	lda Flag
	ora #1			;write empty sectors
	sta Flag
notemptyfg:
	jsr DisableBREAK
	jsr CursorOff
	jsr PrintMenu
	;; parse the VTOC now?
	bit Flag
	bvc ignorevtoc
	jsr VTOCToSectorMask
ignorevtoc:
	jsr ClearBlankBuffer
	sec			;do not ask the user to innsert the source, is already
copyloop:
	php
	jsr ReadSourceSectors
	bcc isok
	jsr CheckForLastSector	;error or are we done?
	bcs isok
	plp
	rts			;ended
isok:
	plp
	jsr WriteTargetSectors	;all done?
	bcc copyloop		;not all done, not an error? Continue!
	;; here: either an error, or all done. In either case, return.
abort:	
	rts
UseVTOC:	.byte 155,"Is the source a Dos 2.++ disk? If so,",155
		.byte "type ",34,"Y",34," to skip unused sectors: "
UseVTOCL	=	*-UseVTOC

WriteEmpty:	.byte 155,"Clean unused or empty sectors? If so,",155
		.byte "type ",34,"Y",34," to write them to disk : "
WriteEmptyL	=	*-WriteEmpty
	
DuplicateRequest:	.byte "Copy disk - give from,to(,first,last)",155
.endproc

