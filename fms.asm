;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: fms.asm,v 1.49 2008-12-29 23:37:23 thor Exp $		**
;;; **									**
;;; ** In this module:	 Implementation of the D: handler		**
;;; **********************************************************************

	.include "fms.i"
	.include "cio.i"
	.include "errors.i"
	.include "sio.i"
	.include "diskinterf.i"
	.include "kernel.i"
	.include "editor.i"	; for the bitmasks
	.include "reset.i"
	.include "pia.i"
	.include "nmi.i"
	.include "misc.i"
	
	.segment  "OsLo"

;;; ***
;;; *** Useful macro definitions
;;; ***
;;; *** Deliver an error to the internal error handler
.macro	Error errno
	jsr PushError
	.byte errno
.endmacro
;;; *** Skip two bytes (by a dummy BIT)
.macro	Skip2
	.byte $2c
.endmacro
.macro	Skip1
	.byte $24
.endmacro
;;; *** The following two bytes are reserved to carry the checksum
	.word	0
;;; *** The device init table for the disk drive/fms
	.global FmsTable
FmsTable:
	.word	FmsOpen-1
	.word	FmsClose-1
	.word	FmsGet-1
	.word	FmsPut-1
	.word	FmsStatus-1
	.word	FmsSpecial-1
;;; *** FmsInit
;;; *** Called by the Os to initialize this beast
	.global FmsInit
.proc	FmsInit
	
	lda WarmStartFlag	; are we coldstarting?
	bne iswarm		; if warmstarting, then no need to initialize buffers
	lda #3			; reserve for two drives
	sta FmsDriveMask
	sta FmsBuffers		; and reserve buffers for three open files

	lda #<LaunchDup		; where we get run to launch the DUP
	sta DupVector
	lda #>LaunchDup
	sta DupVector+1
	lda #'P'		; default is write without verify
	sta WriteCommand
iswarm:
	jsr SetupBuffers

	ldx #'D'
	lda #>FmsTable
	ldy #<FmsTable
	jsr MountHandlerVector	; install the D handler now into HATABS
	;; now the file based bootstrap
	lda WarmStartFlag	; are we warmstarting here?
	bne warmstart
	lda FmsBootFlag
	lsr a			; shall we try to bootstrap?
	bcs warmstart
	lda KeyCodeShadow
	cmp #28			; ESC pressed?
	beq warmstart		; if so, drop it
	lda #>Config
	ldy #<Config
	jsr LoadFile		; now boot off several files
	jsr SetupBuffers	; might have changed the setup
	lda #>Handlers
	ldy #<Handlers
	jsr LoadFile
	lda #>Autorun
	ldy #<Autorun
	jsr LoadFile
warmstart:		
	rts
;;; Binary load the indicated file
LoadFile:
	ldx #$10		; use traditionally IOCB#1 for this
	jsr InitCIOAddress	; Insert this as command
	lda #$c0		; run and init
	sta IOCBAux1,x		; init and run the file
	lda #CmdBload		; binary load
	jmp DispatchCIOCommand	; and launch the command
;;; File names
Config:		.byte "D:CONFIG.SYS",$9b	; shall contain drive/fms settings or patches
Handlers:	.byte "D:HANDLERS.SYS",$9b	; shall contain additional handlers
Autorun:	.byte "D:AUTORUN.SYS",$9b	; the traditional bootstrap
.endproc
;;; *** SetupBuffers
;;; *** Initialize buffers and buffer pointers
.proc	SetupBuffers
	lda #<FmsEnd
	sta FmsPtr
	ldy #>FmsEnd
	sty FmsPtr+1
	ldx FmsDriveMask	; get #of disks we want to support
	stx FmsTmp
	sta DiskBufferBase
	sty DiskBufferBase+1	; will be the base of all buffers
	ldx #7
initdbuflp:			; check for all flags
	lda #0
	ldy #5			; VTOC offset
	sta AvailFlags,x	; default:	not available
	asl FmsTmp		; for the availibity flags
	bcc nosuchdrive
	ror AvailFlags,x	; set the bit
	sta (FmsPtr),y		; ensure that the VTOC is marked as "free"
	jsr AdvanceBuffer
nosuchdrive:
	dex 
	bpl initdbuflp		; until all drives done
	lda FmsPtr
	sta FileBufferBase	; file buffers start here
	lda FmsPtr+1
	sta FileBufferBase+1
	ldy FmsBuffers		; reserve as many buffers
	inx			; reset X back to zero
initbuffers:
	dey
	tya			; write something non-zero in here
	bmi nobuffer
	jsr AdvanceBuffer	; reserve a buffer for it
	lda #0			; clear:	Buffer is free
nobuffer:	
	sta FileBufferFlags,x
	inx
	cpx #8			; up to eight buffers
	bcc initbuffers
	lda FmsPtr
	sta MemLo		; reset MemLo now appropriately
	lda FmsPtr+1
	sta MemLo+1
	
	ldy #0
	lda #$ff		; set buffers as free
initfcb:
	sta FCBBase,y		; clear out the control block
	iny
	bpl initfcb
	rts
;;; Advance the buffer pointer by one disk buffer
AdvanceBuffer:
	lda FmsPtr
	clc
	adc #128		; one disk buffer full
	sta FmsPtr
	bcc nocarry
	inc FmsPtr+1
nocarry:
	rts
.endproc	
;;; *** LaunchDup
;;; *** This calls the DUP from the selftest. 
.proc	LaunchDup
	jsr MapSelfTest
	jmp RunDup		; is within the selftest
.endproc
;;; *** CheckDevice
;;; *** Check whether the indicated device is available
.proc	CheckDevice
	;; Here: Run a pre-check whether the disk is available
	lda #'S'
	sta SIOCommand
	jsr RunDiskInterf	; check whether the disk is here
	lda DiskStatus+1	; check the second disk status for insertion
	bmi diskavailable
	Error DeviceNak		; signal a NAK
diskavailable:
	rts
.endproc
;;; *** MarkFileBusy
;;; *** Mark the file in the directory busy and
;;; *** ditto the VTOC
.proc	MarkFileBusy
	jsr MarkVTOCBusy	; mark the VTOC as busy
	;; runs into the following
.endproc
;;; *** MarkOnlyFileBusy
;;; *** same as above, but VTOC is assumed to be busy already
.proc	MarkOnlyFileBusy
	jsr LoadDirOffset
	lda #$43		; set bit 0 to indicate that the file is busy
	sta (FileBuffer),y
	bne WriteDir		; jumps always, writes the directory back
.endproc
;;; *** ReadDir
;;; *** Read a directory sector into the internal
;;; *** sector buffer
.proc	ReadDir
	clc
	Skip1
	;; runs into the following
.endproc
;;; *** WriteDir
;;; *** Write the internal sector buffer out
;;; *** to the disk
.proc	WriteDir
	sec
	php
	jsr LoadFileBuffer
	ldx ZIOCB 
	lda FileCode,x
	and #$e0		; get the sector bits
	asl a			; do not move the carry into bit 0
	rol a
	rol a
	rol a			; get sector offset. C is now cleared
	adc #<$169
	plp
	tay
	bne AccessAdministrationSector	; jumps always
.endproc
;;; *** ReadVTOC
;;; *** Read the VTOC if it is not yet read already
.proc	ReadVTOC
	ldy #$05		; offset of the read/write status byte
	clc			; direction is read
	lda (DiskBuffer),y	; check whether we are set
	beq AccessVTOC		; if equal, then the VTOC must be reloaded
	rts			; is here already
.endproc
;;; *** MarkVTOCBusy
;;; *** Mark the VTOC as busy
.proc	MarkVTOCBusy
	jsr ReadVTOC		; read it
	;; runs into the following
.endproc
;;; *** WriteVTOC
;;; *** Write the VTOC out to the disk
.proc	WriteVTOC
	lda #$00
	ldy #$05
	sta (DiskBuffer),y	; clear the VTOC valid flag:	Is valid
	sec
	;; runs into the following
.endproc
;;; *** AccessVTOC
;;; *** Read or write the DiskBuffer into the VTOC
.proc	AccessVTOC
	lda DiskBuffer
	sta SIOBufferLo		; initialize the buffer
	lda DiskBuffer+1
	sta SIOBufferHi		; ditto
	ldy #<$168		; enforce an addition carry over to the sector in front of the directory
	;; runs into the following
.endproc
;;; *** AccessAdministrationSector
;;; *** Read/Write a sector relative to the
;;; *** directory start at $169 with the operation on the stack
.proc	AccessAdministrationSector
	lda #>$168		; hi of directory sector
	;; runs into the following
	;jmp AccessDisk		; run a disk access
.endproc
;;; *** AccessDisk
;;; *** Access the disk for the buffer for
;;; *** sector A hi, Y lo. Buffer is already loaded,
;;; *** carry cleared for read, else write
.proc	AccessDisk
	sta SIOAux2
	sty SIOAux1
	lda #'R'		; read command
	bcc RunDiskInterf
	lda WriteCommand	; get the write command
	;; runs into the following
.endproc
;;; *** RunDiskInterf
;;; *** Run the DiskInterf vector with command in A and reload the X register
.proc	RunDiskInterf
	sta SIOCommand		; write the command
	jsr DiskInterfVector	; call the Os. Leave repeating/CmdSetup to DiskInterf
	ldx ZIOCB		; reload the IOCB
	tya			; set error flag
	bmi diskerror
	rts
diskerror:
	jmp YError		; deliver the error
.endproc
;;; *** ReadFileSector
;;; *** Read a sector into the file buffer
;;; *** X contains the offset of the FCB
.proc	ReadFileSector
	clc
	Skip1
	;; runs into the following
.endproc
;;; *** WriteFileSector
;;; *** Write a sector from the file buffer
;;; *** X contains the offset of the FCB
.proc	WriteFileSector
	sec			; gets skipped from above
	jsr LoadFileBuffer	; into the file buffer
	lda FileSector+1,x	; get high
	pha
	ora FileSector,x	; is it zero?
	beq linkerror
	pla			; get back
	ldy FileSector,x	; get low
	jmp AccessDisk		; with flags in C for read/write
linkerror:
	Error BadLinkage	; deliver error 0xaf
.endproc;
;;; *** LoadFileBuffer
;;; *** Load the SIO data pointer with the file buffer
.proc	LoadFileBuffer
	lda FileBuffer
	sta SIOBufferLo
	lda FileBuffer+1
	sta SIOBufferHi		; set the file buffer
	rts
.endproc
;;; *** LinkNextSector
;;; *** insert a link to the next sector into the
;;; *** old sector, and write the old sector out
.proc	LinkNextSector
	lda SpecialFlag,x	; are we boot or file?
	bmi nolinkage		; do not perform a link if we are direct access
	
	tay			; start FMS data in the file area (also in the SpecialFlag)
	lda NextSector+1,x	; get the next sector hi
	ora FileCode,x		; or the directory index in
	sta (FileBuffer),y
	iny
	lda NextSector,x
	sta (FileBuffer),y
	iny
	lda BytePosition,x	; the byte pointer here
	sta (FileBuffer),y	; insert the byte count here as well
nolinkage:
	jmp WriteFileSector	; write this out
.endproc	
;;; *** AdvanceSector
;;; *** advance the sector for reading/writing
;;; *** expects the ZIOCB in x
;;; *** Returns carry set for EOF
.proc	AdvanceSector
	lda AccessFlags,x	; reading/writing?
	bmi writeaccess		; create new links?
	asl a			; sector dirty?
	bpl AdvanceReading
	asl a			; here dirty, clear it
	sta AccessFlags,x
	jsr WriteFileSector	; write the file sector out
	bpl AdvanceReading	; is always true
writeaccess:
	lda SpecialFlag,x	; are we boot or file?
	bpl fileaccess
	jsr AdvanceDirect	; bump the sector in the simple way
	bcc contw
fileaccess:
	jsr AllocateSector	; find a free sector
contw:
	jsr LinkNextSector
	;; runs into the following
.endproc
;;; *** EnlargeFile:	Enlarge the file by one
;;; *** sector and bump the next sector into the current
.proc	EnlargeFile
	inc FileLength,x
	bne nocarry2
	inc FileLength+1,x	; increment the sector size
nocarry2:
	jsr MoveNextSector	; move the next sector to this sector
	lda #$00
	sta NextSector,x	; clear out the next sector, still to be found
	sta NextSector+1,x	; ditto (on EOF, LinkNextSector will write a NULL continuation, which is desired)
	sta BytePosition,x	; reaset the byte pointer
	lda SpecialFlag,x	; byte max in the sector
	sta ByteMax,x
	clc			; not an EOF
	rts
.endproc
;;; *** AdvanceReading
;;; *** Move a file forward one sector for reading by
;;; *** following the file links. Expects the IOCB in X
;;; *** Returns carry set if the file ends here.
.proc	AdvanceReading
	lda SpecialFlag,x	; direct access
	bmi advance
	lda NextSector,x	; is there a next sector?
	ora NextSector+1,x
	bne advance
	sec
	rts
advance:
	jsr MoveNextSector	; advance the sector pointer
	jsr ReadFileSector	; read the sector
	lda #0
	sta BytePosition,x	; within the sector
	lda SpecialFlag,x	; boot access?
	bpl fileaccess
	sta ByteMax,x		; 128 bytes per sector
	bmi AdvanceDirect	; advance sector in direct access mode, then leave with C=0
fileaccess:
	tay			; first linkage byte in the sector (also in SpecialFlag)
	lda (FileBuffer),y
	and #$fc
	cmp FileCode,x		; does this fit?
	bne pointerror		; if not, then linkage is broken
	lda (FileBuffer),y	; hi-byte linkage
	and #$03
	sta NextSector+1,x
	iny
	lda (FileBuffer),y
	sta NextSector,x
	iny
	lda (FileBuffer),y
	sta ByteMax,x		; bytes in this sector
	clc
	rts
pointerror:
	Error FileLinkBroken	; could also happen due to invalid point... yuck
.endproc
;;; *** MoveNextSector
;;; *** Advance the file by moving the next sector to
;;; *** the file sector. Expects the IOCB in X
.proc	MoveNextSector
	lda NextSector,x
	sta FileSector,x
	lda NextSector+1,x
	sta FileSector+1,x
	rts
.endproc
;;; *** AdvanceDirect
;;; *** Advance the sector for direct access by simply incrementing it
.proc	AdvanceDirect
	inc FileSector,x
	bne noinc
	inc FileSector+1,x
noinc:	
	clc			; need to clear the carry for no-eof
	rts
.endproc
;;; *** ReleaseSector
;;; *** Mark the sector in FileSector as free
;;; *** and set it in the VTOC.
;;; *** Expects in X the IOCB, destroys the FileSector
.proc	ReleaseSector
	lda FileSector,x
	ora FileSector+1,x
	beq ignore
	;; compute the bitposition and byteposition in the VTOC
	lda FileSector,x
	pha			; keep it
	ldy #3
sloop:
	lsr FileSector+1,x
	ror a
	dey	
	bne sloop
	clc
	adc #$a			; add offset in the VTOC (carry cleared from above)
	bpl insector
	sbc #$79		; special Dos 2.xl wrap-around for 963 sectors
insector:
	tay			; keep the offset
	pla			; extract sector low again
	and #$07		; get bit position
	tax			; to X register
	lda BitMasks,x
	ora (DiskBuffer),y	; or this bit in
	sta (DiskBuffer),y
	ldx ZIOCB		; return IOCB
	ldy #3			; sector count
	sec			; plus one
	jsr Increment
	iny
Increment:
	lda (DiskBuffer),y	; number of free sectors
	adc #0
	sta (DiskBuffer),y
ignore:	
	rts
.endproc
;;; *** AllocateSector
;;; *** Scan the VTOC for a free sector, return it in
;;; *** NextSector if found
.proc	AllocateSector
	lda #$00
	ldx ZIOCB
	sta NextSector,x
	sta NextSector+1,x	; up to now:	No next sector (required on failure)
	ldx #$ff		; VTOC sector counter
	ldy #9			; VTOC entry for sectors 0..7
incsector:
	iny
	bpl nowrap
	ldy #6			; VTOC wrap-around for 2.0s compatible 963 sectors
nowrap:	
	inx
	cpx #122		; wraparound? (122*8 sectors are administrated here)
	bcs diskful		; bail out if the disk is full
	lda (DiskBuffer),y	; is there a free sector here?
	beq incsector		; if not, leave it alone
	txa			; get counter
	ldx ZIOCB
	sta NextSector,x	; keep for a while
	lda (DiskBuffer),y	; fetch again the sector mask
	ldx #$ff
findbit:			; now detect the first free bit
	inx
	asl a
	bcc findbit
	;; we have the bit index now
	lda (DiskBuffer),y	; get again the VTOC table
	eor BitMasks,x		; set it
	sta (DiskBuffer),y	; in the VTOC
	txa			; these are the low bits, need to shift in
	ldx ZIOCB
	ldy #3			; must shift three bytes up to make room for the bits
	lsr a
	ror a
	ror a
	ror a			; move to bits 7..5
shiftloop:
	asl a
	rol NextSector,x
	rol NextSector+1,x
	dey
	bne shiftloop
	ldy #5			; VTOC allocation byte
	lda #$ff
	sta (DiskBuffer),y	; mark the VTOC as busy
	ldy #3			; free sectors entry
	jsr Decrement		; carry is clear due to ror a above
	iny
Decrement:
	lda (DiskBuffer),y
	sbc #0
	sta (DiskBuffer),y
	rts
diskful:
	Error DiskFull
.endproc
;;; *** AllocateFCB
;;; *** Find a free slot for a file management block,
;;; *** the internal analogue of the IOCB
.proc	AllocateFCB
	stx ZIOCB		; keep the iocb index
	tsx
	inx
	inx
	stx FmsStack		; keep the stack ponter
	ldy ZUnit		; get the unit number
	cpy #9
	bcs wrongunit
	lda AvailFlags-1,y	; is this unit available?
	beq wrongunit
	sty SIODeviceUnit	; keep it, need it here anyhow
	;; get the disk buffer
	dey
	jsr Times128		; multiply by buffer size
	adc DiskBufferBase	; get buffer base
	sta DiskBuffer		; the above keeps C cleared
	tya
	adc DiskBufferBase+1
	sta DiskBuffer+1
	;; now allocate the file buffer
	ldx ZIOCB		; read again the IOCB idx
	ldy OpenBuffer,x	; get buffer index
	bpl havebuffer
	;; need to allocate a new file buffer
	ldy #7
allocfb:
	lda FileBufferFlags,y
	beq foundbuffer
	dey
	bpl allocfb
	Error TooManyFiles	; run out of buffers
foundbuffer:
	lda #$ff
	sta FileBufferFlags,y	; reserve this buffer
	tya
	sta OpenBuffer,x	; keep the buffer index
havebuffer:
	jsr Times128		; convert to an offset
	adc FileBufferBase	; get the buffer base
	sta FileBuffer
	tya
	adc FileBufferBase+1
	sta FileBuffer+1	; keep it
	rts
wrongunit:
	Error IllegalUnit
.endproc
;;; *** Times128
;;; *** multiply Y with 128 (=size of a sector) and return
;;; *** the result with lo in A, hi in Y, and carry cleared,
;;; ***	X untouched
.proc	Times128
	tya
	lsr a
	tay
	lda #$00
	ror a
	rts
.endproc
;;; *** FreeBuffer
;;; *** Release the internal buffers used by the
;;; *** current IOCB
.proc	FreeBuffer
	ldy OpenBuffer,x	; get the buffer responsible for us
	bmi isfree		; do not release if it is released
	lda #0
	sta FileBufferFlags,y	; release the buffer
	lda #$ff
	sta OpenBuffer,x	; and ditto here
isfree:
	rts
.endproc
;;; *** FindColon
;;; *** Extend/find the file name in the buffer
;;; *** pointed to by ZAdr, return the colon position
;;; *** in Y
ErrorName:
	Error FileNameInvalid
.proc	FindColon
	ldy #3			; offset to the : hopefully
findcolon:
	dey
	bmi ErrorName		; signal an invalid name if not found
	lda (ZAdr),y		; get the next character of the path
	cmp #':'
	bne findcolon
	rts
.endproc
;;; *** ExtractFileName
;;; *** Extract the file name from the CIO
;;; *** specified path in ZAdr,ZAdr+1
;;; *** must have the IOCB in X.
.proc	ExtractFileName
	lda #$ff
	sta FileNumber,x	; reset the file number
	jsr FindColon
	;; runs into the following
.endproc
;;; *** ExtractNextFileName
;;; *** in case multiple filenames are in the path,
;;; *** extract here the next at the offset given in the Y register
.proc	ExtractNextFileName
	;; reset the filename
	ldx #11
	lda #' '
clrl:
	sta FileNameBuffer,x
	dex
	bpl clrl
	inx
	stx FmsTmp	
	sty ComponentStart	; keep the offset for outside.
nextchr:
	iny			; advance to the next
	lda (ZAdr),y		; get the next component
	cmp #','
	beq endname
	cmp #'/'
	beq endname
	cmp #'*'
	beq wildany		; match any character
	cmp #'-'
	bne notwildall		; do not match all characters
	ror FmsTmp		; keep note of this by setting bit 7: in the extender
wildany:
	lda #'?'
	jsr InsertChar		; insert all the characters
	bcc wildany
	bit FmsTmp		; in the extender?
	bpl nextchr		; if not, continue parsing
	bmi parseextra		; if so, parse extra information
notwildall:
	cmp #'.'		; base/extender separator?
	bne notdot
	bit FmsTmp		; already in the extender?
errorbranch:
	bmi errorname		; if so, signal an error
	ldx #8			; continue insertion here
	ror FmsTmp		; signal that we are in the file
	bcc nextchr		; parse the next character
notdot:
	cmp #'?'		; wildcard?
	beq insertit		; yes, directly into the buffer
	cmp #'A'		; a character?
	bcc notcharacter
	cmp #'Z'+1
	bcc insertit
notcharacter:			; here possibly invalid, or the end of the name
	cpx #0			; the first character?
	beq errorname		; if so, really an error
	jsr CheckDigits		; check if this is a digit or the end (bails out if end)
insertit:
	jsr InsertChar		; place it in the buffer
	cpx #11			; ignore extra characters in the base
	bcc nextchr
parseextra:			; parse extenders behind the name
	iny			; skip the last character, get next
endname:	
	lda (ZAdr),y		; get the next character
	cmp #'/'		; extension modifier?
	bne enddir		; bail out if not
	iny
	lda (ZAdr),y		; get the extension character
	cmp #'V'		; enable verify?
	beq verifyon
	cmp #'O'		; disable veriy?
	beq verifyoff
	cmp #'A'		; set append mode?
	beq setappend
	cmp #'N'		; norun?
	beq setnorun
	cmp #'D'		; open for directory?
	beq setdirmode
noread:
	jsr CheckDigits		; a digit (or end the game)	
	ldx ZIOCB		; get IOCB again
	and #$0f		; mask out
	beq errorname		; signal an error
	sta FileNumber,x
	bne parseextra		; get more modifiers
errorname:
	Error FileNameInvalid
;;; *** CheckDigits
;;; *** Check whether the A contains a valid digit. If so, return
;;; *** it. Otherwise, bail out.
CheckDigits:
	cmp #'0'
	bcc popparse
	cmp #'9'+1
	bcc exitdigit
popparse:
	pla
	pla			; remove the stack entry
enddir:				; bail out position
	lda FileNameBuffer	; anything in here?
	cmp #' '
	beq errorname		; if not, cause an error
	ldx ZIOCB
exitdigit:
	rts
verifyon:	
	lda #'W'		; write as write command
	Skip2
verifyoff:
	lda #'P'		; put as write command
	sta WriteCommand
	bne parseextra		; continue
setappend:
	lda #1			; or this in
	Skip2
setdirmode:
	lda #2
	ora ZAux1
setaux1:
	sta ZAux1
	bcs parseextra		; c is always set here
setnorun:
	lda ZAux1
	and #$8f
	bcs setaux1		; c is always set here
;;; *** InsertChar:
;;; *** insert the character in the accumulator
;;; *** at the offset X in the internal file buffer, increment.
;;; *** deliver C=C if not yet reached the end
InsertChar:
	cpx #8			; length of the filename
	bcc insertnow
	cpx #12			; really the end?
	bcs secend
	bit FmsTmp		; set both?
	bpl secend		; if not, set the carry and bail out
insertnow:
	sta FileNameBuffer,x
	inx
	rts			; leave carry cleared
secend:
	sec
	rts			; here end of buffer reached
.endproc
;;; *** PushError
;;; *** deliver an error that is behind the address of
;;; *** the JSR to here.
.proc	PushError
	pla
	sta FmsPtr
	pla
	sta FmsPtr+1
	ldy #1
	lda (FmsPtr),y
	;; runs into the following
.endproc
;;; *** AError:	return the error in the A register
.proc	AError
	tay			; deliver the error
	;; runs into the following
.endproc
;;; *** YError:	return the error in the Y register
.proc	YError	
	ldx FmsStack
	txs			; reset the stack
	ldx ZIOCB
	sty FmsTmp
	lda IOCBIndex,x		; is it open
	bpl isopen
	jsr FreeBuffer		; release the FCB if the IOCB is also free
isopen:	
	lda ZIOByte		; the return value
	ldy FmsTmp		; set the status flag (mainly for the bload, CIO doesn't require it)
	rts
.endproc
;;; *** ExitFine
;;; *** Exit the FMS with an "OK" result code
.proc	ExitFine
	lda #$01
	bne AError		; load with fine result code
.endproc
;;; *** Convert the file code to a directory
;;; *** sector offset in A
.proc	LoadDirOffset
	ldx ZIOCB
	lda FileCode,x
	asl a
	asl a
	and #$7f
	tay
	rts
.endproc
;;; ***
;;; *** Load the FmsPtr with the current directory entry
;;; ***
.proc	LoadThisDirEntry
	jsr LoadDirOffset
	clc
	adc FileBuffer
	sta FmsPtr
	lda FileBuffer+1
	adc #$00
	sta FmsPtr+1
	rts
.endproc
;;; *** Insert a wildcard filename into the
;;; *** current directory slot by replacing non-wild characters
.proc	InsertFileName
	jsr LoadThisDirEntry
	ldx #0
	ldy #5
copyloop:
	lda FileNameBuffer,x
	cmp #'?'			; use the old entry
	beq keep
	sta (FmsPtr),y			; insert into directory
keep:	
	inx				; until all done
	iny	
	cpx #11
	bcc copyloop
	ldy #5
	lda (FmsPtr),y			; check whether at least the first character is valid
	cmp #' '			; if not, signal an error
	bne fine
	Error FileNameInvalid
fine:	
	rts
.endproc
;;; *** LocateFile
;;; *** Locate a file given its pattern.
;;; *** FIXME:	Since the file pattern is an FMS global,
;;; *** this is not reentrant and opening a directory for reading
;;; *** works only on one stream at once.
;;; *** returns with C set if the file was not found, otherwise with
;;; *** carry cleared.
;;; *** Expects the IOCB number in X
.proc	LocateFile
	lda #$01
	sta FreeDirCode		; no free slot available
	lda #$00
	beq *+12		; loadnextsector
.endproc
;;; *** LocateNextFile
;;; *** get the next file in the directory
;;; *** returns with C set if the file was not found, otherwise with
;;; *** carry cleared.
.proc	LocateNextFile
nextfile:
	ldx ZIOCB
	lda FileCode,x
	clc			; to the next code
	adc #$04
	bcs notfound
loadnextsector:	
	sta FileCode,x
	and #$1f
	bne notnextsector	; start of a new sector entry
	jsr ReadDir
notnextsector:
	jsr LoadThisDirEntry
	ldy #0
	lda (FmsPtr),y		; check the dir entry
	beq freeentry		; end of directory, and a free entry
	bmi freeentry		; a deleted file
	cmp #$63		; volumne name?
	bne novolumename
	;; here: found a volume name. What do to about it?
	lda ZAux1		; check whether we are open for directory reading with headline
	cmp #6
	bne nextfile		; if not, skip it
	lda FileNumber,x	; are we looking for a specific file?
	bpl nextfile		; if so, skip too
	bmi foundpattern	; check whether the pattern matches
novolumename:
	and #$43		; A DOS 2.5 file?
	cmp #$03
	beq cmppattern		; Read it, but mark it implicitly as protected.
	cmp #$42		; or a plain file?
	bne nextfile		; advance if so
cmppattern:			; here:	 compare the found directory entry with the pattern
	ldx #0
	ldy #5
cmploop:
	lda FileNameBuffer,x	; get the filename
	cmp #'?'		; wildcard?
	beq skip
	cmp (FmsPtr),y		; does the filename match?
	bne nextfile		; if not, skip
skip:
	inx
	iny
	cpx #11			; all entries
	bcc cmploop
foundpattern:
	;; here: found a match
	ldx ZIOCB
	clc			; signal a "file found"
	lda FileNumber,x
	bmi foundfile		; if unnumbered, found the file
	dec FileNumber,x	; otherwise, count down
jnext:				; trampoline jump target
	bne nextfile
	beq foundfile
	;; here: found end of directory
freeentry:
	lda FreeDirCode		; already found a free slot?
	lsr a
	bcc haveslot
	lda FileCode,x
	sta FreeDirCode
haveslot:
	lda (FmsPtr),y		; is this the end of the directory?
	bne nextfile		; if not, continue searching
notfound:
	sec			; not found
foundfile:
exit:
	rts
.endproc
;;; *** TestForProtection
;;; *** Test whether a file is locked or not
;;; *** If the file is locked, return an error
.proc	TestForProtection
	jsr LoadDirOffset
	lda (FileBuffer),y	; get the status
	and #$60		; protection bit set? Dos 2.5 bit cleared?
	cmp #$40
	beq isfree
	Error FileProtected
isfree:
	rts
.endproc
;;; *** DiskGet
;;; *** Read a byte from disk
	.global FmsGet
.proc	FmsGet
	jsr AllocateFCB		; allocate device buffers
	lda FileAux1,x		; check the open mode
	and #2			; directory reading?
	beq regular
	;; here reading from the directory. Restore the context
	jsr LoadThisDirEntry
	ldy BytePosition,x
	lda NextSector+1,x
	pha
	lda NextSector,x
	pha
	rts
regular:
	ldy BytePosition,x	; get the byte position within the sector
	tya
	cmp ByteMax,x		; at the end of the sector
	bcc partial		; read a partial sector first
	jsr BurstGet		; read the sector in burst mode
	jsr AdvanceSector	; get the next one
	bcs EOF
	ldy #0			; we read now from offset zero
partial:
	lda (FileBuffer),y	; get the byte
	sta ZIOByte		; store as output byte
	;; runs into the following
.endproc
;;; *** TestNearEOF:	
;;; *** Increment file pointer, check for near-EOF
;;; *** returns 1 or 3 for near-EOF positions.
.proc	TestNearEOF
	iny
	tya
	sta BytePosition,x	; keep it
	cmp ByteMax,x		; end of the sector reached?
	bcc fine
	lda NextSector,x
	ora NextSector,x	; end of file nearby?
	bne fine
	Error NearEOF		; EOF is near
fine:
	jmp ExitFine
.endproc
;;; *** EOF:	Signal an EOF
.proc	EOF
	Error EndOfFile
.endproc
;;; *** POBDiskPut:	The PutOneByte vector entry for put
;;; *** requires some preparation for bursting
.proc	POBPut
	tay			; keep value	
	lda IOCBUnit,x		; get unit
	sta ZUnit		; keep it
	;stx ZIOCB		; Store IOCB # (done by AllocateFCB)
	lda #8			; set a generic write command
	sta ZCmd		; to avoid bursting	
	and FileAux1,x		; writing allowed?
	bne dowrite
	ldy #InputOnly		; signal an error otherwise
	rts
dowrite:
	tya
	;; runs into the following
.endproc
;;; *** DiskPut:	The CIO based Put routine
;;; *** provides bursting
	.global FmsPut
.proc	FmsPut
	sta ZIOByte		; keep data
	jsr AllocateFCB		; allocate the disk buffer
putloop:
	ldy BytePosition,x	; get the byte pointer
	tya
	cmp ByteMax,x		; at the edge of a sector?
	bcc noburst
	jsr AdvanceSector	; go to the next sector
	bcs eof			; if not, signal an EOF
	jsr BurstPut		; write for bursting
	ldy #0			; we write now at offset zero
	;; bursting reloads ZIOByte correctly.
noburst:
	lda ZIOByte		; retrieve the byte back
	sta (FileBuffer),y	; insert
	lda #$40
	ora AccessFlags,x	; mark the buffer as dirty
	sta AccessFlags,x
	bpl TestNearEOF		; allow error 3 in case we are not allocating sectors
	inc BytePosition,x
	jmp ExitFine		; otherwise, well done.
	;; here: detected an EOF on writing.
	;; check whether we can extend beyond it
eof:	
	jsr ExtendWriteEOF	; can we extend the file beyond EOF?
	bcs EOF			; if not so, signal a true EOF
	bcc putloop		; otherwise, retry putting this byte after resolving the sector
.endproc
;;; *** ExtendWriteEOF
;;; *** extend a file beyond an EOF after writing.
;;; *** return with carry cleared if this is possible
.proc	ExtendWriteEOF
	lda FileAux1,x		; get the open mode again: May we extend the file?
	eor #8			; writing? (This happens if we have used POINT in open mode #8)
	beq iswriting
	and #1			; is extending allowed?
	beq exit		; if not, bail out with carry still set
	lda ByteMax,x		; required for OpenForAppend to place the byte pointer correctly
	sta BytePosition,x	; continue beyond EOF (should be == for manual extension in mode 13)
iswriting:
	;; here: file must get extended at this position
	lda #$80		; set the "create links" flag now
	sta AccessFlags,x
	lda SpecialFlag,x	; get sector maximum count
	sta ByteMax,x		; store maximum byte count
	clc			; go on writing
exit:	
	rts
.endproc
;;; *** BurstPut
;;; *** Burst this sector onto the diskdrive from
;;; *** memory into the diskdrive
.proc	BurstPut
	lda #$80
	Skip2
	;; runs into the following
.endproc
;;; *** BurstGet
;;; *** Burst this sector or a group of sectors directly
;;; *** into the memory, bypassing CIO
.proc	BurstGet
	lda #$00		; set read access (gets skipped)
	sta FmsTmp		; keep the access mode
	lda ZCmd		; block or record IO
	and #2
	beq exit		; if record IO, then no burst
burstloop:
	lda ByteMax,x		; we must have at least one more byte than there is room here
	ldy ZLen+1		; a page or more
	bne burst
	cmp ZLen		; if not at least a complete sector, leave it alone
	bcs exit
burst:
	tay			; sector length
	dey			; start at last byte
	bit FmsTmp		; reading or writing?
	bpl burstread
	sta BytePosition,x	; store sector length as number of bytes in the buffer
fill:
	lda (ZAdr),y
	sta (FileBuffer),y	; copy over
	dey			; up to the first byte
	bpl fill
burstread:
	jsr AdvanceSector	; to the next sector now
	bcs exit		; bail out if EOF reached
	bit FmsTmp		; reading or writing
	bmi advancebuffer	; increment the buffer pointer
	ldy ByteMax,x		; get # of bytes in this sector
	dey
read:
	lda (FileBuffer),y
	sta (ZAdr),y		; copy into the sio supplied buffer
	dey
	bpl read
advancebuffer:
	clc
	lda ZAdr
	adc ByteMax,x		; bytes written
	sta ZAdr
	bcc nocarry1
	inc ZAdr+1
nocarry1:
	ldy #0
	lda (ZAdr),y
	sta ZIOByte		; for write access: update the next byte to be written
	
	sec
	lda ZLen
	sbc ByteMax,x
	sta ZLen
	bcs burstloop
	dec ZLen+1
	bcc burstloop
exit:
	rts
.endproc
;;; *** InitFilePointer
;;; *** Initialize the file pointer from the
;;; *** directory to start following the linkage
.proc	InitFilePointer
	jsr InitializeFCBForRead	; initialize the FCB for reading
	jsr LoadThisDirEntry
	ldy #1
	lda (FmsPtr),y			; initialize the file length
	sta FileLength,x
	iny
	lda (FmsPtr),y
	sta FileLength+1,x
	iny
	lda (FmsPtr),y			; get the first sector
	sta NextSector,x
	iny
	lda (FmsPtr),y
	sta NextSector+1,x
	jmp AdvanceReading		; get now the first file sector into the buffer
.endproc
;;; *** InitializeFCBForRead initialize the file control block
;;; *** such that we read (and do not write) the links
.proc	InitializeFCBForRead
	lda #$00		; zero it
	;; runs into the following
.endproc
;;; *** InitializeFCB
;;; *** Initialize the FCB with the accessflags in A
.proc	InitializeFCB
	sta AccessFlags,x	
	lda SpecialFlag,x	; file open mode
	sta ByteMax,x		; initialize maximum number of bytes
	lda #$00
	sta BytePosition,x	; position within the sector is zero
	sta FileLength,x	; reset the length
	sta FileLength+1,x
	rts
.endproc
;;; *** FmsOpen
;;; *** Disk open vector, for use by the CIO
	.global FmsOpen
.proc	FmsOpen
	jsr AllocateFCB
	jsr CheckDevice		; disk present?
	jsr ExtractFileName	; get the filename
	;; set the put one byte vector for direct handler access
	lda #<(POBPut-1)
	sta ZPut
	lda #>(POBPut-1)
	sta ZPut+1
	lda #125		; bytes per sector (regular case)
	sta SpecialFlag,x
	lda ZAux2		; boot access?
	and #$80
	beq regular
	sta SpecialFlag,x	; set the special flag = full sector length (A is 0x80 now)
	lda ZAux1		; check mode again
	sta FileAux1,x		; keep the mode
	and #3			; valid?
	bne ErrorMode	
	lda ZAux1
	and #8			; a write mode?
	beq reading		; read access?
	lda #$80		; tell the bursting that we need to recreate links (which are not present... (-;)
reading:
	jsr InitializeFCB	; the precise DirEntry is irrelevant as long as it is even
	ldy #1			; do not use ExitFine as it would dispose the buffer
	rts
ErrorMode:
	Error InvalidMode
	;; in the following:	regular device access
regular:
	jsr LocateFile		; try to find the pattern
	lda ZAux1
	sta FileAux1,x		; keep the mode
	bcs notfound		; check what we do if it is not found
ignore:
	php			; keep flags:	C set for not found, C cleared for found (OpenDirectory requires this)
	sec
	sbc #4
	bcc ErrorMode
	cmp #14-3		; max possible mode is 13
	bcs ErrorMode
	tay			; get index
	plp			; restore flags
	lda OpenVectorHi,y
	pha
	lda OpenVectorLo,y
	pha
	rts			; call it
notfound:			; handling if we need to open a file that is not there
	cmp #8			; only open for write and directory allows this
	beq CreateFile		; create it
	cmp #6
	beq ignore		; and set carry
	cmp #7
	beq ignore		; and set carry
	Error FileNotFound	; otherwise, an error
;;; table containing various open modes (no self modifying code possible here)
;;; starts at mode #4
OpenVectorLo:	.byte <(OpenForRead-1),<(OpenForRead-1),<(OpenDirectory-1),<(OpenDirectory-1)
		.byte <(OpenForWrite-1),<(OpenForAppend-1)
		.byte <(ErrorMode-1),<(ErrorMode-1),<(OpenForUpdate-1),<(OpenForUpdateAppend-1)
OpenVectorHi:	.byte >(OpenForRead-1),>(ErrorMode-1),>(OpenDirectory-1),>(OpenDirectory-1)
		.byte >(OpenForWrite-1),>(OpenForAppend-1)
		.byte >(ErrorMode-1),>(ErrorMode-1),>(OpenForUpdate-1),>(OpenForUpdateAppend-1)
;;; *** Open a file for reading/writing/appending
OpenForUpdateAppend:
	jsr TestForProtection	; is the file locked? If so, fail.
	jsr MarkFileBusy	; make the file busy
OpenForUpdate:			; ditto, but less restrictive
	jsr TestForProtection	; no need to make the file busy
OpenForRead:
	jsr InitFilePointer	
	ldy #1			; do not use ExitFine as it would dispose the buffer
	rts
OpenForAppend:			; open for appending
	jsr TestForProtection	; may we write into it?
	jsr MarkFileBusy	; since we may append it
	jsr InitFilePointer	; locate the first sector of the file
	;; now wind the file up to the EOF
windloop:
	jsr AdvanceReading
	bcc windloop		; until EOF
	jsr ExtendWriteEOF	; extend the file behind EOF	
	ldy #1			; do not use ExitFine as it would dispose the buffer
	rts
CreateFile:			; create a new entry
	lda FreeDirCode		; found a free directory slot?
	ror a
	bcs dirfull
	rol a
	sta FileCode,x		; allocate the directory here
	jsr ReadDir		; and read it
	jsr MarkVTOCBusy	; we need to allocate sectors now
	jsr LoadThisDirEntry
	ldy #5			; clear the directory slot now
	lda #' '
clearslot:	
	sta (FmsPtr),y
	iny
	cpy #16
	bcc clearslot
	jsr InsertFileName	; copy it in
	bne contwrite		; jumps always
OpenForWrite:			; open a file for writing
	jsr InsertFileName	; copy it in:	Must do this first as it checks the pattern for validity
	jsr DeleteFile		; delete the slot
contwrite:			; from above:	file writing
	jsr AllocateSector	; for the first sector of the file
	jsr LoadThisDirEntry
	ldy #3
	lda NextSector,x
	sta (FmsPtr),y
	lda NextSector+1,x	; insert the first file sector
	iny
	sta (FmsPtr),y
	jsr MarkOnlyFileBusy	; write it back as busy file
	lda #$80		; mark access mode as allocating
	jsr InitializeFCB	; initialize remaining data
	jsr EnlargeFile		; bump size, switch to next sector
	;; special code for DOS.SYS is missing. There is no such thing....	
	ldy #1			; do not use ExitFine as it would dispose the buffer
	rts
dirfull:
	Error DirectoryFull
.endproc
;;; *** DeleteFile
;;; *** Remove a file and release all its sectors
.proc	DeleteFile
	jsr TestForProtection	; are we protected?
	jsr MarkVTOCBusy	; need to access it now
	jsr LoadDirOffset	; get the offset
	lda #$80
	sta (FileBuffer),y	; release this slot
	jsr WriteDir		; Put the directory back
	jsr InitFilePointer	; get the first entry
eraseloop:
	jsr ReleaseSector
	jsr AdvanceReading	; get the next one
	bcc eraseloop		; until EOF
	ldy #5
	lda #$ff
	sta (DiskBuffer),y	; set the VTOC as busy
	jmp ReadDir		; re-read the directory buffer
.endproc
;;; *** FmsClose
;;; *** Close a disk file again
;;; *** possibly write back the directory and VTOC
	.global FmsClose
.proc	FmsClose
	lda OpenBuffer,x	; if the buffer is not open, ignore
	bmi exitim		; this could happen because an OPEN failed with error
	jsr AllocateFCB		; get the FCB for this operation
	lda FileAux1,x		; was it open for writing?
	and #8
	beq exit		; if not, no need to update anything, just bail out
	rol AccessFlags,x	; check whether we are in allocation link mode
	bcs completelinks
	rol AccessFlags,x	; check whether the sector buffer is dirty
	bcc cleanbuffer
	;; here the buffer is dirty, but we are within an existing sector. Therefore,
	;; we must not reset the byte count of this sector
	jsr WriteFileSector	; just write the data back
	bpl cleanbuffer		; jumps always
completelinks:
	jsr LinkNextSector	; write out the linkage
cleanbuffer:
	;; now check whether we need to update the directory entry as well
	lda FileAux1,x
	cmp #12			; udpate did not touch the directory entry
	beq exit
	jsr ReadDir		; read directory back
	jsr LoadDirOffset	; get the offset back
	lda #$42		; change the state back
	sta (FileBuffer),y
	iny
	lda FileLength,x	; place the file length back
	sta (FileBuffer),y
	iny
	lda FileLength+1,x
	sta (FileBuffer),y	; insert back into the directory
	jsr WriteDir		; place directory back
	jsr WriteVTOC		; write VTOC out
exit:	
	jsr FreeBuffer		; release the file buffer
	jmp ExitFine		; since this is not done here since the IOCB is still busy right here.
exitim:				; no stack management here
	ldy #$01
	rts
.endproc
;;; *** FmsStatus
;;; *** check the status flags
	.global FmsStatus
.proc	FmsStatus
	jsr AllocateFCB
	jsr CheckDevice
	jsr ExtractFileName	; check whether the file is fine
	jsr LocateFile
	bcs notfound
	jsr TestForProtection
	jmp ExitFine
notfound:
	Error FileNotFound
.endproc
;;; *** DirWaitNext
;;; *** Prepare to read the next byte from the
;;; *** directory buffer, return the result in A.
.proc	DirWaitNext
	sta ZIOByte
	ldx ZIOCB
	tya
	sta BytePosition,x
	pla
	sta NextSector,x
	pla
	sta NextSector+1,x
	lda ZIOByte
	ldy #1			; do not use ExitFine as it would dispose the buffer
	rts
.endproc
;;; *** OpenDirectory
;;; *** open the directory for reading
;;; *** Must get called with carry set for not found, 
;;; ***	and carry cleared for object found
.proc	OpenDirectory
	bcs eof			; if here, then the directory is empty
directoryloop:
	lda #$9b
	jsr DirWaitNext
	ldy #$0
	lda #' '		; = $20 (the protected bit)
	and (FmsPtr),y		; check whether the file is protected
	beq iswriteable
	lda #'*'		; write protected
iswriteable:
	ora #' '
	jsr DirWaitNext
	lda #' '		; next is always blank
	jsr DirWaitNext
	lda (FmsPtr),y		; check for headline
	cmp #$63
	beq cphdline
	ldy #4			; just in front of the regular name
cphdline:
	iny
	lda (FmsPtr),y
	jsr DirWaitNext
	cpy #15			; up to the end of the name
	bcc cphdline
	ldy #0
	lda (FmsPtr),y		; check for headline
	cmp #$63
	beq donext
	lda #' '		; one blank
	jsr DirWaitNext
	ldy #2
	lda (FmsPtr),y		; get length, hi-byte
	tax
	dey
	lda (FmsPtr),y		; get length, lo-byte
	tay
	jsr Modulo100		; 100 digit
	jsr DirWaitNext
	jsr Modulo10Next	; 10th digit
	jsr DirWaitNext
	tya
	ora #'0'
	jsr DirWaitNext		; 1th digit
donext:
	jsr LocateNextFile	; advance to the next file in the scan
	bcc directoryloop
	;; here: end of directory found
eof:	
	lda #$9b
	jsr DirWaitNext
	jsr ReadVTOC		; get the VTOC for the free sectors
	ldy #4
	lda (DiskBuffer),y	; get hi
	tax
	dey
	lda (DiskBuffer),y	; get low
	tay
	jsr Modulo100		; 100 digit
	jsr DirWaitNext
	jsr Modulo10Next	; 10th digit
	jsr DirWaitNext
	tya
	ora #'0'
	ldy #0
freetext:
	jsr DirWaitNext		; 1th digit
	;; deliver "FREE sectors"
	iny
	lda FreeTxt-1,y
	bpl freetext
	jsr DirWaitNext
	Error EndOfFile
FreeTxt:
	.byte " FREE SECTORS",$9b
FreeTxtLen	=	*-FreeTxt
.endproc
;;; *** Modulo10Next
;;; *** Continue the computation with the tenth-digit
.proc	Modulo10Next
	ldx #0			; hi-byte is zero for 100th remainder
	lda #10
	Skip2
.endproc
;;; *** Modulo100
;;; *** Compute the modulus of the number (X,Y) by A
;;; *** return the divisor digit in A, return the
;;; *** low-byte of the remainder in Y.
.proc	Modulo100
	lda #100
	sta FmsTmp		; keep divident
	txa
	ldx #$ff		; counter
subloop:
	sty FmsPtr		; keep lo
	sta FmsPtr+1		; keep hi
	inx
	sec
	tya			; get low
	sbc FmsTmp		; subtract modulus
	tay			; keep low
	lda FmsPtr+1		; get hi
	sbc #0			; and carry over
	bcs subloop		; until all done
	txa			; the result
	ldy FmsPtr
	ora #'0'
	rts
.endproc
;;; *** FmsSpecial
;;; *** special XIO commands for the FMS
	.global FmsSpecial
.proc	FmsSpecial
	jsr AllocateFCB
	lda ZCmd	
	cmp #CmdFormat		; classical format?
	bne standard
	lda #CmdFormatStandard
standard:
	cmp #43+1		; maximum
	bcs invalid
	sbc #32-1		; carry is cleared
	bcc invalid

	tay			; to the index register
	
	lda SpecialHi,y
	pha
	lda SpecialLo,y
	pha			; prepare for the jump:	target address
		
	lda AgendaByte,y	; check what we had to do here
	sta FmsTmp
	eor IOCBIndex,x
	bmi channelstatus	; must be ether opened or closed
	

	bit FmsTmp		; get again for CheckDevice and ExtractFileName
	bpl mustopen		; and hence is checked already
	jsr CheckDevice		; check whether the device is available then
mustopen:
	bit FmsTmp		; extract a file name?
	bvc nofile
	jsr ExtractFileName
nofile:	
	rts			; call it
channelstatus:
	ldy #ChannelInUse
	lda IOCBIndex,x
	bpl isopen
	ldy #ChannelNotOpen
isopen:
	Skip2
invalid:
	ldy #UnsupportedCmd
	jmp YError
SpecialLo:
	.byte <(FmsRename-1),<(FmsDelete-1),<(FmsFind-1),<(FmsLock-1),<(FmsUnlock-1),<(FmsPoint-1),<(FmsNote-1)
	.byte <(FmsInitDisk-1),<(FmsFind-1),<(FmsBload-1),<(FmsFormat-1),<(FmsFormatStandard-1)
SpecialHi:
	.byte >(FmsRename-1),>(FmsDelete-1),>(FmsFind-1),>(FmsLock-1),>(FmsUnlock-1),>(FmsPoint-1),>(FmsNote-1)
	.byte >(FmsInitDisk-1),>(FmsFind-1),>(FmsBload-1),>(FmsFormat-1),>(FmsFormatStandard-1)
AgendaByte:			; $80:	channel must be closed for operation, $40: ExtractFileName
	.byte	$c0,$c0,$c0,$c0,$c0,$00,$00
	.byte	$80,$c0,$c0,$80,$80
.endproc
;;; *** FmsFind
;;; *** Resolve a wildcard filespec
.proc	FmsFind
	lda ZAux1
	and #2			; has been modified by /D?
	bne FileNotFoundError	; if so, skip:	Keep this filename as it is, including the /D
	lda FileNumber,x	; a specific filenumber?
	bpl keepfile
	lda ZAux2		; get the file counter
	beq keepfile		; ignore if the argument is zero
	sta FileNumber,x	; and store it
keepfile:		
	jsr LocateFile		; check whether we can find it
	bcs FileNotFoundError	; signal an error if not found
	jsr LoadThisDirEntry
	ldy #5
	sty DirEntryOffset
extract:
	lda (FmsPtr),y
	cmp #' '
	beq skip
	cpy #8+5
	bne notextender
	pha
	lda #'.'		; insert a dot in front of the extender
	jsr insert
	pla	
notextender:
	jsr insert
skip:	
	inc DirEntryOffset
	ldy DirEntryOffset
	cpy #16
	bcc extract
	lda #$9b		; EOF append
	jsr insert
	jmp ExitFine
insert:	
	ldy ComponentStart
	iny
	sta (ZAdr),y
	sty ComponentStart
	rts
.endproc
FileNotFoundError:
	Error FileNotFound
;;; *** FmsRename
;;; *** Rename a filespec
.proc	FmsRename	
	tya			; still loaded with the offset from ExtractFileName
	pha			; keep where this name ended
	jsr LocateFile		; try to find the source filespec
	bcs FileNotFoundError	; bail out with an error if not available
	pla
	tay			; restore the offset
	jsr ExtractNextFileName	; get the target name
renameloop:
	jsr TestForProtection	; check whether the located source is write protected
	jsr InsertFileName	; overwrite by the target name
	jsr WriteDir
	jsr LocateNextFile	; get the next matching pattern
	bcc renameloop		; continue until all files renamed
	jmp ExitFine
.endproc
;;; *** FmsDelete
;;; *** Delete a filespec
.proc	FmsDelete	
	jsr LocateFile		; try to find it
	bcs FileNotFoundError	; bail out if not found
	lda #125		; bytes per sector (regular case)
	sta SpecialFlag,x
deleteloop:
	jsr DeleteFile		; delete it
	jsr LocateNextFile	; advance to the next
	bcc deleteloop
	jsr WriteVTOC		; write the modified VTOC back
	jmp ExitFine		; done with it
.endproc
;;; *** FmsLock
;;; *** Enable write protection for a filespec
.proc	FmsLock
	lda #$20		; new mask
	Skip2
	;; runs into the following
.endproc
;;; *** FmsUnlock
;;; *** Remove write protection from a filespec
.proc	FmsUnlock
	lda #$00		; ditto
	sta FmsTmp		; keep it temporarely	
	jsr LocateFile
	bcs FileNotFoundError
lockloop:
	jsr LoadDirOffset	; entry in the directory buffer
	lda (FileBuffer),y	; get the old setting
	and #$df		; mask the protection bit out
	ora FmsTmp		; insert the new bit
	sta (FileBuffer),y
	jsr WriteDir		; write it back
	jsr LocateNextFile	; and advance
	bcc lockloop
	jmp ExitFine		; done with it
.endproc
;;; *** FmsPoint
;;; *** Set the sector pointer
.proc	FmsPoint
	lda FileAux1,x
	and #$02		; open for directory?
	bne invalidpoint
	lda IOCBAux3,x
	cmp FileSector,x	; are we in the current sector?
	bne notsame
	lda IOCBAux4,x
	sbc FileSector+1,x
	beq samesector		; if so, then this is the same sector
notsame:	
	;; now check whether we must write the current sector back
	lda AccessFlags,x	; check whether the sector buffer is dirty
	beq keepsector
	lda FileSector,x	; get the current sector
	ora FileSector+1,x	; on direct access, this might be initialy zero. Then, no sector needs to be written
	beq keepsector
	jsr LinkNextSector	; this does also the right thing in case we are in update mode and while direct mode
keepsector:
	lda AccessFlags,x	; reset the dirty flag
	and #$bf
	ldy SpecialFlag,x	; is boot, so keep the "create links" flag
	bmi resetonly
	lda #0			; otherwise, assume that we pointed in front of the EOF and switch to follow links
resetonly:
	sta AccessFlags,x	; mark sector buffer as clean, assume that we pointed in front of EOF
	;; and enforce following the links, not creating them.
	lda IOCBAux3,x
	sta NextSector,x	; store where we want to continue
	lda IOCBAux4,x		; ditto
	sta NextSector+1,x
	jsr AdvanceReading	; get the next sector, read this sector.
	bcs invalidpoint	; create an error if both are zero (and we wanted to point to the EOF)
samesector:			; now, that's the easy case
	lda IOCBAux5,x		; get target byte position
	cmp ByteMax,x		; not beyond EOF
	beq fine		; at the edge is also acceptable
	bcs invalidpoint
fine:
	sta BytePosition,x	; keep me
	jmp ExitFine
invalidpoint:
	Error InvalidPoint
.endproc
;;; *** FmsNote
;;; *** Note a sector
.proc	FmsNote
	lda BytePosition,x
	sta IOCBAux5,x
	lda FileSector,x
	sta IOCBAux3,x		; just deliver the sector
	lda FileSector+1,x
	sta IOCBAux4,x		; and hi
	jmp ExitFine
.endproc
;;; *** FmsFormatStandard
;;; *** Format a disk enhanced density, 963 sectors
.proc	FmsFormatStandard
	lda #34			; enhanced format command
	bne Format		; jump into the real formatter
.endproc
;;; *** FmsFormat
;;; *** Format a disk, user controlled. AUX1 is the format command,
;;; *** aux2 the number of bytes to set in the VTOC
.proc	FmsFormat
	lda ZAux1		; disk format command (could be 33 or 34)
	;; Format
.endproc
;;; *** Format
;;; *** Format a disk with parameters in A
.proc	Format
	sta FmsPtr
	jsr LoadFileBuffer	; prepare for the file buffer
	;; remaining data is set correctly from CheckDevice,
	;; the rest is done by DiskInterf which now also
	;; supports enhanced density format	
	lda FmsPtr		; reload the command in A
	jsr RunDiskInterf
	bpl InitDisk		; init the disk now (jumps always)
.endproc
;;; *** FmsInitDisk
;;; *** Initialize a disk by clearing the VTOC
;;; *** This is a quick format
.proc	FmsInitDisk
	ldy #34
	bit DiskStatus		; is it single density?
	bmi single
	dey
single:
	sty FmsPtr		; write bytes
.endproc
;;; *** InitDisk
;;; *** Initialize the disk sectors
.proc	InitDisk
	lda #0
	tay
initdsk:
	sta (DiskBuffer),y	; initialize buffer
	iny
	bpl initdsk
	tay			; reset Y
	lda #2
	sta (DiskBuffer),y	; set flag: Dos 2 compatible
	iny
	lda #$c3		; #of free sectors for the disk (lo)
	sta (DiskBuffer),y	; free sectors
	iny
	iny
	sta (DiskBuffer),y
	lda FmsPtr
	sec
	sbc #33-2		; $2c3 sectors for low density, $3c3 for high density
	dey
	sta (DiskBuffer),y
	iny
	iny
	sta (DiskBuffer),y	; enter high-byte
	ldx #$ff		; reset value
	cmp #3			; long or short
	txa
	bcc short
	ldy #6			; ditto
resetlong:
	sta (DiskBuffer),y
	iny
	bpl resetlong
	bmi reserve
short:	
	ldy #10			; start classical VTOC
resetshort:
	sta (DiskBuffer),y
	iny
	cpy #100		; reserve all up to byte 100
	bcc resetshort
reserve:
	;; now reserve bytes for VTOC and directory
	ldy #$0a
	lda #$0f
	sta (DiskBuffer),y
	ldy #$37
	lda #$00
	sta (DiskBuffer),y
	iny
	lda #$7f
	sta (DiskBuffer),y
	jsr WriteVTOC
	;; now reset the directory
	lda #$00
	tay
resetdir:
	sta (FileBuffer),y
	iny
	bpl resetdir
clrlp:
	sta FileCode,x
	jsr WriteDir
	clc
	lda FileCode,x
	adc #$20
	bcc clrlp
	sta FileCode,x		; Prepare to write again the first directory
	;; now write the headline
	jsr FindColon
	iny
	lda (ZAdr),y		; a usable disk name?
	bmi nohdr
	sty FmsTmp
	ldy #$f			; end offset, clear the headline buffer
	lda #' '|$80		; inverse blank
hdclear:
	sta (FileBuffer),y
	dey
	bne hdclear
	lda #$63		; headline status
	sta (FileBuffer),y
	sty FmsPtr
	;; insert the headline now
hd3l:
	ldy FmsTmp
	lda (ZAdr),y
	bmi done
	inc FmsTmp
	inc FmsPtr
	ora #$80		; make inverse
	ldy FmsPtr
	sta (FileBuffer),y
	cpy #$f
	bcc hd3l
done:
	jsr WriteDir
nohdr:
	jmp ExitFine
.endproc
;;; *** FmsBload
;;; *** load & execute a binary file
.proc	FmsBload
	lda FmsStack
	pha			; keep the stack pointer, we need to call us recursively
	lda ZAux1		; get the read mode
	sta BloadFlags		; keep it
	stx BloadIOCB		; Keep IOCB
	lda #0
	sta IOCBAux2,x
	lda #4
	sta IOCBAux1,x		; open for read
	lda #CmdOpen
	jsr DispatchCIOCommand	; open the file for reading
	bmi errorexit		; on failure, out
	lda #>StartAddress
	ldy #<StartAddress
	jsr InitAddress		; load to here
	jsr GetBlock		; try to read the header
	bmi errorexit		; fail if not available
	lda StartAddress
	and StartAddress+1	; must be 0xff,0xff
	cmp #$ff
	bne nobinary		; bail out on error
	;; do not overwrite init and run unless we know its a binary
	ldy #0
	jsr VectorInit		; initialize the run vector
	ldy #2
	jsr VectorInit		; initialize the init vector
loadloop:
	lda #>StartAddress
	ldy #<StartAddress
	jsr InitAddress		; get the start address
	jsr GetBlock
	bpl continue
	;; here:	an error on the file start. EOF would be legal here
	cpy #EndOfFile
	bne errorexit
	ldy #1			; exit is fine
errorexit:			; here error on exit
	tya
	pha
	lda #CmdClose
	ldx BloadIOCB
	jsr DispatchCIOCommand
	pla
	tay
	bmi reter
	bit BloadFlags		; shall we run it?
	bvc norun
	jsr CallRunVector	; if so, launch the program now
norun:
	ldy #1
reter:	
	pla
	sta FmsStack		; restore stack
	jmp YError		; bail out, deliver possible error code in Y
nobinary:
	ldy #NoBinaryFile	; deliver this as error
	bmi errorexit
continue:
	;; check whether we have here an ff,ff pair. If so, skip this ("compound file") and redo
	lda StartAddress
	and StartAddress+1
	cmp #$ff
	beq loadloop
	lda #>EndAddress
	ldy #<EndAddress	; get the end address now
	jsr InitAddress
	jsr GetBlock
	bmi errorexit
	;; now load the block
	ldy StartAddress
	lda StartAddress+1
	jsr InitAddress		; initialize where to load to
	sec
	lda EndAddress
	sbc StartAddress
	tay			; keep low
	lda EndAddress+1
	sbc StartAddress+1	; compute high
	bcc nobinary		; outch!
	iny			; for the length, add one
	bne nocarry
	adc #0			; carry is set by above test
nocarry:
	jsr InitSize		; install this as size
	jsr GetBlock		; read this block into the file
	bmi errorexit
	bit BloadFlags		; call the init vector now?
	bpl loadloop		; continue loading if not
	jsr CallInitVector	; initialize the file
	ldy #2
	jsr VectorInit		; restore the vector contents
	jmp loadloop
;;; *** initialize an init vector
VectorInit:
	lda #<NoRun
	sta RunVector,y
	lda #>NoRun
	sta RunVector+1,y
NoRun:
	rts
CallRunVector:
	jmp (RunVector)
CallInitVector:
	jmp (InitVector)
;;; *** InitAddress:	Initialize the load address
;;; *** Y is low, A is high
InitAddress:
	ldx BloadIOCB		; get the IOCB#
.endproc
;;; *** more helpers
.proc	InitCIOAddress
	sta IOCBAdr+1,x		; hi
	tya
	sta IOCBAdr,x		; lo
	ldy #2		
	lda #0			; initialize the default size to two
.endproc
;;; *** InitSize:	Initialize the size, A is high, Y is low
.proc	InitSize
	sta IOCBLen+1,x
	tya
	sta IOCBLen,x
	rts
.endproc
;;; *** Read a block from CIO
.proc	GetBlock
	lda #7
.endproc
;;; *** Dispatch a CIO Command
.proc	DispatchCIOCommand
	sta IOCBCmd,x
	jmp CIOVector
.endproc

