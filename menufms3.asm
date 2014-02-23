;;; **********************************************************************
;;; ** Thor Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: menufms3.asm,v 1.5 2013/06/02 20:41:06 thor Exp $         	**
;;; **                                                                  **
;;; ** In this module:   DOS 3 emulation handler			**
;;; **********************************************************************

        .include "menujmptab.i"
	.include "kernel.i"
	.include "cio.i"
	.include "sio.i"
	.include "fms.i"
	.include "errors.i"
	.segment "menufunction"

;;; ***
;;; *** Useful macro definitions
;;; ***
;;; *** Deliver an error to the internal error handler
.macro  Error errno
        jsr PushError
        .byte errno
.endmacro
.macro  Skip2
        .byte $2c
.endmacro
.macro  Skip1
        .byte $24
.endmacro

;;; *** Program init.
;;; *** The built infrastructure requires that a JMP instruction goes here.
	jmp FMS3Init
;;; *** The destructor function
.proc	Destructor
	jmp unload
chain:	
	jmp RTSVector		;will be patched over
unload:
	jsr Remove
	jmp chain		;destroy the rest of the stuff
.endproc
;;; *** Init function
.proc	FMS3Init
	ldx #'3'
	lda #>HandlerTable
	ldy #<HandlerTable
	jsr MountHandlerVector
	bcs error
	ldx #<Destructor
	ldy #>Destructor
	jsr InstallDestructor
	lda #InstalledL
	ldx #<Installed
	ldy #>Installed
	jsr Print
	jsr CursorOff
error:
	rts
Installed:	.byte 155,155,"Dos 3 handler by THOR - Software",155,155
		.byte "Use ",34,"Duplicate file(s)",34," to convert",155
		.byte "DOS 3 files by copying from 3: to D:.",155,155
		.byte "The 3: device is not available in",155
		.byte "external functions.",155
InstalledL	=	*-Installed
.endproc
;;; *** Remove
;;; *** Remove the 3: handler again
.proc	Remove
	lda #'3'
	ldx #0
loop:	cmp HaTabs,x
	beq found
	inx
	inx
	inx
	cpx #$22
	bcc loop
	;; here: not installed. Works for me, too.
	rts
found:
	lda #0
	sta HaTabs,x
	sta HaTabs+1,x
	sta HaTabs+2,x		;hopefully the last one....
	rts
.endproc
;;; *** The handler entry points.
HandlerTable:
	.word FMS3Open-1
	.word FMS3Close-1
	.word FMS3Get-1
	.word FMS3Put-1
	.word FMS3Status-1
	.word FMS3Special-1
;;; *** Extract the file name from ZIOCB
;;; *** and place the expanded wild card into
;;; *** Pattern
.proc	ExtractFileName
	ldy #1
fndfile:
	lda (ZAdr),y
	cmp #':'
	beq foundsep
	iny
	cpy #3
	bne fndfile
nameerror:	
	Error FileNameInvalid
uniterror:
	Error IllegalUnit
foundsep:
	lda ZUnit
	cmp #9
	bcs uniterror
	ldx #8+3-1
	lda #' '
clrpat:	sta PatternBuffer,x
	dex
	bpl clrpat
	jsr ExtractPat
	cmp #$9b
	beq exit
	cmp #','
	bne nameerror
exit:	rts
.endproc
;;; *** Extract the Pattern from
;;; *** (ZAdr),y to PatBuf
;;; *** Return the first invalid character in A
.proc	ExtractPat
	iny
	ldx #0			;insert here
	lda #8			;end position
	jsr ExtractPatternPart
	cmp #'.'		;separated by a dot?
	bne exit		;if not, name ends here
	iny			;skip the dot
	ldx #8
	lda #11			;end position
ExtractPatternPart:
	sta EndPos		;last possible position
getlop:
	lda (ZAdr),y		;get part of the name.
	jsr isValid		;acceptable?
	bcs exit		;if not, name ends here.
	jsr Insert		;insert the character into the buffer
	iny
	bne getlop
exit:
	rts
;;; isValid
;;; Check whether the current character is valid
;;; returns C=0 if so, C=1 if not.
isValid:
	cmp #'A'
	bcc notalpha
	cmp #'Z'+1
	bcc exitok		;is valid
notalpha:
	cmp #'0'
	bcc notnumber
	cmp #'9'+1
	bcc exitok		;is also valid
notnumber:
	cmp #'?'
	beq exitok
	cmp #'*'
	beq exitok
	cmp #'-'
	beq exitok
	sec
	rts
exitok:
	clc
	rts
;;; Insert
;;; Insert a character into the pattern buffer.
Insert:
	cpx EndPos
	bcs full
	cmp #'*'		;this inserts all question marks
	beq fillq
	cmp #'-'
	beq fillall
	sta PatternBuffer,X
	inx
	clc
full:
	rts
fillq:
	lda #'?'
	jsr Insert
	bcc fillq
	rts
fillall:
	lda #'?'
filllop:
	cpx #11
	bcs exiti
	sta PatternBuffer,X
	inx
	bne filllop
exiti:
	rts
EndPos:		.byte 0
.endproc
;;; *** AllocateFCB
;;; *** Initialize the file control block
;;; *** which isn't much, because there is only one here...
.proc	AllocateFCB
	tsx
	inx
	inx
	stx Fms3Stack
	lda ZUnit
	sta SIODeviceUnit
	rts
.endproc
;;; *** ReadFAT
;;; *** Read the FAT from disk
.proc	ReadFAT
	lda #<FATBuffer
	sta SIOBufferLo
	lda #>FATBuffer
	sta SIOBufferHi
	ldx #$18		;the sector containing the FAT
	ldy #$00
	;; jsr ReadSector
	;; rts			;runs into the following
.endproc
;;; *** ReadSector
;;; *** Read the sector X,Y from disk
.proc	ReadSector
	stx SIOAux1
	sty SIOAux2
	lda #'R'		;read command
	sta SIOCommand
	jsr DiskInterfVector
	bmi fail
	rts
fail:
	lda SIOStatus
	jmp ErrorA
.endproc
;;; *** ReadFileSector
;;; *** Read the current sector from the file
.proc	ReadFileSector
	lda CurrentBlock
	ldx #0
	stx Tmp+1
	asl a
	rol Tmp+1
	asl a
	rol Tmp+1
	asl a
	rol Tmp+1		;*8
	sta Tmp

	lda FilePointer
	asl a
	lda FilePointer+1
	rol a
	and #7			;sector offset
	ora Tmp			;gives full sector
	clc
	adc #$19		;offset first block
	tax
	lda Tmp+1
	adc #0
	tay
	jmp ReadSector
Tmp:	.word 0
.endproc	
;;; *** LoadFileBuf
;;; *** Set the SIO buffer address to the file buffer
.proc	LoadFileBuf
	lda #<FileBuf
	sta SIOBufferLo
	lda #>FileBuf
	sta SIOBufferHi
	rts
.endproc
;;; *** PushError
;;; *** Generate an error code that is behind the return
;;; *** address, remove this return address, then exit
.proc	PushError
	pla
	sta FmsPtr
	pla
	sta FmsPtr+1
	ldy #1
	lda (FmsPtr),y
	;; runs into the following
	Skip2
.endproc
;;; *** ExitOK
;;; *** Leave with an OK error code
.proc	ExitOK
	lda #1
.endproc
;;; *** ErrorA
;;; *** Report the error code in the accumulator
.proc	ErrorA
	sta ZStatus
.endproc
;;; *** Leave
;;; *** Leave the FMS and return to the caller
;;; *** with the currently loaded status code
.proc	Leave
	ldx Fms3Stack
	txs
	lda ZIOByte
	ldx ZIOCB
	ldy ZStatus
	rts
.endproc
;;; *** CheckDiskType
;;; *** Check whether the disk is a valid DOS 3 disk
.proc	CheckDiskType
	lda #<FileBuf
	sta SIOBufferLo
	lda #>FileBuf
	sta SIOBufferHi
	ldx #$10		;is in sector 10
	ldy #$0
	sty FileIndex
	jsr ReadSector
	lda FileBuf+15
	cmp #$a5		;this is the Dos 3 ID
	bne invalid
	rts
invalid:
	Error NotADosDisk
.endproc
;;; *** LocateNextFile
;;; *** Find the next file from the current index on
.proc	LocateNextFile
	jsr LoadNextDirectory
	bcs notfound		;reached the end of the directory
	lda FileBuf,x
	bpl notfound		;bit 7 indicates whether the entry is valid or the last one
	rol a
	bpl LocateNextFile	;bit 6 indicates whether the entry is occupied
	and #$02
	bne LocateNextFile	;bit 0 indicates whether the entry is deleted
	;; now check whether the pattern matches
	ldy #0
matchlp:
	lda PatternBuffer,y
	cmp #'?'
	beq matches
	cmp FileBuf+1,x
	bne LocateNextFile
matches:
	iny
	inx
	cpy #11			;all characters?
	bcc matchlp
	jsr FileIndexToOffset
	clc
	rts
notfound:
	sec
	rts
;;; LoadNextDirectory:
;;; Read the next directory buffer into the file buffer.
LoadNextDirectory:
	inc FileIndex
	lda FileIndex
	and #7
	bne nonewdir		;on a sector boundary?
	lda FileIndex
	lsr a
	lsr a
	lsr a			;8 entries per directory
	clc
	adc #$10		;first directory entry at sector $10
	cmp #$18		;eight sectors in total
	bcs exit
	tax
	ldy #0
	jsr ReadSector
nonewdir:
	;; runs into the following
;;; FileIndexToOffset
;;; Computes from the file index the offset into the FileBuf
;;; in register X.
FileIndexToOffset:
	lda FileIndex
	asl a
	asl a
	asl a
	asl a			;each entry takes 16 bytes
	and #$7f		;each sector has 128 bytes
	tax			;caller expects this in X
	clc
exit:
	rts
.endproc
;;; *** FillDirectoryBuffer
;;; *** Fill the buffer of the current file
.proc	FillDirectoryBuffer
	lda #0
	sta DirOutputOffset
	jsr LocateNextFile
	bcs appendfreeblocks	;end of directory?
	lda FileBuf,x
	ldy #' '		;unprotected
	sty DirOutBuf+1
	sty DirOutBuf+13	;gap between name and block count
	cmp #$c0		;valid, occupied
	beq unlocked
	ldy #'*'		;protected
unlocked:
	sty DirOutBuf
	ldy #1
cpname:
	inx
	iny
	lda FileBuf,x
	sta DirOutBuf,y
	cpy #12
	bcc cpname
	lda #$9b
	sta DirOutBuf+17	;end there
	lda #0
	sta DirOutBuf+18
	lda FileBuf+1,x		;length in blocks
	iny
	iny
	jsr toDecimal		;insert the length in blocks in decimal
	rts
appendfreeblocks:
	;; end of directory found, append the free block count
	ldx #$ff
cpfree:
	inx
	lda FreeBlocksMsg,x
	sta DirOutBuf+3,x
	bne cpfree
	;; compute now the number of free blocks
	ldy #$ff		;block offset
	ldx #0			;block counter
fatcnt:
	iny
	bmi donecnt
	lda FATBuffer,y
	cmp #$fe		;free?
	bne fatcnt
	inx
	bne fatcnt
donecnt:	
	ldy #0
	txa
;;; insert the value in A as decimal string into the DirOutBuf+y
toDecimal:
	ldx #100
	jsr DivideBy
	ora #'0'
	sta DirOutBuf,y
	txa			;remainder->A
	ldx #10
	jsr DivideBy
	ora #'0'
	sta DirOutBuf+1,y
	txa
	ora #'0'
	sta DirOutBuf+2,y
	rts
;;; DivideBy
;;; Divide A by X, do not touch Y, return the quotient in A, the remainder in X
DivideBy:
	stx Tmp
	ldx #0
divloop:
	cmp Tmp
	bcc done
	sbc Tmp
	inx
	bne divloop
done:
	;; quotient is now in X, remainder in A.
	sta Tmp
	txa
	ldx Tmp
	rts
Tmp:		.byte 0
FreeBlocksMsg:	.byte " FREE BLOCKS",155,0
.endproc
;;; *** AdvanceBlockOnBoundary
;;; *** Check whether the file pointer sits at a block
;;; *** boundary. If so, advance the block by one.
.proc	AdvanceBlockOnBoundary
	lda FilePointer
	bne noboundary
	lda FilePointer+1
	and #3
	bne noboundary
	
	jsr AdvanceBlock
noboundary:
	rts
.endproc
;;; *** AdvanceBlock
;;; *** Advance the block by one, go to the next block
.proc	AdvanceBlock
	ldy CurrentBlock
	lda FATBuffer,y
	bmi linkerror
	sta CurrentBlock
exit:	rts
linkerror:
	Error BadLinkage	;file size and size in FAT do not match
.endproc
;;; *** IncrementFilePointer
;;; *** Increment the file pointer by A bytes, adjust CIO data
;;; *** accordingly to include burst support.
.proc	IncrementFilePointer
	sta Tmp
	clc
	adc FilePointer
	sta FilePointer
	bcc noinc
	inc FilePointer+1
	bne noinc
	inc FilePointer+2
noinc:
	;; If we read N bytes at once, bypassing CIO in
	;; the block mode, advance the CIO variables by N-1
	dec Tmp
	sec
	lda ZLen
	sbc Tmp
	sta ZLen
	bcs nodec
	dec ZLen+1
nodec:
	clc
	lda ZAdr
	adc Tmp
	sta ZAdr
	bcc noinc2
	inc ZAdr+1
noinc2:
	rts
Tmp:	.byte 0
.endproc
;;; *** GetByte
;;; *** Read a single byte from a file
.proc	GetByte
	lda FilePointer
	and #$7f
	bne noboundary
	jsr LoadFileBuf
	jsr ReadFileSector
	lda #0
noboundary:	
	tay
	lda FileBuf,y
	sta ZIOByte
	lda #1
	jsr IncrementFilePointer
	jsr AdvanceBlockOnBoundary
	rts
.endproc
;;; *** BurstGet
;;; *** Read an entire sector by bursting
;;; *** This happens only if we are at a sector
;;; *** boundary and at least an entire sector
;;; *** remains in the file.
.proc	BurstGet
	lda ZAdr
	sta SIOBufferLo
	lda ZAdr+1
	sta SIOBufferHi
	jsr ReadFileSector
	;; place the last byte in the ZByte so CIO
	;; gets what it expects
	ldy #$7f
	lda (ZAdr),y
	sta ZIOByte
	lda #128
	jsr IncrementFilePointer
	jsr AdvanceBlockOnBoundary
	rts
.endproc
;;; *** FMS3Open
;;; *** The FMS3 Open vector
.proc	FMS3Open
	jsr AllocateFCB
	jsr ExtractFileName
	lda ZAux1
        and #$8                 ;write access is not permitted
        beq isfine
        Error FileProtected
isfine:
	jsr ReadFAT
	jsr CheckDiskType
	lda ZAux1		;only reading and directory access is supported here
	cmp #4
	beq openforread
	cmp #6
	beq openfordirectory
	Error InvalidMode
openforread:
	jsr LocateNextFile
	bcc found
	Error FileNotFound
openfordirectory:
	jsr FillDirectoryBuffer
	jmp ExitOK
found:
	ldy #0
getstats:	
	lda FileBuf+12,x
	sta BlockLength,y
	inx
	iny
	cpy #4
	bcc getstats
	
	lda BlockLength
nonzero:			;compute the upper two bits of the file size
	rol a
	rol a
	rol a
	and #3			;in upper two bits
	sta FileSize+2
	;; position at the start of the file
	lda #0
	sta FilePointer
	sta FilePointer+1
	sta FilePointer+2
	lda StartBlock
	sta CurrentBlock
	;; reset the sector offset within the block
	jmp ExitOK
.endproc
;;; *** FMS3Close
;;; *** Close the channel again
.proc	FMS3Close
	ldy #1
	rts
.endproc
;;; *** CheckEOF
;;; *** Test whether the file pointer points at or behind the EOF
;;; *** Returns C=1 if the file pointer sits at or behind the EOF
.proc	CheckEOF
	sec
	lda FilePointer
	sbc FileSize
	sta RemainingBytes
	lda FilePointer+1
	sbc FileSize+1
	sta RemainingBytes+1
	lda FilePointer+2
	sbc FileSize+2
	sta RemainingBytes+2
	rts
.endproc
;;; *** FMS3Get
;;; *** Get a byte from the file and return it
.proc	FMS3Get
	jsr AllocateFCB
	lda ZAux1
	and #2
	bne readdir
	jsr CheckEOF
	bcs erroreof
	;; bursting possible?
	lda ZCmd
	cmp #CmdGetBlock
	bne noburst
	;; at sector boundary?
	lda FilePointer
	and #$7f
	bne noburst
	;; at least a sector remaining?
	lda RemainingBytes+1
	and RemainingBytes+2
	cmp #$ff
	bne mayburst
	lda RemainingBytes
	cmp #127
	bcs noburst
mayburst:
	lda ZLen+1
	bne burst
	lda ZLen
	bmi burst
noburst:
	jsr GetByte
done:
	jsr CheckEOF
	bcc exit
	lda #3			;EOF warning
	jmp ErrorA
exit:	jmp ExitOK
burst:
	jsr BurstGet
	jmp done
erroreof:
	Error EndOfFile
readdir:
	ldy DirOutputOffset
	inc DirOutputOffset
	lda DirOutBuf,y
	beq refill
	sta ZIOByte
	jmp ExitOK
refill:
	cpy #18			;regular entry?
	bcc erroreof
	jsr FillDirectoryBuffer
	jmp readdir
.endproc
;;; *** FMS3Put
;;; *** Put a byte to FMS3 - not supported
.proc	FMS3Put
	ldy #UnsupportedCmd
	rts
.endproc
;;; *** FMS3Status
;;; *** Return the status
.proc	FMS3Status
	;; actually, Dos 3 would here try to validate the file name.
	lda IOCBStatus,x
	jmp ErrorA
.endproc
;;; *** FMS3Special
;;; *** Implements POINT and NOTE required for
;;; *** copy. Nothing else is implemented here.
.proc	FMS3Special
	jsr AllocateFCB
	lda ZCmd
	cmp #CmdPoint
	beq FMS3Point
	cmp #CmdNote
	beq FMS3Note
	Error UnsupportedCmd
.endproc
;;; *** FMS3Note
;;; *** Implements the NOTE command
;;; *** The file pointer is just copied
;;; *** to AUX.
.proc	FMS3Note
	ldx ZIOCB
	lda IOCBIndex,x
	bmi closed
	lda FilePointer+2
	sta IOCBAux5,x
	lda FilePointer
	sta IOCBAux3,x
	lda FilePointer+1
	sta IOCBAux4,x
	jmp ExitOK
closed:
	Error ChannelNotOpen
.endproc
;;; *** FMS3Point
;;; *** Set the file pointer to a specific location
.proc	FMS3Point
	ldx ZIOCB
	lda IOCBIndex,x
	bmi closed
	lda IOCBAux5,x
	sta FilePointer+2
	lda IOCBAux3,x
	sta FilePointer
	lda IOCBAux4,x
	sta FilePointer+1
	jsr CheckEOF
	bcc setposition
	lda RemainingBytes
	ora RemainingBytes+1
	ora RemainingBytes+2
	beq setposition
	Error EndOfFile
setposition:
	lda StartBlock
	sta CurrentBlock
	;; Compute the number of blocks to wind forward
	lda FilePointer+1
	sta Tmp
	lda FilePointer+2
	;; must divide this by four
	;; eight sectors per block
	lsr a
	ror Tmp
	lsr a
	ror Tmp
	ldx Tmp
	beq done
skipblocks:
	jsr AdvanceBlock
	dex
	bne skipblocks
done:
	jmp ExitOK
closed:
	Error ChannelNotOpen
Tmp:	.byte 0
.endproc
;;; *** Globals
Fms3Stack:		.byte	0	;keeps the stack pointer for error handling
CurrentBlock:		.byte	0	;block number of the block at the file position
FileIndex:		.byte	0	;absolute offset into the directory.
DirOutputOffset:	.byte 	0	;current pointer into the directory output buffer
BlockLength:		.byte	0	;Size of the file in blocks
StartBlock:		.byte	0	;First block of the file
FileSize:		.byte	0,0,0	;Size of the file in bytes
FilePointer:		.byte	0,0,0	;current position of the file pointer in the file
RemainingBytes:		.byte	0,0,0	;negative of the number of bytes before the EOF
PatternBuffer:		.res	11	;keeps the pattern for file name matching
DirOutBuf:		.res	19	;output buffer for the directory reader
FATBuffer:		.res	128	;keeps the FAT
FileBuf:		.res	128	;keeps the file sector or directory sector
