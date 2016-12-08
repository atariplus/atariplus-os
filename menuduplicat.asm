;;; **********************************************************************
;;; ** Thor Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: menuduplicat.asm,v 1.10 2015/11/08 15:26:39 thor Exp $       **
;;; **                                                                  **
;;; ** In this module:   Duplicate file					**
;;; **********************************************************************

        .include "menujmptab.i"
	.include "reset.i"
	.include "fms.i"
	.segment "menufunction"

CurrentEntry	=	$d6	;the current entry that is worked on
FileList	=	$f0	;list of files to copy
BufferStart	=	$f2	;where the file buffer starts
Flag		=	$f4	;bit 7=0: ask for disk changes, otherwise do not. bit 6=1: do not ask for confirmation, bit 0=1: always ask for disk changes

;;; Flags usage in the directory list
IsOpenR		=	$02	;IOCB for this file is already open for reading, do not POINT
IsOpenW		=	$04	;IOCB for this file is already open for writing, do not POINT
IsComplete	=	$08	;all data for this file has been read.
IsRead		=	$10	;data for this file has been read.
IsPartial	=	$20	;file is partially written already
IsSkipped	=	$40	;do not work on this
IsDone		=	$80	;already completely done

.macro  Skip2
        .byte $2c
.endmacro
.macro  Skip1
        .byte $24
.endmacro

	
;;; Init Vector
	rts
;;; Run vector
	jmp Duplicate
;;; *** RequestDisk
;;; *** Ask the user to insert the source or the destination.
;;; *** if C=0, request for the source, otherwise request the destination.
;;; *** if bit 7 of the flag is set, do not ask because source and target are
;;; *** different. Returns C=0 if the user wants to continue, otherwise C=1.
.proc	RequestDisk
	bcc source
	jsr SetRedColor
	bit Flag
	bmi exit
	ldx #<InsertDest
	ldy #>InsertDest
	lda #InsertDestL
	bne print
source:
	jsr SetGreenColor
	bit Flag
	bmi exit
	ldx #<InsertSource
	ldy #>InsertSource
	lda #InsertSourceL
print:
	jsr Print
	jsr EnableBREAK
	jsr GetKey
	cmp #$9b
	php
	jsr DisableBREAK
	plp
	beq exit
	sec
	rts
exit:
	clc
	rts
InsertSource:	.byte 155,"Insert "
		.byte 's'+128,'o'+128,'u'+128,'r'+128,'c'+128,'e'+128
		.byte "      and press "
		.byte 'R'+128,'E'+128,'T'+128,'U'+128,'R'+128,'N'+128
		.byte 155
InsertSourceL	=	*-InsertSource
InsertDest:	.byte 155,"Insert "
		.byte 'd'+128,'e'+128,'s'+128,'t'+128,'i'+128,'n'+128,'a'+128,'t'+128,'i'+128,'o'+128,'n'+128
		.byte " and press "
		.byte 'R'+128,'E'+128,'T'+128,'U'+128,'R'+128,'N'+128
		.byte 155
InsertDestL	=	*-InsertDest
.endproc
;;; *** CheckAccept
;;; *** Ask if the user wants to copy the file in Buf1. If C = 0,
;;; *** then skip this file. If bit 6 of flags is set, then
;;; *** do not check for permission.
;;; *** Source is in buffer 1, destination in buffer 3.
.proc	CheckAccept
	bit Flag
	bvs exit
	ldx #<OkToCopy
	ldy #>OkToCopy
	lda #OkToCopyL
	jsr Print
	jsr LoadBuf1Ptr
	jsr PrintRecord
	ldx #<OkToCopyDest
	ldy #>OkToCopyDest
	lda #OkToCopyDestL
	jsr Print
	jsr LoadBuf3Ptr
	jsr PrintRecord

	jsr EnableBREAK
	jsr YesNo
	php
	jsr DisableBREAK
	plp
	rts
exit:
	sec
	rts
OkToCopy:	.byte 155,"Type ",34,"Y",34," to copy "
OkToCopyL	=	*-OkToCopy
OkToCopyDest:	.byte "to "
OkToCopyDestL	=	*-OkToCopyDest
.endproc
;;; *** CreateDestFilespec
;;; *** Build the destination file specification
;;; *** from the user supplied filespec and/or device,
;;; *** and the source. The target is in Buf2, the source in Buf1.
.proc	CreateDestFilespec
	ldy #3
	ldx #2
	jsr CopyBuffer2Buffer	;copy target to buffer3
	lda Flag
	and #$7f
	sta Flag		;ask user to swap disks.
	;; copy the current default device
	ldx #<DefaultBuffer
	ldy #>DefaultBuffer
	jsr GetDefaultDevice
	;; read the default device from the source
	jsr LoadBuf1Ptr
	jsr SetDefaultDevice	;may fail, default remains unchanged then (which is what we want)
	jsr LoadBuf3Ptr
	jsr AddDevice		;may or may not do anything
	bcs nodevice		;keep asking if no device was present
	lda Flag
	lsr a
	bcs nodevice		;keep asking
	asl a
	ora #$80		;disable requesting disk change
	sta Flag
nodevice:
	ldx #<DefaultBuffer
	ldy #>DefaultBuffer	;restore the old default device
	jsr SetDefaultDevice
	;; now check whether a target device name is present.
	jsr LoadBuf3Ptr
	jsr SetZPtr
	;; at least a device name is present now.
	ldy #2
	;; position 2 is either the : for three-letter devices, or the first
	;; character of the filename, or an EOL
	lda (ZPtr),y
	cmp #$9b
	beq nofilespec
	cmp #':'
	bne havefilename	;
	;; here three letter device
	iny
	lda (ZPtr),y
	cmp #$9b		;end here?
	bne havefilename	;ok filename is present
nofilespec:
	sty Z3Ptr+1		;destination pointer
	;; scan for source file name now.
	jsr LoadBuf1Ptr		;get source file name
	stx Z2Ptr
	sty Z2Ptr+1		;source pointer
	ldy #1
	lda #':'
	cmp (Z2Ptr),y
	beq foundcolon
	iny
	cmp (Z2Ptr),y
	beq foundcolon
	ldy #$ff
foundcolon:
	iny
	;; y points now to the file name in the source buffer
	sty Z3Ptr
cpfilenam:
	ldy Z3Ptr
	lda (Z2Ptr),y
	inc Z3Ptr
	ldy Z3Ptr+1
	sta (ZPtr),y
	inc Z3Ptr+1
	cmp #$9B
	bne cpfilenam
havefilename:
	rts
DefaultBuffer:	.byte 0,0,0,0	;buffer for the default device name.
.endproc
;;; *** ResetBuffers
;;; *** Restore AppMemHi to the start of the available memor
.proc	ResetBuffers
	lda BufferStart
	sta AppMemHi
	lda BufferStart+1
	sta AppMemHi+1
	ldx FileList
	ldy FileList+1
	stx CurrentEntry
	sty CurrentEntry+1
	rts
.endproc
;;; *** ReserveQuiet
;;; *** Increment AppMemHi without causing a warning if the buffer reaches zero.
.proc	ReserveQuiet
	clc
	txa
	adc AppMemHi
	sta AppMemHi
	tya
	adc AppMemHi+1
	sta AppMemHi+1
	rts
.endproc
;;; *** AvailMem
;;; *** Return the number of bytes available in X,Y, return Z=1 if nothing is left.
.proc	AvailMem
	lda MemTop
	sec
	sbc AppMemHi
	tax
	sta Z3Ptr
	lda MemTop+1
	sbc AppMemHi+1
	bcc error
	tay
	ora Z3Ptr
	rts
error:
	lda #0
	tax
	tay
	rts
.endproc
;;; *** GetCurrentDirEntry
;;; *** Read the current directory entry from CurrentEntry to CurrentEntry
;;; *** copy it into buffer1, the destination name into buffer2, set C=1
;;; *** if all data is done.
.proc	GetCurrentDirEntry
	ldx CurrentEntry
	ldy CurrentEntry+1
	lda DefaultDevice+1
	jsr GetDirectoryEntry
	bcs exit		;exit if nothing else to do
	stx CurrentEntry
	sty CurrentEntry+1
	ldx #0
	ldy #1
	jsr CopyBuffer2Buffer	;copy source to buffer #1
	jsr CreateDestFilespec	;create the destionation in buffer #3 now
	clc
exit:
	rts
.endproc
;;; *** ReadSources
;;; *** Fill the available sources with as much data as we can.
;;; *** if C=1, do not ask the user to insert the source, otherwise do
.proc	ReadSources
	jsr ResetBuffers
	;; use IOCB #1 for the source
	ldx #$10
	jsr SetIOCB
readcontinue:
	php
	jsr AvailMem
	bne havemem
done:	
	plp
	clc			;abort here for this loop, but write what we have.
abort:	
	rts
havemem:
	jsr GetCurrentDirEntry
	bcs done		;all done here.
	plp			;is already inserted?
	bcs donotask
	jsr RequestDisk		;ask the user to insert the source
	bcs abort
donotask:
	ldy #0
	lda (CurrentEntry),y	;is the current entry already worked on?
	and #IsRead		;was already partially written.
	bne isworkedon
	jsr CheckAccept
	bcs isworkedon
	lda #IsSkipped|IsRead	;set the skip flag
	bne nextentry
isworkedon:
	jsr PrintMsg
	
	jsr OpenFileForReading

	jsr AvailMem
	jsr SetLength
	ldx AppMemHi
	ldy AppMemHi+1
	jsr BGet		;fill as much as we can
	jsr CheckIOError
	tya
	eor #3
	;; if C=1 fit into memory, makes sense to try something next
	;; if Z=1 or C=1: File is complete.
	php
	;; keep the length
	jsr GetLength
	jsr SetBufferedLength
	jsr GetLength
	jsr ReserveQuiet	;keep in memory, advance AppMem
	;; check whether the file did fit
	plp
	beq isdone		;file is done, just fit right in.
	bcc incomplete		;some data left.
	;; file has been read completely (EOF hit) and there is memory left.
	jsr Close
	jsr CheckIOError
	lda #IsRead|IsComplete
nextentry:
	jsr setflags
	ldx #CurrentEntry
	jsr NextDirectoryEntry
	sec			;do not ask to insert the source anymore
	bcs readcontinue	;continue with the next file.
incomplete:	
	;; here: file is not complete. Try to NOTE the file position
	lda #CmdNote
	jsr XIO
	bmi nonote
	;; here: Note worked!
	jsr Close
	jsr CheckIOError
	lda #IsRead
	Skip2
nonote:
	;; note did not work. Leave the channel open.
	lda #IsOpenR|IsRead
	clc
	bcc setflags
isdone:
	jsr Close
	jsr CheckIOError
	;; file has been read completely, and no buffer memory is left.	
	lda #IsRead|IsComplete
setflags:	
	ldy #0
	eor (CurrentEntry),y
	and #(~(IsOpenW|IsPartial|IsDone) & 255) ;preserve these bits
	eor (CurrentEntry),y
	sta (CurrentEntry),y
	clc
	rts
;;; Open the file for reading, potentially set the file pointer,
;;; or skip opening if it is already open.
OpenFileForReading:
	ldy #0
	lda (CurrentEntry),y
	and #IsOpenR
	bne exit
	jsr LoadBuf1Ptr
	lda #4
	jsr Open		;re-open
	jsr CheckIOError
	ldy #0
	lda (CurrentEntry),y
	and #IsPartial
	beq exit
	;; set the file pointer
	lda #CmdPoint
	jsr XIO			;still in Aux3,4,5
	jsr CheckIOError
exit:
	rts
PrintMsg:
	ldx #<ReadingMsg
	ldy #>ReadingMsg
	lda #ReadingMsgL
	jsr Print
	jsr LoadBuf1Ptr
	jsr PrintRecord
	rts
ReadingMsg:	.byte "Reading "
ReadingMsgL	=	*-ReadingMsg
.endproc
;;; *** WriteTargets
;;; *** Write targets back
.proc	WriteTargets
	jsr ResetBuffers
	;; Use IOCB #2 for the target.
	ldx #$20
	jsr SetIOCB
	sec			;request that the user inserts the target
writeloop:
	php
	jsr GetCurrentDirEntry
	bcc haveentry
allwritten:
	pla			;do not touch C flag.
abort:
	rts			;all entries done, abort writing.
haveentry:
	ldy #0
	lda (CurrentEntry),y
	beq allwritten		;nothing else to do, reached the last entry read.
	plp			;already restore the carry.
	and #IsSkipped
	bne skipfile		;nothing to do either
	bcc donotask		;already have the target filespec
	jsr RequestDisk		;ask the user to insert the destination
	bcs abort		;user aborted
donotask:
	jsr PrintMsg
	jsr OpenFileForWriting
	jsr GetBufferedLength
	jsr SetLength
	ldx AppMemHi
	ldy AppMemHi+1
	jsr BPut
	jsr CheckIOError
	;; Update the pointer to the next file.
	jsr GetBufferedLength
	jsr ReserveQuiet	;implements the increment
	;; Now test whether we need to continue writing later
	ldy #0
	lda (CurrentEntry),y
	and #IsComplete
	bne completed		;yup
	;; here not. Check whether we can NOTE and thus close the stream.
	lda #CmdNote
	jsr XIO
	bmi keepopen		;nope
	;; here: NOTE worked.
	lda #IsPartial
	Skip2
completed:
skipfile:
	lda #IsDone
	pha
	jsr Close
	jsr CheckIOError
	pla
	Skip2
keepopen:
	lda #IsPartial|IsOpenW
	ldy #0
	ora (CurrentEntry),y
	sta (CurrentEntry),y
	bmi donecontinue
	clc
	and #IsPartial		;if partial and not done, this is also the last entry.
	bne abort
donecontinue:	
	ldx #CurrentEntry
	jsr NextDirectoryEntry
	bcc writeloop
	rts
;;; Open the file for writing, potentially seek to the end
;;; or skip the process if the file is already open
OpenFileForWriting:
	ldy #0
	lda (CurrentEntry),y
	and #IsPartial
	beq opennew
	;; here: the file is already partially written
	lda (CurrentEntry),y
	and #IsOpenW
	bne exit		;skip the open drill, is already there.
	jsr LoadBuf3Ptr
	lda #13			;read,write and append. Append for us.
	jsr Open
	jsr CheckIOError
	lda #CmdPoint
	jsr XIO			;point to the continuation position
	jsr CheckIOError
	rts
	;; The file must be opened anew
opennew:
	jsr LoadBuf3Ptr
	lda #8			;for writing only
	jsr Open
	jsr CheckIOError
exit:
	rts
PrintMsg:	
	ldx #<WritingMsg
	ldy #>WritingMsg
	lda #WritingMsgL
	jsr Print
	jsr LoadBuf3Ptr
	jsr PrintRecord
	rts
WritingMsg:	.byte "Writing "
WritingMsgL	=	*-WritingMsg
.endproc
;;; *** Duplicate
;;; *** Copy files from one device to another. Or to the same.
.proc	Duplicate
	ldx #<DuplicateReq
	ldy #>DuplicateReq
	lda #45
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	jsr PrintEOL
	jsr DisableBREAK
	;; parse off source
	jsr LoadBuf1Ptr
	jsr ReadInputParameter
	beq exit
	;; parse off destination.
	;; might be empty.
	jsr LoadBuf2Ptr
	jsr ReadInputParameter
	;; clear the flag(s)
	lda #0
	sta Flag
	;; check whether we should ask for each file.
	jsr LoadBuf1Ptr
	lda #'N'
	jsr GetArgumentExtension
	bcc doask
	lda #$40		;disable the confirmation
	jsr OrToFlag
doask:
	jsr LoadBuf1Ptr
	lda #'I'
	jsr GetArgumentExtension
	bcc doask2
	lda #$01
	jsr OrToFlag
doask2:	
	jsr LoadBuf2Ptr
	lda #'N'
	jsr GetArgumentExtension
	bcc doask3
	lda #$40
	jsr OrToFlag
doask3:
	jsr LoadBuf2Ptr
	lda #'I'
	jsr GetArgumentExtension
	bcc doask4
	lda #$01
	jsr OrToFlag
doask4:	
	jsr SetGreenColor
	jsr LoadBuf1Ptr
	jsr GetDirectoryList
	stx FileList
	sty FileList+1
	lda AppMemHi
	sta BufferStart
	lda AppMemHi+1
	sta BufferStart+1
	;; here start of the main copy loop
	ldx #<125
	ldy #>125    		;one sector
	jsr Reserve		;ensure that at least a bit of memory is there...
	jsr Dispose
	sec			;do not ask now
copyloop:
	jsr ReadSources
	bcs exit
	jsr WriteTargets
	bcc copyloop
exit:
	rts
OrToFlag:
	ora Flag
	sta Flag
	rts
DuplicateReq:	.byte "Duplicate - give from(,to) :",155
.endproc
	
