;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: taperesident.asm,v 1.17 2013/06/05 19:11:21 thor Exp $	**
;;; **									**
;;; ** In this module:	 Resident part of the disk-based tape handler	**
;;; **********************************************************************
	
	.include 	"sio.i"
	.include	"cio.i"
	.include	"irq.i"
	.include	"nmi.i"
	.include	"pokey.i"
	.include	"antic.i"
	.include	"pia.i"
	.include	"gtia.i"
	.include	"reset.i"
	.include	"kernel.i"
	.include	"errors.i"
	.include	"fms.i"
	.include	"taperesident.i"
	.segment	"taperesident"

;;; *** Skip two bytes (by a dummy BIT)
.macro  Skip2
        .byte $2c
.endmacro
.macro  Skip1
        .byte $24
.endmacro
	
;;; *** Initial service JumpTab
TapeResidentStart	=	*
TapeLoPtr:	.word 0		;will point back to the low part of the handler
MemLoAdj:	.word 0		;will be the new MemLo
	jmp TapeOpen
	jmp TapeClose
	jmp TapeGet
	jmp TapePut
	jmp TapeStatus
	jmp TapeSpecial
	jmp TapeReset		;called on reset
	jmp TapeInitialize	;called the first type
	jmp TapeInit
	jmp SIOTape

;;; *** HatabsCopy
;;; *** This is a copy of Hatabs
;;; *** which is installed for booting
HatabsCopy:
	.word	HatabsCopy+2	;pointer to the table
	.word	TapeOpen-1
	.word	TapeClose-1
	.word	TapeGet-1
	.word	TapePut-1
	.word	TapeStatus-1
	.word	TapeSpecial-1
	
;;; *** WriteTapeRecord
;;; *** Write a record of type A to the tape
.proc	WriteTapeRecord
	sta TapeBuffer-1
	lda #$55		;the sync marker
	sta TapeBuffer-2
	sta TapeBuffer-3
	sec
	;; runs into the following
.endproc
;;; *** TapeAccess
;;; *** Access the tape through the top-level SIO emulation
;;; *** if C=1, then write access, otherwise a read access.
.proc	TapeAccess
	lda #$40
	ldx #'R'
	bcc isread
	lda #$80
	ldx #'W'
isread:
	sta SIOStatus
	stx SIOCommand		;actually, neither needed.
	lda #$60
	sta SIODeviceId		;actually, not needed.
	lda #0
	sta SIODeviceUnit	;neither. There is only one, anyhow...

	lda #<(TapeBuffer-3)
	sta SIOBufferLo
	lda #>(TapeBuffer-3)
	sta SIOBufferHi

	clc
	lda RecordSize
	adc #3
	sta SIOSizeLo
	lda RecordSize
	bne keep
	sec
keep:	
	lda #0
	adc #0
	sta SIOSizeHi

	lda #23
	sta SIOTimeout		;set timeout
	
	lda GapType
	sta SIOAux2
	;; runs into the following
.endproc
;;; *** SIOTape
;;; *** This is the SIO layer of the tape device.
;;; *** It does actually not use much of the resident SIO.
.proc	SIOTape
	lda #$01
	sta CriticIO		; critical IO starts here
	sta SerialStatus	; ok so far
	tsx			; keep stack pointer for direct return
	stx SIOStackPtr
	bit SIOStatus		;receive or send?
	bpl receive
	;; sending
	jsr SendTapeBytes
	clc
	bcc exit
receive:
	;; receiving
	jsr ReceiveTapeBytes
exit:
	jsr Cleanup
	sta CriticIO
	jsr StopSIOTimer

	ldy SerialStatus
	bmi motoroff
	bit SIOAux2		;short block mode?
	bmi shortmode
motoroff:	
	lda #$3c
	sta PIAPortACtrl	;turn off the motor on errors
shortmode:	
	sty SIOStatus
	cpy #$00
	rts
.endproc
;;; *** StopSIOTimer
;;; *** Stop running the SIO timer, ensure
;;; *** we are not called again
.proc	StopSIOTimer
	ldx #0
	ldy #0
	;; runs into the following
.endproc
;;; *** SetSIOTimeout
;;; *** Start the SIO timeout timer with the value X,Y
;;; *** with HI in Y
.proc	SetSIOTimeout
	lda #1
	jsr SetIRQVector	;set the VBI timer count
	
	lda #8
	ldx Timer0CallIndirect+1
	ldy Timer0CallIndirect
	jsr SetIRQVector	;set the timer 0 vector

	lda #1
	sta SIOTimerFlag
	rts
Timer0Call:
	lda #0	
	sta SIOTimerFlag	;indicates the timeout.
	rts
Timer0CallIndirect:	.word Timer0Call
.endproc
;;; *** WaitShortGap
;;; *** Wait for Y VBIs before proceeding
;;; *** Hi is set to zero.
.proc	WaitShortGap
	lda #0
	;; runs into the following
.endproc
;;; *** WaitGap
;;; *** Initialize the timer to wait for Y,A VBIs,
;;; *** then set the timer flag, wait for the
;;; *** long or short gap to pass. Returns with Z=0
;;; *** if aborted by break.
.proc	WaitGap
	tax
	jsr SetSIOTimeout
	
	lda #$34
	sta PIAPortACtrl	;turn on the motor
wait:
	lda BreakFlag
	beq abort
	
	lda SIOTimerFlag
	bne wait
abort:
	lda BreakFlag
	rts
.endproc
;;; *** ComputeBaudRate
;;; *** Compute the baud rate from the measured time difference
;;; *** of BaudCnt and BaudCnt+2
;;; *** returns with C=1 if the rate could not be computed.
.proc	ComputeBaudRate
	ldx PalNTSCShadow

	;; adjust the line count so that line zero is at the VBI
	;; position.
	lda BaudCnt
	jsr Normalize
	sta BaudCnt
	lda BaudCnt+2
	jsr Normalize
	sta BaudCnt+2

	;; compute the high-difference, convert to double-lines
	sec
	lda BaudCnt+3
	sbc BaudCnt+1
	beq nolines		;again modulo arithmetic, may underflow
	cmp #2			;must be at most 1
	bcs error
	;; one frame in between
	lda LinesPerFrame,x
nolines:
	clc
	adc BaudCnt+2
	sec
	sbc BaudCnt		;add low-lines difference
	tay			;keep the line difference

	;; compute, approximately,
	;; lines * 91/16. The correct
	;; computation would be
	;; lines * 114/20, but 114/20 is approximately
	;; 91/16.
	lsr a
	lsr a
	lsr a			;/8. Now between 0..31
	asl a			;entries in the table are words
	sec
	sbc #22			;minimum value (hopefully) 
	bcc error
	tax			;table entry for multiplying with 91.2 = 114/20
	
	tya
	and #7			;remaining three lower bits
	tay
	lda Times11,y		;multiply by the remainder, 91.2/8
	
	clc
	adc BaudAdjust,x	;this is the times 91.2 table
	sta BaudRate
	lda #0
	adc BaudAdjust+1,x
	sta BaudRate+1		;hopefully C=0 now (or error)
	rts
error:
	sec
	rts
Normalize:
	cmp #$7c		;the VBI position?
	bcc novbi
	sbc #$7c		;wrap-around, the hi would have been incremented
	rts
novbi:
	adc VBISize,x		;number of lines in the VBlank
	rts
VBISize:	.byte 7,32	;double-lines taken by the VBI
LinesPerFrame:	.byte 131,156	;double-lines per frame in a NTSC and PAL frame
Times11:	.byte 0,11,23,34,45,57,68,80 ;multiply-by-11 table (actually, 91/8)
	;; this table contains the baud rates, indexed by (2 * line-difference) / 8
	;; the -7 is the carry over time pokey needs.
BaudAdjust:
	.word 1000-7,1091-7,1182-7,1273-7,1364-7,1455-7,1546-7,1637-7,1728-7,1818-7
.endproc
;;; *** ReceiveTapeBytes
;;; *** Receive data from the tape
.proc	ReceiveTapeBytes
	jsr ComputeSIOBuffer

	ldx PalNTSCShadow	;one for PAL
	
	ldy ShortGapLength,x
	bit SIOAux2		;short or long inter-record gab?
	bmi isshort
	ldy LongGapLength,x
isshort:
	jsr WaitShortGap
	beq breakabort

	jsr ComputeTimeout
	jsr SetSIOTimeout

	;; measure the tape speed
	lda #$10
	sta SerialInBit
	sei
	
	ldx #1
	jsr WaitForSerialLineChange
	bmi error
	;; get the position, as good as we can
	ldx YPos
	ldy Clock
	stx BaudCnt
	sty BaudCnt+1

	ldx #$0a		;measure ten transitions
	jsr WaitForSerialLineChange
	bmi error
	;; we are now at the start bit of the second byte.
	
	;; compute the time difference
	ldx YPos
	ldy Clock
	stx BaudCnt+2
	sty BaudCnt+3
	;; in cycles, and from that the baud rate.
	jsr ComputeBaudRate
	bcs bauderror

	ldx #$09		;skip the remaining nine
	jsr WaitForSerialLineChange
	bmi error
	;; now at the stop bit of the second byte
	
	;; start pokey now immediately ASAP
	jsr AudioInit
	
	ldy #0
	sty SkStat		;reset the serial port
	lda SkStatShadow
        and #$7
        ora #$10
        sta SkStatShadow        ;ready to receive
        sty SkReset
        sta SkStat
	sty SerialDataDone
	sty SerialXferDone	;checksum will be filled later...

	;; fill in the missing bytes manually
	jsr FillSyncMarkers
	cli			;off we go
	lda #$20		;enable the receive IRQ
	jsr AbleIRQ
wt:
	lda BreakFlag
	beq breakabort
	lda SIOTimerFlag
	beq timeout
	lda SerialXferDone
	beq wt			;until all done
	rts
breakabort:
	dec BreakFlag
	ldy #BreakError
	Skip2
timeout:
	ldy #TimeoutError
	Skip2
bauderror:
	ldy #DeviceError
error:
	sty SerialStatus
	rts
ShortGapLength:		.byte 10,8 	;a bit shorter on receiving
LongGapLength:		.byte 120,100 	;
.endproc
;;; *** FillSyncMarkers
;;; *** Fiddle in the sync markers we received manually
.proc	FillSyncMarkers
	ldy #0
	lda #$55
	sta (SerBufLo),y
	iny
	sta (SerBufLo),y
	iny
	lda #$aa
	sta SerialChkSum	;and this would be their checksum
	;; increment the buffer pointer
	tya
	clc
	adc SerBufLo
	sta SerBufLo
	bcc noinc
	inc SerBufHi
noinc:
	rts
.endproc
;;; *** AudioInit
;;; *** Initialize the audio channels for receiving or
;;; *** transmitting the tape data
.proc	AudioInit
	lda #$28		;two-tone mode, channel-coupling
	sta AudioCtrl
	
	lda BaudRate
	sta AudFreq2
	lda BaudRate+1
	sta AudFreq3
	
	lda #$05
	sta AudFreq0
	lda #$07
	sta AudFreq1

	lda #$a0
	sta AudCtrl2
	ldy SerialSound
	beq quiet
	lda #$a8
quiet:
	sta AudCtrl0
	sta AudCtrl1
	sta AudCtrl3
	rts
.endproc
;;; *** Cleanup
;;; *** Clean the Pokey and serial registers after a SIO operation
.proc	Cleanup
	lda #$00
	jsr AbleIRQ		; disable all serial interrupts
	lda SkStatShadow
	and #$07
        sta SkStat
	sta SkStatShadow
	lda #$00
	sta AudCtrl0
	sta AudCtrl1
	sta AudCtrl2
	sta AudCtrl3		; clear pokey audio:	 Hmm, less would be possible
	rts
.endproc
;;; *** WaitForSerialLineChange
;;; *** Wait for X changes of the serial line
.proc	WaitForSerialLineChange
waitsignal:	
	lda SIOTimerFlag
	beq timeout

	lda SkStat
	and #$10
	cmp SerialInBit
	beq waitsignal
	sta SerialInBit
	dex
	bne waitsignal
	rts
timeout:
	ldy #TimeoutError
	rts
.endproc
;;; *** SendTapeInit
;;; *** Initialize the SIO audio for tape output
.proc	SendTapeInit
	jsr AudioInit

	lda SkStatShadow
	and #$07		;
	ora #$28		;two-tone mode
	sta SkStatShadow
	sta SkStat
	sta SkReset		; reset pokey, setup serial transfer mode
	rts
.endproc
;;; *** SendTapeBytes
;;; *** Transmit bytes to the tape
.proc	SendTapeBytes
	jsr ComputeSIOBuffer
	jsr SendTapeInit
	
	ldx PalNTSCShadow	;one for PAL
	ldy ShortGapLength,x
	bit SIOAux2		;short or long inter-record gab?
	bmi isshort
	ldy LongGapLength,x
isshort:
	jsr WaitShortGap
	beq breakabort
	
	ldy #0
	sty SerialChkSum
	sty SerialChkSumDone
	sty SerialSentDone

	lda #$18
	jsr AbleIRQ	;enable Transmit empty and done IRQ, starting the transmission
	cli
wait:			;wait now until the IRQ is done.
	lda BreakFlag
	beq breakabort
	lda SerialSentDone
	beq wait
	bne done
breakabort:
	dec BreakFlag	
	ldy #BreakError
	sty SerialStatus
done:	
	rts
ShortGapLength:		.byte 15,11 	;gap length for NTSC,PAL in the short mode
LongGapLength:		.byte 180,150	;gap length for NTSC,PAL in the long mode
.endproc
;;; *** ComputeSIOBuffer
;;; *** Compute the start and end pointers of the SIO Buffer.
.proc	ComputeSIOBuffer
	clc
	lda SIOBufferLo
	sta SerBufLo
	adc SIOSizeLo
	sta SerBufEndLo
	lda SIOBufferHi
	sta SerBufHi
	adc SIOSizeHi
	sta SerBufEndHi
	rts
.endproc
;;; AbleIRQ
;;; Enable the pokey IRQ given in A
.proc	AbleIRQ
	pha
	lda #$c7
	and IRQStatShadow	; first disable all serial IRQ
	sta IRQStatShadow	; keep
	pla
	ora IRQStatShadow	; or this IRQ flag in
	sta IRQStatShadow
	sta IRQStat		; and now in pokey
	rts
.endproc
;;; ComputeTimeout
;;; Compute the timeout value for the operation
;;; with Lo->Y, Hi->X
.proc	ComputeTimeout
	lda SIOTimeout
	ror a
	ror a			; /4 * 256 = *64
	tay
	and #$3f		; -> Hi
	tax
	tya
	ror a
	and #$c0		; remainder->Lo
	tay
	rts
.endproc
;;; *** TapeInit
;;; *** The tape init vector
.proc	TapeInit
	lda #<1484
	sta BaudRate
	lda #>1484
	sta BaudRate+1
	ldx #0
	stx TapeMode
	stx TapeRecordLen
	stx TapeBufferPtr
	stx GapType
	dex
	stx TapeEOFFlag
	lda #$80
	sta RecordSize
	rts
.endproc
;;; *** Beep
;;; *** Run the keyboard beeper for A times
;;; *** and wait for a key-press
.proc	Beep
	pha
repeat:	
	lda Clock
	clc
	ldx PalNTSCShadow
	adc HalfSecond,x
	tax			;wait until then
beep:	
	lda #$ff
	sta Console
	jsr Delay
	lda #$00
	sta Console
	jsr Delay
	cpx Clock
	bne beep

	pla
	sbc #1
	beq exit
	pha
	
	lda Clock
	clc
	ldx PalNTSCShadow
	adc PauseLength,x
	
wt2:	cmp Clock
	bne wt2
	beq repeat
exit:	
	jsr GetKey
	tya			;check flags
	rts
Delay:
	ldy #$ed
wait:	
	dey
	bne wait
	rts
GetKey:
	lda KeyboardTable+5
	pha
	lda KeyboardTable+4
	pha
	rts
HalfSecond:	.byte 30,25	;for NTSC, PAL
PauseLength:	.byte 10,8	;for PAL, NTSC
.endproc
;;; *** ScanTapeOptions
;;; *** Scan the file name for tape options
.proc	ScanTapeOptions
	jsr TapeInit
	lda ZAux2
	sta GapType
	ldy #0
findcolon:	
	iny
	lda (ZAdr),y
	cmp #':'
	bne findcolon
parseoptions:	
	iny
	lda (ZAdr),y
	cmp #'/'
	bne regular
	iny
	lda (ZAdr),y
	cmp #'S'
	beq shortgap
	cmp #'N'
	beq norun
	cmp #'T'
	bne regular
	lda #0
	sta RecordSize
	lda #<1298
	sta BaudRate
	lda #>1298
	sta BaudRate+1
shortgap:
	lda #$80
	sta GapType		;short gap type enabled by /S
	bmi parseoptions
norun:
	lda ZAux1
	and #$8f
	sta ZAux1
	bcs parseoptions
regular:
	rts
.endproc
;;; *** TapeOpen
;;; *** The open vector of the tape buffer
.proc	TapeOpen
	jsr ScanTapeOptions
	lda ZAux1
	cmp #4
	beq OpenTapeForReading
	cmp #8
	beq OpenTapeForWriting
error:
	ldy #InvalidMode
	rts
.endproc
;;; *** OpenTapeForReading
;;; *** start the tape in reading mode.
.proc	OpenTapeForReading
	lda #1
	jsr Beep
	bmi error
	
	lda PalNTSCShadow
	asl a
	tax
	ldy InitialGapLength,x
	lda InitialGapLength+1,x
	jsr WaitGap
	beq breakabort

	lda RecordSize		;at the end of the record
	sta TapeBufferPtr
	sta TapeRecordLen
	ldy #0
	sty TapeEOFFlag
	iny
	rts
breakabort:
	jsr StopErrorBreak
error:
	rts
InitialGapLength:	.word 576,480
.endproc
;;; *** OpenTapeForWriting
;;; *** start the tape in writing mode.
.proc	OpenTapeForWriting
	lda #2
	jsr Beep
	bmi error
	
	jsr SendTapeInit	;start beeping until the data comes

	lda PalNTSCShadow
	asl a
	tax
	ldy InitialGapLength,x
	lda InitialGapLength+1,x
	jsr WaitGap
	beq breakabort

	lda #$80
	sta TapeMode
	lda #0
	sta TapeBufferPtr
	ldy #1
	rts
breakabort:
	jsr StopErrorBreak
error:	
	rts
InitialGapLength:	.word 1152,960
.endproc
;;; *** StopErrorBreak
;;; *** Generate a break error, then abort
.proc	StopErrorBreak
	ldy #BreakError
	dec BreakFlag
	;; runs into the following
.endproc
;;; *** StopTapeCleanup
;;; *** Stop the tape, turn the motor off, and stop the beeping.
;;; *** does not destroy the Y register.
.proc	StopTapeCleanup
	lda #$3c
	sta PIAPortACtrl
	lda #0
	sta TapeMode
	jsr Cleanup
	rts
.endproc
;;; *** TapeClose
;;; *** close function of the tape handler
.proc	TapeClose
	ldy #1
	bit TapeMode
	bpl exit
		
	lda TapeBufferPtr
	beq notbuffered
	
	ldx RecordSize
	dex
	sta TapeBuffer,x	;store the record length at the end
	lda #$fa		;tape record type
	jsr WriteTapeRecord	;write out what is buffered
	bmi exit
notbuffered:
	ldx #0
	txa
	
clrbuf:	sta TapeBuffer,x
	inx
	cpx RecordSize
	bne clrbuf
	
	lda #$fe		;the EOF record
	jsr WriteTapeRecord
exit:
	jsr StopTapeCleanup
	rts
.endproc
;;; *** TapeGet
;;; *** Run a byte-get from the tape
.proc	TapeGet
	bit TapeEOFFlag
	bmi eof
	ldx TapeBufferPtr
	cpx TapeRecordLen
	bne havedata
	clc
	jsr TapeAccess		;re-fill the buffer
	bmi exit
	ldx RecordSize		;size of a full record
	;; check for the record type
	lda TapeBuffer-1
	cmp #$fe		;EOF record?
	beq seteof
	cmp #$fa		;partial record
	bne fullrecord
	ldx RecordSize
	dex
	lda TapeBuffer,x	;get size of the partial record
	tax
fullrecord:
	stx TapeRecordLen
	ldx #0
	stx TapeBufferPtr
havedata:	
	lda TapeBuffer,x
	inc TapeBufferPtr
	ldy #1
exit:	
	rts
seteof:
	dec TapeEOFFlag		;now reached an EOF
eof:
	ldy #EndOfFile
	rts
.endproc
;;; *** TapePut
;;; *** Write a byte to the tape
.proc	TapePut
	ldy #1
	ldx TapeBufferPtr
	sta TapeBuffer,x
	inx
	cpx RecordSize
	bne setbufexit		;if still room, just done

	lda #$fc		;full record
	jsr WriteTapeRecord
	ldx #0			;reset the buffer, keep the error code
	tya
	bpl setbufexit
	stx TapeMode		;do not attempt to close
setbufexit:
	stx TapeBufferPtr
	rts
.endproc
;;; *** TapeStatus
;;; *** The status handler of the tape.
.proc	TapeStatus
	ldy #1			;we're fine
	rts
.endproc
;;; *** TapeSpecial
;;; *** The special handler of the tape
.proc	TapeSpecial
	lda ZCmd
	cmp #CmdBload
	beq TapeBoot		;this is the bootstrap command.
	;; return the CIO-pre-set error code
	rts
.endproc
;;; *** TapeBoot
;;; *** Boot from a tape-based boot file.
.proc	TapeBoot
	;; the Os is really wierd. It first stores
	;; the init address in DosInit, and then moves
	;; the data over. Wierd enough, some programs
	;; even depend on this mess.

	lda DosVector
	pha
	lda DosVector+1
	pha
	lda DosInit
	pha
	lda DosInit+1
	pha

	jsr ScanTapeOptions
	lda ZAux1
	sta TapeBload
	lda #$80
	sta GapType
	
	jsr OpenTapeForReading
	tya
	bmi errortrampoline
	clc
	jsr TapeAccess
	tya
	bmi errortrampoline
	lda TapeBuffer+1
	sta DiskBootSectors
	lda TapeBuffer+2
	sta DiskBootAddress
	sta BootPtr
	lda TapeBuffer+3
	sta BootPtr+1
	sta DiskBootAddress+1
	ora BootPtr
	bne continue
booterror:	
	ldy #NoBinaryFile
	bmi errortrampoline
continue:
	lda TapeBuffer+4
	sta DosInit
	lda TapeBuffer+5
	sta DosInit+1

	ldy HatabsCopy
	lda HatabsCopy+1
	jsr InitTapeAt
	;; now copy the tape boot data to their final destination
nextblock:	
	ldy #0
cptotarget:
	lda TapeBuffer,y
	sta (BootPtr),y
	iny
	bpl cptotarget
	clc
	lda BootPtr
	adc #$80
	sta BootPtr
	bcc noincb
	inc BootPtr+1
noincb:
	dec DiskBootSectors
	beq done
	clc
	jsr TapeAccess
	tya
	bpl nextblock
errortrampoline:
	bmi error
done:
	clc
	jsr TapeAccess		;read EOF chunk

	clc
	lda DiskBootAddress
	adc #6
	sta BootPtr
	lda DiskBootAddress+1
	adc #0
	sta BootPtr+1

	jsr CallTapeInit
	bcs booterror
	
	bit TapeBload
	bpl noinit
	jsr CallTapeRun
noinit:	
	jsr PrepBootEnv
	
	pla
	sta DosInit+1
	pla
	sta DosInit
	
	jsr StopTapeCleanup

	pla
	tax
	pla
	cmp DosVector
	bne runprogram
	cpx DosVector+1
	beq norun

runprogram:
	ldy DosVector
	sty RunVector
	ldy DosVector+1
	sty RunVector+1

	sta DosVector
	stx DosVector+1

	bit TapeBload
	bvc norun
	jsr CallRunVector
norun:
exit:
	ldy #1
	rts
error:
	tya
	pha
	jsr StopTapeCleanup
	jsr TapeInitialize
	pla
	tay
restore:	
	pla
	sta DosInit+1
	pla
	sta DosInit
	pla
	sta DosVector+1
	pla
	sta DosVector
	rts
PrepBootEnv:
	lda BootFlag
	ora #2
	sta BootFlag
	lda DosInit
	sta CasInit
	lda DosInit+1
	sta CasInit+1
	jsr TapeInitialize
	rts
CallTapeInit:
	sec
	jmp (BootPtr)
CallTapeRun:
	jmp (DosInit)
CallRunVector:
	jmp (RunVector)
.endproc
;;; *** TapeInitialize
;;; *** Hook the new tape vector into HaTabs
.proc	TapeInitialize
	jsr AdjustMemLo
	jsr TapeInit
	ldy TapeLoPtr
	lda TapeLoPtr+1
	;; runs into the following
.endproc
;;; *** InitTapeAt
;;; *** Mount the tape hander for the entry
;;; *** at the given offset.
.proc	InitTapeAt
	ldx #'C'
	jsr MountHandlerVector
	bcc ismounted
	bmi full
	sta HaTabs+1,x
	tya
	sta HaTabs,x		; force replace
ismounted:
full:	
	rts
.endproc
;;; *** AdjustMemLo
;;; *** Ensure MemLo is behind the current setting
.proc	AdjustMemLo
	lda MemLo
	cmp MemLoAdj
	lda MemLo+1
	sbc MemLoAdj+1
	bcs exit
	lda MemLoAdj
	sta MemLo
	lda MemLoAdj+1
	sta MemLo+1
exit:
	rts
.endproc
;;; *** TapeReset
;;; *** Called on reset by the Os.
.proc	TapeReset
	jsr TapeInitialize
	clc
	lda TapeLoPtr
	adc #<(2*6+9*7)
	sta CallInit+1
	lda TapeLoPtr+1
	adc #>(2*6+9*7)
	sta CallInit+2
CallInit:
	jsr $ffff		;jump to the old init vector
	jsr AdjustMemLo
	rts
.endproc
	;; Length
	.global TapeResidentLength
TapeResidentLength	=	*-TapeResidentStart
