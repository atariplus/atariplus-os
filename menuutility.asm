;;; **********************************************************************
;;; ** Thor Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: menuutility.asm,v 1.9 2013/11/24 19:29:00 thor Exp $         **
;;; **                                                                  **
;;; ** In this module:          Various Utility Functions               **
;;; **********************************************************************

        .include "menujmptab.i"
        .include "kernel.i"
        .include "fms.i"
        .include "errors.i"
	.include "cio.i"
	.include "sio.i"
	.include "diskinterf.i"
	.include "irq.i"
	.include "nmi.i"
	.include "screen.i"
        .segment "menufunction"

SIOBuffer	=	$c2	;the IO buffer to use.
SecsPerTrack	=	$f3	;the number of sectors per track for the scanner
CurrentSector	=	$f6	;current sector addressed (two bytes)
CurrentTrack	=	$f9	;the curren track we are on
FullSecsCount	=	$e8	;counts the number of filled sectors
FreeSecsCount	=	$ea	;counts the free sectors
ErrorSecsCount	=	$ec	;counts the number of error sectors
	
;;; *** Skip two bytes (by a dummy BIT)
.macro  Skip2
        .byte $2c
.endmacro
	
;;; init vector
	rts
	jmp CheckSectors
	jmp RadixConvert
	jmp CheckDriveSpeed
	jmp SetHeadline
	jmp DiskDensity
	jmp AdjustDriveDefault
;;; *** AccessSector
;;; *** Access the sector X,Y on the current device.
;;; *** C=0: Read, C=1 write. Does not test for errors
.proc   AccessSector
        sty SIOAux2
        stx SIOAux1
        lda #'R'
        bcc isread
        lda WriteCommand
isread:
        sta SIOCommand
	lda SIOBuffer
	sta SIOBufferLo
	lda SIOBuffer+1
        sta SIOBufferHi
        jsr DiskInterfVector
        rts
.endproc
;;; *** TestedAccessSector
;;; *** Similar to the above, but errors are reported
.proc	TestedAccessSector
	jsr AccessSector
	jsr CheckIOError
	rts
.endproc
;;; *** GetDeviceNumber
;;; *** Read a device number from the keyboard, return in A
;;; *** Returns with C=1 if the user pressed return or BREAK
;;; *** to abort.
.proc	GetDeviceNumber
	jsr CursorOn
	jsr EnableBREAK
	jsr GetKey
	cmp #155
	beq abort
	cmp #'D'		;D: is also allowed here.
	bne nodisk
	sta ZPtr
	jsr EchoInput
	jsr GetKey		;but the next one must be a number
	cmp #155
	beq abort
	bne keytwo
nodisk:
	pha
	lda #'D'
	sta ZPtr
	jsr EchoInput
	pla
keytwo:	
	sta ZPtr
	cmp #'1'
	bcc nodrive
	cmp #'9'
	bcs nodrive
	jsr EchoInput
	lda ZPtr
	and #$0f
	pha
	lda #':'
	sta ZPtr
	jsr EchoInput
	jsr DisableBREAK
	jsr PrintEOL
	clc
	pla
	rts
nodrive:
	ldx #<BadDriveNumber
	ldy #>BadDriveNumber
	jsr PrintRecord
	jmp ParameterError
abort:	
	sec
	rts
EchoInput:
	ldx #<ZPtr
	ldy #>ZPtr
	lda #1
	jsr Print
	rts
BadDriveNumber:
	.byte 155
	.byte "Drive number must be a number"
	.byte "between 1 and 9.",155
.endproc
;;; *** AllocateSIOBuffer
;;; *** Allocate the IO Buffer for all SIO based commands.
.proc	AllocateSIOBuffer
	ldx #<128
	ldy #>128
	jsr Reserve
	stx SIOBuffer
	sty SIOBuffer+1
	rts
.endproc
;;; *** GetDensity
;;; *** Returns the number of sectors of the disk in X,Y and the
;;; *** number of sectors per track in A. SIODeviceUnit must
;;; *** already been set.
.proc	GetDensity
	lda #'S'
	sta SIOCommand
	jsr DiskInterfVector
	jsr CheckIOError
	lda DiskStatus+1
	bpl devicenak
	lda #26
	ldx #<$410
	ldy #>$410
	bit DiskStatus
	bmi exit
	lda #18
	ldx #<$2d0
	ldy #>$2d0
exit:
	rts
devicenak:
	ldy #DeviceNak
	jmp Error
.endproc
;;; *** IncrementSectorCount
;;; *** Increment the sector counter indexed by X
;;; *** x=0: filled sectors, x=2: empty sectors, x=4: error sectors
.proc	IncrementSectorCount
	inc FullSecsCount,x
	bne exit
	inc FullSecsCount+1,x
exit:
	rts
.endproc
;;; *** ClearSectorCounter
;;; *** Reset all sector counters
.proc	ClearSectorCounter
	ldx #7
	lda #0
rloop:	
	sta FullSecsCount,x
	dex
	bpl rloop
	rts
.endproc
;;; *** CheckSector
;;; *** Check the sector (X,Y) and update counters accordingly.
;;; *** return the character to print in A. Returns A=0 if
;;; *** the user wants to abort.
.proc	CheckSector
	clc			;read
	jsr AccessSector
	bmi errorsector
	ldy #0
tloop:	lda (SIOBuffer),y
	bne nonempty
	iny
	bpl tloop
	;; here: the sector is empty.
	ldx #2
	jsr IncrementSectorCount
	lda #'.'
	rts
nonempty:
	cpy #$7d		;only the file link?
	bcc full
	;; here only the file link is present, the rest is zero.
	lda #'+'
	Skip2
full:
	lda #'*'		;full
	pha
	ldx #0			;still non-empty
	jsr IncrementSectorCount
	pla
	rts
errorsector:
	cpy #BreakError
	beq abort
	lda BreakFlag
	beq abort

	ldx #4			;error sectors
	jsr IncrementSectorCount
	lda #'-'		;defect sector
	rts
abort:
	lda #0
	rts
.endproc
;;; *** PrintCount
;;; *** Print the sector counter indexed by X on the screen
.proc	PrintCount
	ldy FullSecsCount+1,x
	lda FullSecsCount,x
	tax
	jsr ToHex
	lda #'$'
	sta ZPtr
	ldx #<ZPtr
	ldy #>ZPtr
	lda #1
	jsr Print
	jsr LoadBuf3Ptr
	lda #4
	jsr Print
	rts
.endproc
;;; *** PrintSectorStatistics
;;; *** Print the sector counts collected during the disk scan.
.proc	PrintSectorStatistics
	jsr DisableBREAK
	jsr PrintEOL
	;; full sectors
	ldx #0
	jsr PrintCount
	ldx #<FullMsg
	ldy #>FullMsg
	lda #45
	jsr PrintRecord
	;; empty sectors
	ldx #2
	jsr PrintCount
	ldx #<EmptyMsg
	ldy #>EmptyMsg
	lda #45
	jsr PrintRecord
	;; error sectors
	ldx #4
	jsr PrintCount
	ldx #<ErrorMsg
	ldy #>ErrorMsg
	lda #45
	jsr PrintRecord
	rts
FullMsg:	.byte " full  sectors,",155
EmptyMsg:	.byte " empty sectors and",155
ErrorMsg:	.byte " error sectors.",155
.endproc
;;; *** ScanOneTrack
;;; *** Scan one track of sectors, returns C=1 on abortion
.proc	ScanOneTrack
	lda SecsPerTrack
	sta Z2Ptr
	ldx CurrentSector
	ldy CurrentSector+1
	jsr ToHex
	jsr LoadBuf3Ptr
	jsr Advance
	lda #3
	jsr Print		;print the sector number
	ldx #<Colon
	ldy #>Colon
	lda #ColonL
	jsr Print
secloop:
	ldx CurrentSector
	ldy CurrentSector+1
	jsr CheckSector
	beq abort
	sta ZPtr
	ldx #<ZPtr
	ldy #>ZPtr
	lda #1
	jsr Print
	
	inc CurrentSector
	bne nohi
	inc CurrentSector+1
nohi:
	dec Z2Ptr
	bne secloop
	;; print the track number
	lda #32
	sta CursorColumn
	ldx #<Track
	ldy #>Track
	lda #TrackL
	jsr Print

	ldx CurrentTrack
	ldy #0
	jsr ToHex
	jsr LoadBuf3Ptr
	jsr Advance
	jsr Advance
	lda #2
	jsr Print
	jsr PrintEOL
	inc CurrentTrack
	clc
	rts
abort:
	sec
	rts
Advance:
	inx
	bne exit
	iny
exit:	rts
Colon:	.byte ": "
ColonL	=	*-Colon
Track:	.byte " tr.:"
TrackL	=	*-Track
.endproc
;;; *** CheckEntireDisk
;;; *** check the full entire disk.
.proc	CheckEntireDisk
	jsr ClearSectorCounter
	jsr PrintEOL
	jsr EnableBREAK
	ldx #0
	stx CurrentTrack
	stx CurrentSector+1
	inx
	stx CurrentSector
trackloop:
	jsr ScanOneTrack
	bcs abort
	lda CurrentTrack
	cmp #40
	bcc trackloop
abort:
	jsr PrintEOL
	jsr PrintSectorStatistics
	jsr DisableBREAK
	rts
.endproc
;;; *** CheckSectors
;;; *** check the sectors of the given drive.
.proc	CheckSectors
	ldx #<CheckReq
	ldy #>CheckReq
	lda #CheckReqL
	jsr Print
	jsr GetDeviceNumber
	bcs exit
	sta SIODeviceUnit
	jsr AllocateSIOBuffer
	jsr BlockDisplay
	jsr PrintEOL
	ldx #<CheckTitle
	ldy #>CheckTitle
	lda #45
	jsr PrintRecord
	jsr GetDensity
	sta SecsPerTrack
	jsr CheckEntireDisk
exit:
	rts
CheckReq:	.byte "Enter drive # to check:"
CheckReqL	=	*-CheckReq
CheckTitle:	.byte "Sector        contents           track",155
.endproc
;;; *** RunExtendedCommand
;;; *** Run a command over SIO (not CIO). Command in A,
;;; *** buffer in SIOBuffer, C=1 is write
.proc	RunExtendedCommand
	sta SIOCommand
	ldx SIOBuffer
	stx SIOBufferLo
	ldy SIOBuffer+1
	sty SIOBufferHi
	lda #$80		;write?
	bcs havedir
	lsr a			;read command
havedir:	
	sta SIOStatus
	lda #<128
	sta SIOSizeLo
	lda #>128
	sta SIOSizeHi
	lda #$31		;disk drive
	sta SIODeviceId
	lda #15
	sta SIOTimeout
	lda #0
	sta SIOAux1
	sta SIOAux2
	jsr SIOVector
	rts
.endproc
;;; *** Run a diag test #A, return results in X,Y
.proc	RunDiag
	ldy #0
	sta (SIOBuffer),y	;store the type of the test
	lda #$23
	sec
	jsr RunExtendedCommand	;start the motor test
	cpy #DeviceError	;not supported?
	beq not1050
	jsr CheckIOError
	lda #$24
	clc
	jsr RunExtendedCommand
	jsr CheckIOError
	ldy #0
	lda (SIOBuffer),y	;low-byte
	tax
	iny
	lda (SIOBuffer),y
	tay
	rts
not1050:
	ldx #<Only1050
	ldy #>Only1050
	lda #Only1050L
	jsr Print
	ldy #139
	jmp CheckIOError
Only1050:
	.byte "Drive diagnostics require the inter-",155
	.byte "nal diagnostics commands only imple-",155
	.byte "mented in the 1050. Third party drives",155
	.byte "are not supported.",155
Only1050L	=	*-Only1050
.endproc
;;; *** MicroStepUp
;;; *** Move the stepper motor just a tiny bit with a diagnostic command
.proc	MicroStepUp
	ldy #1
	lda #0
	sta (SIOBuffer),y
	lda #3			;move up command
	dey
	sta (SIOBuffer),y
	lda #$23		;write DIAG
	sec
	jsr RunExtendedCommand
	rts
.endproc
;;; *** ReadT0Sensor
;;; *** Read the track 0 sensor. Returns N=1 on error
;;; *** Returns NE if the sensor is *SET*, i.e. we are on track zero.
.proc	ReadT0Sensor
	lda #'S'
	sta SIOCommand
	jsr DiskInterfVector
	bmi error
	lda DiskStatus+1
	and #4			;the track-zero indicator bit
error:	
	rts
.endproc	
;;; *** CheckDriveSpeed
;;; *** Check the speed of a drive
.proc	CheckDriveSpeed
	ldx #<SpeedReq
	ldy #>SpeedReq
	lda #SpeedReqL
	jsr Print
	jsr GetDeviceNumber
	bcc runtest
	rts
runtest:
	sta SIODeviceUnit
	jsr CursorOff
	jsr PrintEOL
	jsr BlockDisplay
	jsr DisableBREAK
	jsr AllocateSIOBuffer
	jsr GetDensity
	sta SecsPerTrack
	;;
	lda #1			;motor speedup test
	jsr RunDiag
	jsr SetZPtr
	ldx #<MotorSpeedup
	ldy #>MotorSpeedup
	lda #MotorSpeedupL
	jsr Print
	ldx ZPtr
	ldy ZPtr+1
	jsr PrintDecimal
	ldx #<MotorSpeedupNorm
	ldy #>MotorSpeedupNorm
	lda #MotorSpeedupNormL
	jsr Print
	;;
	lda #0
	jsr RunDiag		;motor speed test
	jsr SetZPtr
	ldx #<RotationRate
	ldy #>RotationRate
	lda #RotationRateL
	jsr Print
	ldx ZPtr
	ldy ZPtr+1
	jsr PrintDecimal
	ldx #<RotationRateNorm
	ldy #>RotationRateNorm
	lda #RotationRateNormL
	jsr Print
	;; Step test.
	ldx #<1
	ldy #>1
	clc
	jsr AccessSector
	ldx SecsPerTrack
	ldy #0
	clc
	jsr AccessSector

	ldy #0
	tay
clr:	sta (SIOBuffer),y
	iny
	bpl clr
	
	ldy #1
	lda #$1a		;possibly: number of sectors per track?
	sta (SIOBuffer),y
	lda #2
	dey
	sta (SIOBuffer),y
	sec
	lda #$23
	jsr RunExtendedCommand
	lda SIOStatus
	cmp #1
	beq ok
	ldx #<StepperFail
	ldy #>StepperFail
	bne print
ok:
	ldx #<StepperOK
	ldy #>StepperOK
print:
	lda #45
	jsr PrintRecord
	jsr PrintEOL
	;; run the track zero test.
	ldx #<1
	ldy #>1
	clc
	jsr AccessSector
	bmi t0error

	ldx #<3
	ldy #>3
	clc
	jsr AccessSector
	bmi t0error
	ldx #0
	ldy #0
	clc
	jsr AccessSector	;no matter.
	jsr ReadT0Sensor
	bmi t0error
	beq t0error		;must be on T0
	jsr MicroStepUp
	jsr ReadT0Sensor
	bmi t0error
	beq t0error		;must still be on T0
	jsr MicroStepUp
	;; ignore result.
	jsr MicroStepUp
	jsr ReadT0Sensor
	bmi t0error
	bne t0error		;must no longer be on T0
	jsr MicroStepUp
	jsr ReadT0Sensor
	bmi t0error
	bne t0error		;ditto
	;; read something else, forcing re-calibration.
	clc
	lda SecsPerTrack
	asl a
	asl a			;*4
	adc SecsPerTrack	;*5
	tax
	ldy #0
	clc
	jsr AccessSector
	bpl isok
	cmp #TimeoutError
	bne t0error
	ldx SIOAux1
	ldy SIOAux2
	clc
	jsr AccessSector
	bmi t0error
	;; and back to zero
isok:	
	ldx #<1
	ldy #>1
	clc
	jsr AccessSector
	bmi t0error
	;; here all ok.
	ldx #<SensorOK
	ldy #>SensorOK
	bne prints
t0error:
	ldx #<SensorFail
	ldy #>SensorFail
prints:
	lda #45
	jsr PrintRecord
exit:	
	rts
SpeedReq:		.byte "Enter drive # to check:"
SpeedReqL		=	*-SpeedReq
MotorSpeedup:		.byte "Raw motor speed-up time : "
MotorSpeedupL		=	*-MotorSpeedup
MotorSpeedupNorm:	.byte 155,"Value should be < 9000.",155,155
MotorSpeedupNormL	=	*-MotorSpeedupNorm

RotationRate:		.byte "Raw rotation rate       : "
RotationRateL		=	*-RotationRate

RotationRateNorm:	.byte 155,"Value should be between 2025 and 2082.",155,155
RotationRateNormL	=	*-RotationRateNorm

StepperFail:	.byte "Head settle test failed.",155
StepperOK:	.byte "Head settle test passed.",155
SensorFail:	.byte "Track 0 sensor test failed.",155
SensorOK:	.byte "Track 0 sensor test passed.",155
.endproc
;;; *** RadixConvert
;;; *** Convert between hex and decimal
.proc	RadixConvert
	ldx #<RadixTitle
	ldy #>RadixTitle
	lda #45
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr LoadInputBufferPtr
	jsr SetZPtr
	ldy #0
	lda (ZPtr),y
	cmp #155
	beq exit
	cmp #'$'
	beq ishex
	cmp #'0'
	bne isdec
	iny
	lda (ZPtr),y
	cmp #'x'
	beq ishex
	cmp #'X'
	beq ishex
isdec:
	jsr LoadInputBufferPtr
	jsr FromDecimal
	clc
	bcc printresult
ishex:
	sec			;plus one
	tya
	ldy ZPtr+1
	adc ZPtr
	bcc noinc
	iny
noinc:
	tax
	jsr FromHex
printresult:
	stx Z2Ptr
	sty Z2Ptr+1
	ldx #<Result1
	ldy #>Result1
	lda #Result1L
	jsr Print
	ldx Z2Ptr
	ldy Z2Ptr+1
	jsr PrintDecimal
	ldx #<Result2
	ldy #>Result2
	lda #Result2L
	jsr Print
	ldx Z2Ptr
	ldy Z2Ptr+1
	jsr ToHex
	jsr LoadBuf3Ptr
	lda #4
	jsr Print
	ldx #<Result3
	ldy #>Result3
	jsr PrintRecord
exit:
	rts
RadixTitle:	.byte "Give number, hex starts with ",34,"$",34," :",155
Result1:	.byte "Decimal "
Result1L	= * - Result1
Result2:	.byte " = Hex $"
Result2L	= * - Result2
Result3:	.byte ".",155
Result3L	= * - Result3
.endproc
;;; *** PrintDecimal
;;; *** Print the decimal number in X,Y without any additional blanks.
.proc	PrintDecimal
	jsr ToDecimal
	jsr LoadBuf3Ptr
	jsr SetZPtr
	ldy #$ff
len:	iny
	lda (ZPtr),y
	cmp #' '
	bne len
	tya
	pha
	jsr LoadBuf3Ptr
	pla
	jsr Print
	rts
.endproc
;;; *** SetHeadline
;;; *** Adjust the headline of the disk if it is possible.
.proc	SetHeadline
	ldx #<HdlReq
	ldy #>HdlReq
	lda #45
	jsr PrintRecord
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr LoadInputBufferPtr
	jsr AddDevice
	jsr CursorOff
	jsr LoadInputBufferPtr
	jsr SetZPtr
	;; check whether there is a device specifier
	ldy #1
	lda #':'
	cmp (ZPtr),y
	beq unitone		;yes
	iny
	cmp (ZPtr),y		;or here?
	bne baddevice		;huh? Didn't we add the device?
	dey
	lda (ZPtr),y
	cmp #'1'
	bcc badunit
	cmp #'8'+1
	bcs badunit
	and #$0f		;this will be the unit
	iny			;headline starts behind here.
	Skip2
unitone:
	lda #1
	sta SIODeviceUnit
	sty Z2Ptr		;keep the offset.
	ldy #0
	lda (ZPtr),y		;must be the disk.
	cmp #'D'
	bne baddevice
	
	jsr AllocateSIOBuffer
	
	ldx #<$169
	ldy #>$169
	clc
	jsr TestedAccessSector
	ldy #0
	sty Z2Ptr+1
	lda (SIOBuffer),y
	beq free
	bmi free
	cmp #$63
	bne inuse
free:	
	lda #$63
	sta (SIOBuffer),y	;install the flag
	
	ldy Z2Ptr
	iny
	lda (ZPtr),y
	cmp #$9b		;empty? If so, clear it
	beq clearhdl
	;; here: fill it.
setloop:
	inc Z2Ptr
	ldy Z2Ptr
	lda (ZPtr),y		;get the next character
	cmp #$9b
	bne fillme
	dec Z2Ptr
	lda #' '		;blank the rest of it
fillme:
	inc Z2Ptr+1
	ldy Z2Ptr+1
	ora #$80		;must be inversevid
	sta (SIOBuffer),y
	cpy #$0f
	bcc setloop
	bcs writeback
clearhdl:			;here erase the headline.
	lda #$80		;erased file flag
	ldy #0
	sta (SIOBuffer),y
writeback:
	sec
	ldx #<$169
	ldy #>$169
	jsr TestedAccessSector
	rts
badunit:
	ldy #IllegalUnit
	jmp Error
baddevice:
	ldx #<DiskOnly
	ldy #>DiskOnly
	lda #45
	jsr PrintRecord
	ldy #InvalidCmd
	jmp Error
inuse:	ldx #<HdlInUse
	ldy #>HdlInUse
	lda #45
	jsr PrintRecord
	ldy #DirectoryFull
	jmp Error
HdlReq:		.byte "Set headline - give (device:)headline",155
DiskOnly:	.byte "Only the disk device supports headlines.",155
HdlInUse:	.byte "Headline position already in use.",155
.endproc
;;; *** DiskDensity
;;; *** Print the disk density
.proc	DiskDensity
	ldx #<DensityReq
	ldy #>DensityReq
	lda #DensityReqL
	jsr Print
	jsr GetDeviceNumber
	bcs exit
	sta SIODeviceUnit
	jsr CursorOff
	jsr PrintEOL
	jsr GetDensity
	sta Z2Ptr
	jsr SetZPtr
	lda Z2Ptr
	cmp #26
	bcc single
	ldx #<Enhanced
	ldy #>Enhanced
	lda #EnhancedL
	bne print1
single:
	ldx #<Single
	ldy #>Single
	lda #SingleL
print1:
	jsr Print
	ldx #<DensityMsg
	ldy #>DensityMsg
	lda #45
	jsr PrintRecord
	ldx ZPtr
	ldy ZPtr+1
	jsr PrintDecimal
	ldx #<Sectors
	ldy #>Sectors
	lda #45
	jsr PrintRecord
	ldx Z2Ptr
	ldy #0
	jsr PrintDecimal
	ldx #<SecsPerTrackMsg
	ldy #>SecsPerTrackMsg
	lda #45
	jsr PrintRecord
exit:	
	rts
DensityReq:	.byte "Enter drive # to check:"
DensityReqL	= * - DensityReq
Enhanced:	.byte "Enhanced"
EnhancedL	= * - Enhanced
Single:		.byte "Single"
SingleL		= * - Single
DensityMsg:	.byte " density,",155
Sectors:	.byte " sectors,",155
SecsPerTrackMsg:	.byte " sectors per track.",155
.endproc
;;; *** AdjustDriveDefault
;;; *** Set the default device
.proc	AdjustDriveDefault
	ldx #<DefaultReq
	ldy #>DefaultReq
	lda #DefaultReqL
	jsr Print
	ldx #<DefaultDevice
	ldy #>DefaultDevice
	lda DefaultDeviceLen
	jsr Print
	ldx #<DefaultDeviceEnd
	ldy #>DefaultDeviceEnd
	lda #DefaultDeviceEndL
	jsr Print
	jsr CursorOn
	jsr LoadInputBufferPtr
	jsr GetLine
	jsr CursorOff
	jsr LoadInputBufferPtr
	jsr SetZPtr
	ldy #0
	lda (ZPtr),y
	beq baddevice
	cmp #$9b		;empty->do nothing
	beq exit
	;; check whether the thing actually exists in hatabs
	ldx #0
scan:
	lda HaTabs,x
	beq baddevice
	cmp (ZPtr),y
	beq isgood
	inx
	inx
	inx
	cpx #$21
	bcc scan
	bcs baddevice
isgood:	
	jsr LoadInputBufferPtr
	jsr SetDefaultDevice
	bcc exit		;done
baddevice:	
	ldx #<BadDevice
	ldy #>BadDevice
	lda #45
	jsr PrintRecord
	ldy #UnknownDevice
	jmp Error
exit:
	rts
DefaultReq:		.byte "Give default device (currently "
DefaultReqL		=	*-DefaultReq
DefaultDeviceEnd:	.byte ")",155
DefaultDeviceEndL	=	*-DefaultDeviceEnd
BadDevice:		.byte "Invalid device specification",155
.endproc


	
