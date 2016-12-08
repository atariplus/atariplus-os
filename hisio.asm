;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: hisio.asm,v 1.15 2015/08/15 14:47:43 thor Exp $		**
;;; **									**
;;; ** In this module:	 High Speed Serial IO interface			**
;;; ** This code originates from the high-speed SIO patch by		**
;;; ** Matthias Reichl							**
;;; **********************************************************************

	.include  "pokey.i"
	.include  "antic.i"
	.include  "pia.i"
	.include  "irq.i"
	.include  "nmi.i"
	.include  "sio.i"
	.include  "misc.i"
	.include  "errors.i"
	.include  "kernel.i"
	.include  "hisio.i"
	
	.segment  "CharLo"

	AudioInitHi = AudioInit+2
	
.macro	Skip2
	.byte $2c
.endmacro
.macro	Skip1
	.byte $24
.endmacro
;;; *** SIOInit
;;; *** Setup the serial IO internface
	.global SIOInit
.proc	SIOInit
	lda #$3c
	sta PIAPortBCtrl	; ditto
	lda #$03
	sta SkStatShadow
	sta SkStat		; reset pokey
	sta SerialSound		; enable sound output
	lda #0
	ldx #7
clrspd:	sta SpeedTab,x		; clear the drive speed recognition, redo on reset.
	dex
	bpl clrspd
	rts
.endproc
;;; *** InitCmdFrameBuffer
;;; *** Set the buffer size and pointers to the command
;;; *** frame
.proc	InitCmdFrameBuffer
	lda #<SIOCmdFrame
	ldx #>SIOCmdFrame
	sta SerBufLo
	stx SerBufHi
	;; size of the buffer
	lda #<4
	ldx #>4
	sta SerBufEndLo
	stx SerBufEndHi
	rts
.endproc
;;; *** InitBuffer
;;; *** Initialize the SIO size and buffer pointers
;;; *** for a block transmission
.proc	InitBuffer
	lda SIOSizeLo
	ldx SIOSizeHi
	sta SerBufEndLo
	stx SerBufEndHi
	lda SIOBufferLo
	ldx SIOBufferHi
	stx SerBufHi
	sta SerBufLo
	rts
.endproc
;;; *** HiSpeedSendByte
;;; *** Transmit a byte in A in high-speed mode
.proc	HiSpeedSendByte
	tax
	lda #$10
wait:	bit IRQStat
	bne wait
	lda IRQStatShadow	;clear the bit again (it is disabled in the shadow)
	sta IRQStat
	stx SerDat
	txa
	clc
	adc SerialChkSum
	adc #$0
	sta SerialChkSum
	rts
.endproc
;;; *** CheckSum
;;; *** Compute the checksum over the received bytes and return
;;; *** eq if it is identical to the transmitted checksum.
.proc	CheckSum
	ldx SerBufEndHi
	lda #0
	tay
loop:	clc
	adc (SerBufLo),y
	adc #0
	iny
	bne winc
	inc SerBufHi
	dex
winc:
	cpy SerBufEndLo
	bne loop
	cpx #0
	bne loop
	cmp SerialChkSum	;compare against the stored sum.
	rts
.endproc
;;; *** TransmitBytes
;;; *** Transmit a data block in high speed
;;; *** Returns with eq if everything is fine, ne on error.
.proc	TransmitBytes
	;; Initialize Pokey for sending data
	ldy #$01
	sty SerialStatus	; clear the status flags
	lda #$0
	jsr AbleIRQ		; no serial interrupts enabled here, also restore all regularly enabled IRQs here.
	lda #$20
	jsr SetSerialMode
	dey
	lda (SerBufLo),y
	sta SerDat
	sta SerialChkSum
	iny
	bne winc
sendloop:	
	lda (SerBufLo),y
	jsr HiSpeedSendByte
	iny
	bne winc
	inc SerBufHi
	dec SerBufEndHi		;misused as length here
	;; delay
	ldx #$e0
wait2:	inx
	bne wait2
winc:
	cpy SerBufEndLo
	bne sendloop
	lda SerBufEndHi
	bne sendloop
	
	;; transmission done, just complete with the checksum
	lda SerialChkSum
	jsr HiSpeedSendByte
	;; No, we do not wait here until the transmission is done,
	;; only wait until the register is free. IRQDone will be
	;; waited for when waiting for the acknowledge of the data.
	lda #$10
wait:	bit IRQStat
	bne wait
	lda IRQStatShadow	;clear the bit again (it is disabled in the shadow)
	sta IRQStat
	rts
.endproc
;;; *** Receive
;;; *** Receive a block or bytes with the High speed SIO transfer
;;; *** The receive mode depends on SerialNoChkSum:
;;; *** An acknowledge phase is always expected and the result is
;;; *** returned in A.
;;; *** If bit 6 is set, a data phase follows, plus a checksum.
;;; *** If bit 7 is set, a second acknowledge phase follows if
;;; *** the first acknowledge phase transmitted an 'A'.
.proc	Receive
	;; IRQs are still disabled from sending the command frame
	lda IRQStatShadow
	pha
	and #$80		;only keep the Break key enable.
	sta IRQStatShadow
	sta IRQStat		;disable all other interrupts.
	lda BreakVec+1
	pha
	lda BreakVec
	pha
	lda VecImmediate+1
	pha
	lda VecImmediate
	pha
	tsx			; keep stack pointer for direct return
	stx SIOStackPtr
	lda #6
	ldy #<HispeedVBI
	ldx #>HispeedVBI
	jsr SetIRQVector
	lda #16
	ldy #<HispeedBreak
	ldx #>HispeedBreak
	jsr SetIRQVector
	ldy #1
	sty SerialStatus	; indicate an ok transmission
	dey			; start with Y=0
	;; here: Read the drive acknowledge as part of the process.
	lda #$20
	ldx IRQStatShadow
wait2:	bit IRQStat
	bne wait2
	stx IRQStat
	ldx SerDat		;empty the buffer

	;; now check for serial transmission errors
	bit SkStat
	bpl errframing
	beq erroverrun

	stx SIOStatusTmp	;store here

	bit SerialNoChkSum	;does a data phase follow?
	bvs readbyte
	bpl exit		;does a second byte follow? If not, we are done now.
	
	;; now receive the result of writing the data
	ldx IRQStatShadow
wait4:	bit IRQStat
	bne wait4
	stx IRQStat
	ldx SerDat		;empty the buffer
	
	;; now check for serial transmission errors
	bit SkStat
	bpl errframing
	beq erroverrun
	
	cpx #'A'		;but the second acknowledge only follows if the first was ok.
	beq exit		;is ok, keep the result of the first phase
	cpx #'C'
	beq exit		;is also ok, keep the result of the first phase
	stx SIOStatusTmp	;store new error, overwrite the result of the above
	bne exit
	;; here read the data phase. This is always delivered, even if the result is negative.
readbyte:
	lda #$20		;transmission buffer full flag
	ldx IRQStatShadow
wait:	bit IRQStat
	bne wait

	stx IRQStat
	ldx SerDat		;empty the buffer
	
	;; now check for serial transmission errors
	bit SkStat		;overrun?
	bpl errframing
	beq erroverrun
	;; store the data
	txa			;get the received byte
	sta (SerBufLo),y
	;; increment the buffer pointer
	iny
	bne nowrap
	inc SerBufHi
	dec SerBufEndHi
nowrap:
	cpy SerBufEndLo
	bne readbyte
	lda SerBufEndHi
	bne readbyte
	;; here done. Receive the checksum.
	lda #$20		;transmission buffer full flag
	ldx IRQStatShadow
wait3:	bit IRQStat
	bne wait3
	stx IRQStat
	ldx SerDat		;empty the buffer
	
	;; now check for serial transmission errors
	bit SkStat
	bpl errframing
	beq erroverrun
	stx SerialChkSum
	;; test now for the validity of the checksum
	jsr InitBuffer	   	;load the IO buffer into the buffer pointer.
	jsr CheckSum
	beq exit		;if identical, return with ok.
	;; otherwise, a checksum error.
	ldy #ChkSumError
	Skip2
errframing:
	ldy #FrameError
	Skip2
erroverrun:
	ldy #OverrunError
	sty SerialStatus
exit:	
	;; returns the error
.endproc
;;; *** SerialRecDone
;;; *** Callers jump in here to return from the high speed serial receive
.proc	SerialRecDone
	pla
	tay
	pla
	tax
	lda #6
	jsr SetIRQVector
	pla
	tay
	pla
	tax
	lda #16
	jsr SetIRQVector
	pla
	sta IRQStatShadow
	sta IRQStat
	cli			;if this was part of an interrupt, enable again
	;; return the data
	lda SIOStatusTmp	;the drive acknowledge byte
	ldx BreakFlag		;could have been set before, now check
	beq brkabort
	ldy SerialStatus
	bmi exit		; if not, check for error
	cmp #'A'		; command acknowledged?
	beq exit
	cmp #'C'		; command completed
	beq exit
	cmp #'E'		; device error?
	beq err
	ldy #DeviceError	; return unexpected error code error
	Skip2
err:
	ldy #DeviceNak		; preload error code
	Skip2
brkabort:
	ldy #BreakError
	sty SerialStatus
exit:
	tya
	rts
.endproc
;;; *** SetupCommandFrame
;;; *** Compute from the current speed setting and the command frame
;;; *** a speed value and a command frame
.proc	SetupCommandFrame
	clc
	lda SIODeviceId
	tax			; keep me
	adc SIODeviceUnit
	adc #$ff
	sta SIOCmdFrame		; setup the first byte of the command frame:	The target device
	lda SIOCommand
	sta SIOCmdFrame+1	; setup the second byte of the command frame:	The command
	ldy SIOAux1
	sty SIOCmdFrame+2
	ldy SIOAux2
	sty SIOCmdFrame+3	; setup the full command frame:	 AUX1,AUX2

	cpx #$31		; a disk drive?
	bne doslow		; non-disk drives work with regular speed

	ldx CurrentSpeed
	bmi turbosetup
	cpx #Happy810
	beq warpsetup
	cpx #XF551
	beq xf551setup
	cpx #Happy1050
	bne setupdone
	
	;; here Happy1050 setup
	cmp #$48
	bne setupdone		;must use regular speed for 1050 Happy command $48
	beq doslow
	
	;; here XF551 setup
xf551setup:
	ora #$80		;signal a high-speed command by setting bit 7 of the command frame
	sta SIOCmdFrame+1
	cmp #$21+$80		;format
	beq doslow		;is done slow
	cmp #$22+$80		;format extended
	beq doslow
	bne setupdone
	
	;; US turbo setup
turbosetup:
	tya
	ora #$80		;modify the sector to indicate a high-speed command
	sta SIOCmdFrame+3
	bmi setupdone
	
	;; here Warp Setup
warpsetup:
	cmp #'R'
	beq dowarp
	cmp #'P'
	beq dowarp
	cmp #'W'		;only put,write and read are fast
	bne doslow
dowarp:	ora #$20		;signal high-speed by setting bit 5
	sta SIOCmdFrame+1	;of the command
	bne setupdone
doslow:
	lda #RegularSpeed
	sta CurrentSpeed
setupdone:
	rts
.endproc

;;; *** SendBytes
;;; *** High-Speed send bytes. Transmits bytes, then
;;; *** waits for the acknowledgement of the block.
;;; *** Returns with EQ if everything is ok, otherwise with NE
;;; *** The A register contains the flags for the data phase.
;;; *** If A is zero, a single acknowledge is expected (write command frame)
;;; *** If A is 80, a second ack is expected.
;;; *** X and Y are the timeout, y low and x high.	
.proc	SendBytes
	sty SIOTimer		; timeout value: new VBI is not yet installed
	stx SIOTimer+1		; it is harmless to set them now.
	sta SerialNoChkSum	; one-byte indicator for the transmission mode.
	ldy #0
delay:	iny
	bne delay
	
	jsr TransmitBytes
	;; wait until pokey transmitted the last byte of the
	;; data frame this is acknowledged here.
	lda #$8
wtdone:	bit IRQStat		; output buffer done?
	bne wtdone
	;; the 1050 turbo sends the acknowledge in high-speed
	bit CurrentSpeed
	bpl no1050rec
	lda #6			;switch speed for the command acknowledge: This is for the US Turbo
	sta AudFreq2
no1050rec:
	lda #$10
	jsr SetSerialMode	; switch to receive mode
	lda #$3c
	sta PIAPortBCtrl	; clear the COMMAND line	
	jsr Receive		; a single byte
	rts
.endproc
;;; *** SIOTransmit
;;; *** The high-speed replacement of SIO
;;; *** This is the low-level of SIO that performs
;;; *** the actual interaction.
.proc	SIOTransmit
	lda #$01
	;; runs into the following
.endproc
;;; *** SIOTransmitRetry
;;; *** Similar to the above, but with the maximum number of retries in A
.proc	SIOTransmitRetry
	sta SIORetry		; setup retry counter	
	lda #$01
	sta CriticIO		; critical IO startzzs here
	lda SIOSpeed
	sta CurrentSpeed
	;; construct the command frame
	;; handle all the special cases
	jsr SetupCommandFrame
	;; command frame is constructed
	;; can start with the serious work
fullretry:
	lda #$0d
	sta SIOCmdRetry		; retry counter command setup
restart:
	lda BreakFlag
	beq abort		; check whether the user aborted this
	lda CurrentSpeed
	cmp #XF551
	bcc high		; all other drives use a regular speed command frame
	lda #RegularSpeed	; this is for the Happys, US Turbo and XF551: Command frame is regular speed
high:
	jsr AudioInitHi
	lda #$34
	sta PIAPortBCtrl	; lower the COMMAND line
	jsr InitCmdFrameBuffer
	ldy #<2
	ldx #>2			; timeout
	lda #0			; no data phase, no second ack
	jsr SendBytes		; send this over the line
	bpl framefine
errorcmd:
	dec SIOCmdRetry		; retry the serial command, did not work
	bpl restart
	bmi sioerror
framefine:
	;;
	;; the rest expects the data block in high speed
	bit CurrentSpeed
	bvc noxfc
	lda #16			; this is the speed for the Happy810 and XF551: The data frame speed.
	sta AudFreq2
noxfc:
	bit SIOStatus		; reading or writing transmission?
	bpl nosentdata
	;; here writing a data frame
	lda #$0d
	sta SIOCmdRetry		; re-initialize the error counter
	jsr InitBuffer
	jsr ComputeTimeout	; compute the timeout for the receiving data
	lda #$80		; expect a second ack
	jsr SendBytes		; transmit the payload data
	bmi sioerror		; on timeout try try again from the beginning
	bpl exit		; otherwise, write command is done.
	;; otherwise, even on error, wait for the result of the command
nosentdata:
	bit SIOStatus		; now check whether this command expects data in return
	bvs receivedata		; note that the data phase result code is part of the buffer receive here!
sioerror:			; here:	error on transmission
	ldy SerialStatus
	cpy #DeviceNak	  	; no fallback to standard speed on NAK
	beq nonak
	lda #RegularSpeed	; fallback to regular speed on timeout or 'E'.
	sta CurrentSpeed 
nonak:	
	dec SIORetry		; retry all of it?
	bpl fullretry		; redo from start if we have some tries left
	bmi exit		; give up trying
;;; here: receive input data: The block arrives, even if the device signalled an error!
receivedata:			
	jsr ComputeTimeout	; compute the timeout for the receiving data
	lda #$40		; expect an acknowlege and a data phase
	sty SIOTimer
	stx SIOTimer+1
	sta SerialNoChkSum	; one-byte indicator for the transmission mode.
	jsr InitBuffer		; setup the buffer for the payload data
	jsr Receive		; wait for the bytes for arrive: This *also* reads the device acknowledge
	bmi sioerror		; retry on error
exit:	
	ldy SerialStatus	; get the serial status, set N,Z flag
	bne cleanup		; jump always
abort:
	ldy #BreakError		; set error code
	sty BreakFlag		; reset the break key flag
cleanup:
	sty SIOStatus		; set the SIO Status	
	lda #0
	sta CriticIO		; clear the IO critical flag (Y still set, A still set)
	jsr AudioCleanup
	lda #$3c
	sta PIAPortBCtrl	; reset PIA
	tya			; set CPU flags
	rts
.endproc	
;;; *** HispeedBreak
;;; *** This is the shortened version of the break IRQ
;;; *** it also creates an error.
.proc	HispeedBreak
	lda #0
	sta BreakFlag
	;; sta IRQStat   ;clear the interrupt flag, keep break disabled.
	;; NOPE. Ensure we run into the original break when it's over
	ldy #BreakError
	bmi SignalError
.endproc
;;; *** HispeedVBI
;;; *** This is an extremly shortened modified VBI
;;; *** that does not intrude the serial transfer
.proc	HispeedVBI
	dec SIOTimer
	bne notimeout
	dec SIOTimer+1
	bmi timeout
notimeout:
	;; implement the exitVBI
	pla
	tay
	pla
	tax
	pla
	rti
timeout:
	ldy #TimeoutError
	;; runs into the following
.endproc
;;; *** SignalError
;;; *** This removes a six byte command frame
;;; *** and returns to the interrupted method
;;; *** signalling the error in Y.
.proc	SignalError
	ldx SIOStackPtr
	txs
	sty SerialStatus
	jmp SerialRecDone	;return to the caller of the wait.
.endproc
;;; *** SIO
;;; *** This is the high-level entry point for the new SIO
	.global SIO
.proc	SIO
	lda SIODeviceId
	cmp #$31		;only for disk drives
	bne regular
	ldx SIODeviceUnit	;only for units 1 to 8
	beq regular
	cpx #9
	bcs regular
	lda SpeedTab-1,x	;speed for the drive known?
	bne havespeed		;speed known?
	;; SIO speed detection algorithm follows
	lda #RegularSpeed
	sta SIOSpeed		;for the time, use a regular speed.
	ldy #9
setdetect:
	lda SIOCommand,y
	pha
	lda SIOSpeedDetect,y
	sta SIOCommand,y
	dey
	bpl setdetect
	lda #0			;do not retry
	jsr SIOTransmitRetry	;go through low-level SIO
	bmi noultra		;not detected through this command

	lda SpeedTmpBuf		;get the speed detection byte
	cmp #10			;a Happy 1050?
	beq starthappy
	bne setspeed
noultra:
	;; otherwise, try to run a status commando through the high speed SIO and see what we get...
	lda #TurboFlag
	jsr SendSIOStatus	;try this first
	bpl setspeed
	lda #XF551
	jsr SendSIOStatus	;and this....
	bpl setspeed
	lda #Happy810
starthappy:
	ldx #$48		;the Happy command
	ldy #0			;no data phase
	jsr SendSIOShort
	bpl setspeed
	lda #RegularSpeed
setspeed:
	clc
	adc #1
	ldx SIODeviceUnit	;only for units 1 to 8
	sta SpeedTab-1,x	;speed for the drive known?
	;; now restore the SIO command
	ldy #0
rsto:	pla
	sta SIOCommand,y
	iny
	cpy #$a
	bcc rsto
	lda SpeedTab-1,x
	clc
havespeed:
	sbc #0			;nonzero if the speed is known (carry is cleared here)
	sta SIOSpeed		;install the speed. Low level ignores this for everything but disk drives
regular:
	jsr SIOTransmit
	rts
.endproc
;;; *** SendSIOStatus
;;; *** Run a status command through the SIO with the
;;; *** indicated serial speed and check how it goes...
.proc	SendSIOStatus
	ldx #'S'
	ldy #$40		;read command
.endproc
;;; *** SendSIOShort
;;; *** Run a short SIO command for detection purposes
.proc	SendSIOShort
	sty SIOStatus
	stx SIOCommand
	sta SIOSpeed
	ldy #5
setcmd:	lda SIOSpeedStatus,y
	sta SIOBufferLo,y
	dey
	bpl setcmd
	lda #0			;no retries
	jsr SIOTransmitRetry	;low level
	php
	lda SIOSpeed		;restore A
	plp			;restore condition codes
	rts
.endproc
;;; *** SerInIRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerInIRQ
.proc	SerInIRQ
.endproc
;;; *** SerOutRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerOutIRQ
.proc	SerOutIRQ
.endproc
;;; *** SerXmtIRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerXmtIRQ
.proc	SerXmtIRQ
	pla
	rti
.endproc
	
	;; The SIO detect mechanism.
SIOSpeedDetect:
	.byte $3f,$40		;a read command
	.word SpeedTmpBuf,1,1	;timeout, and size
	.word $20		;aux byte
SIOSpeedStatus:
	.word SpeedTmpBuf,1,4	;timeout, and size
