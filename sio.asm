;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: sio.asm,v 1.16 2014/03/15 00:39:16 thor Exp $		**
;;; **									**
;;; ** In this module:	 Serial IO interface				**
;;; **********************************************************************

	.include "sio.i"
	.include "pokey.i"
	.include "pia.i"
	.include "nmi.i"
	.include "irq.i"
	.include "errors.i"
	.include "kernel.i"
	.include "misc.i"
	
	.segment  "OsHi"
	
;;; *** Skip two bytes (by a dummy BIT)
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
	sta PIAPortACtrl	; reset PIA ports
	sta PIAPortBCtrl	; ditto
	lda #$03
	sta SkStatShadow
	sta SkStat		; reset pokey
	sta SerialSound		; enable sound output
	rts
.endproc
;;; *** SIO
;;; *** This is the general serial input/output routine
;;; *** sed for all communications across the serial
;;; *** port.
	.global SIO
.proc	SIO
	lda #$01
	sta CriticIO		; critical IO starts here
	tsx			; keep stack pointer for direct return
	stx SIOStackPtr
	lda #$01
	sta SIORetry		; setup retry counter	
	sta SIOStatusTmp	; reset the error store
	clc
	lda SIODeviceId
	adc SIODeviceUnit
	adc #$ff
	sta SIOCmdFrame		; setup the first byte of the command frame:	The target device
	lda SIOCommand
	sta SIOCmdFrame+1	; setup the second byte of the command frame:	The command
	lda SIOAux1
	sta SIOCmdFrame+2
	lda SIOAux2
	sta SIOCmdFrame+3	; setup the full command frame:	 AUX1,AUX2
fullretry:
	lda #$0d
	sta SIOCmdRetry		; retry counter command setup
restart:
	ldy #<SIOCmdFrame
	lda #>SIOCmdFrame
	ldx #4
	jsr InitShortBuffer	; setup the serial buffer we have to transmit
	lda #$34
	sta PIAPortBCtrl	; lower the COMMAND line
	jsr SendBytes		; send this over the line
	bpl framefine		; transmitted ok
	dec SIOCmdRetry		; retry the serial command, did not work
	bpl restart
	bmi sioerror		; retry completely.
framefine:	
;;; here: check whether we need to write out the data frame
	bit SIOStatus
	bpl nosentdata
	jsr InitBuffer		; setup the buffer pointers
	jsr SendBytes		; transfer the data, already generates an acknowledge
	bpl nosentdata		; done if no error
	cpy #TimeoutError
	beq sioerror		; on timeout, do not try again
	sty SIOStatusTmp	; keep the error code here.
;;; here: receive the result byte of the write or read operation. Even if the write data phase aborted.
nosentdata:
	jsr ComputeTimeout	; compute the timeout for the receiving data
	jsr ReceiveResult	; check the result code of the device
	bpl fine
	cpy #TimeoutError	; on timeout, do not try to receive the data frame
	beq sioerror		; but abort immediately
	sty SIOStatusTmp	; keep the error 
fine:	
	bit SIOStatus		; now check whether this command expects data in return
	bvc check		; if aborting, might still be that there is a former error
;;; here: receive input data: Note this data is received even if the device signalled an error
	jsr InitBuffer		; setup the buffer again
	jsr ReceiveBytes	; wait for the bytes for arrive
	bmi sioerror		; on error, surely try again
check:				; last phase was ok, but probably not the phase before
	ldy SIOStatusTmp	; but could be that the data acknowledge phase created an error
	sty SerialStatus	; retain that
	bpl exit		; if that is also ok, return
sioerror:			; here:	 SIO on error
	dec SIORetry		; retry all of it?
	bpl fullretry		; redo from start if we have some tries left
exit:	
	ldy SerialStatus	; get the serial status, set N,Z flag
	jmp Exit		; bail out
.endproc
;;; InitBuffer:
;;; Initialize the serial transfer buffers
;;; from the SIO command vector
.proc	InitBuffer
	lda SIOBufferLo
	sta SerBufLo
	clc
	adc SIOSizeLo
	sta SerBufEndLo
	lda SIOBufferHi
	sta SerBufHi
	adc SIOSizeHi
	sta SerBufEndHi
	rts
.endproc
;;; *** InitShortBuffer
;;; *** Initialize the serial data buffer to Y=lo,A=hi,length=X
.proc	InitShortBuffer
	sty SerBufLo
	sta SerBufHi
	txa
	clc
	adc SerBufLo
	sta SerBufEndLo
	lda SerBufHi
	adc #$00
	sta SerBufEndHi
	rts
.endproc
;;; *** SendBytes
;;; *** Transmit bytes over the serial port,
;;; *** as indicated by the SerBufLo/Hi pointers
.proc	SendBytes
	;; a delay loop to allow the serial port to rest and to allow the receiver to recognize the
	;; CMD line pull.
	ldy #$00
deyloop1:	
	dey
	bne deyloop1
	jsr TransmitBytes
	ldy #<2
	ldx #>2			; VBI timeout count
	;jsr ReceiveResult	; and now expect the result of all this
	;rts
	;; runs into the following
.endproc
;; ReceiveResult:
;; expect an answer from the device concerning the
;; transfer phase. Returns with Z set on a timeout.
;; expects the timeout value in X (hi), Y (lo)
.proc	ReceiveResult	
	lda #<TimeOut
	sta VecVBITimer0
	lda #>TimeOut
	sta VecVBITimer0+1
	lda #$01
	sta SIOTimerFlag
	jsr SetIRQVector	; install the error handler for the serial timeout
	ldy #$ff
	sty SerialNoChkSum	; do not expect a checksum for the single byte
	ldy #<SIOAck
	lda #>SIOAck
	ldx #1
	jsr InitShortBuffer	; expect a single byte at SIOAck
	jsr ReceiveBytes	; and receive what we get
	bmi exit		; on error, set directly
	lda SIOAck		; get device result code
	cmp #'A'		; command acknowledged?
	beq exit
	cmp #'C'		; command completed
	beq exit
	cmp #'E'		; device error?
	beq setnak
	ldy #DeviceError	; return unexpected error code error
	Skip2
setnak:
	ldy #DeviceNak
	sty SerialStatus	; keep the status code here
exit:	
	rts
TimeOut:
	lda #$00
	sta SIOTimerFlag	; clear the timer flag. What a waste for this timer!	
	lda #TimeoutError
	sta SerialStatus	; setup the error code then
	rts
.endproc
;;; *** TransmitBytes
;;; *** Transmit a couple of bytes, expect timeout and
;;; *** stuff like that.
.proc	TransmitBytes
	lda #$01
	sta SerialStatus	; clear the status flags
	jsr InitForSend		; setup pokey for sending data
	ldy #$00
	sty SerialSentDone
	sty SerialChkSumDone	; clear up the flags
	lda #$08
	jsr AbleIRQ		; enable the Xmt Done IRQ thus triggering the output
waitloop:			; the remaining stuff is done by the interrupt
	lda BreakFlag		; has Break been hit?
	beq Abort
	lda SerialSentDone	; serial write ready?
	beq waitloop
	jsr AudioCleanup
	rts
.endproc
;;; *** ReceiveBytes
;;; *** Receive bytes from the serial
;;; *** device back
.proc	ReceiveBytes
	ldy #$00
	sty SerialChkSum	; clear the checkum
	sty SerialDataDone	; data has not yet been received
	sty SerialXferDone	; read transfer still waits to be finished
	iny
	sty SerialStatus	; set the status to OK
	jsr InitForReceive	; setup for data receive
	lda #$3c
	sta PIAPortBCtrl	; clear the COMMAND line
waitloop:
	lda BreakFlag		; has break been hit?
	beq Abort		; if so, abort here immediately
	lda SIOTimerFlag	; timer run out?
	beq exit		; timeout error has been set then already
	lda SerialXferDone	; transfer completed?
	beq waitloop		; if not, continue looping
exit:
	ldy SerialStatus	; return the status and set the flags
	rts
.endproc
;;; *** Abort
;;; *** Abort a serial transfer because the Break
;;; *** key has been hit
.proc	Abort
	ldy #BreakError		; set error code
	sty BreakFlag		; reset the break key flag
	sty SerialStatus	; keep the error also here for consistency
	;; runs into the following
.endproc
;;; Exit
;;; Clean the Pokey and serial registers after a SIO operation
;;; Y is the error code, and bail out of SIO
.proc	Exit
	lda #0
	sta CriticIO		; clear the IO critical flag (Y still set, A still set)
	jsr AudioCleanup
	sty SIOStatus		; set the SIO Status	
	ldx SIOStackPtr		; reload stack pointer
	txs			; and set it	
	lda #$3c
	sta PIAPortBCtrl	; ditto
	cli			; release interrupt blocking
	cpy #$00		; set CPU flags
	rts
.endproc
;;; *** SerInIRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerInIRQ
.proc	SerInIRQ
	tya
	pha			; keep Y register
	lda #$20		; overrun error?
	bit SkStat		; get serial status
	sta SkReset		; clear status (does not set flags)
	bmi noframeerror	; a framing error?
	ldy #FrameError	
	sty SerialStatus	; signal the error (also: clear Z flag)
noframeerror:
	bne nooverrun
	ldy #OverrunError
	sty SerialStatus	; signal the error
nooverrun:
	lda SerialDataDone	; serial buffer is filled?
	bne datadone
	ldy #$00		; no. Data follows
	lda SerDat
	clc
	sta (SerBufLo),y	; store the data
	adc SerialChkSum	; add up the checksum
	adc #$00		; add up the carry
	sta SerialChkSum	; ditto
	inc SerBufLo		; increment the buffer pointer
	bne nocarry
	inc SerBufHi
nocarry:
	lda SerBufLo
	cmp SerBufEndLo
	lda SerBufHi
	sbc SerBufEndHi		; are we done?
	bcc exit		; done?
	lda SerialNoChkSum	; do we need a checksum?
	beq needchksum
	sty SerialNoChkSum	; clear the checksum flag (does not set Z flag)
	bne xferdone		; the transfer is done
needchksum:
	dec SerialDataDone	; serial data is now done, await checksum
	bne exit		; clears Z flag
datadone:	
	lda SerDat		; here:	 is done. Test the checksum
	cmp SerialChkSum
	beq xferdone
	ldy #ChkSumError	; signal a checksum error
	sty SerialStatus
xferdone:	
	dec SerialXferDone	; serial transfer done
exit:		
	pla
	tay
	pla
	rti
.endproc
;;; *** SerOutRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerOutIRQ
.proc	SerOutIRQ
	inc SerBufLo		; next byte. The first has been sent manually
	bne nocarry
	inc SerBufHi
nocarry:
	lda SerBufLo
	cmp SerBufEndLo
	lda SerBufHi
	sbc SerBufEndHi
	bcc senddata
	lda SerialChkSumDone
	bne sumdone
	lda SerialChkSum	; get the checksum
	sta SerDat		; write it out
	dec SerialChkSumDone	; note that we're done with it
	bne exit		; done with it
sumdone:			; serial checksum has been transfered, we're done
	lda #$08
	jsr AbleIRQ		; disable this IRQ, enable XmtDone
	bne exit
senddata:			; carry is clear here
	tya
	pha	
	ldy #$00		; carry is clear here
	lda (SerBufLo),y	; get data
	sta SerDat		; write out data
	adc SerialChkSum
	adc #$00
	sta SerialChkSum
	pla
	tay
exit:	
	pla
	rti	
.endproc
;;; *** SerXmtIRQ
;;; *** The os supplied default serial input handler
;;; *** called on a serial shift register full
	.global SerXmtIRQ
.proc	SerXmtIRQ
	lda SerialChkSumDone	; checksum done already?
	beq start		; if not so, then why are we here? Could be the start of a transfer
	sta SerialSentDone	; xfer is done here
	lda #$00
	jsr AbleIRQ		; disable all IRQs here
	pla
	rti
;; test whether this starts a serial transfer now
start:
	lda SerialSentDone	; are we done already?
	bne exit
	tya
	pha
	lda #$10		; disable this interrupt, enable SerOut
	jsr AbleIRQ
	ldy #$00
	lda (SerBufLo),y	; get data
	sta SerDat		; write out data
	sta SerialChkSum	; keep the checksum
	pla
	tay
exit:
	pla
	rti
.endproc
	
