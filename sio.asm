;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: sio.asm,v 1.8 2013/06/02 20:41:07 thor Exp $		**
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
	
	.segment  "OsHi"
	
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
	beq errorcmd		; timeout?
	lda SIOError		; an error on the serial bus?
	beq framefine
errorcmd:
	dec SIOCmdRetry		; retry the serial command, did not work
	bpl restart
	bmi sioerror
framefine:	
;;; here: check whether we need to write out the data frame
	bit SIOStatus
	bpl nosentdata
	lda #$0d
	sta SIOCmdRetry		; re-initialize the error counter
	jsr InitBuffer		; setup the buffer pointers
	jsr SendBytes		; transfer the data
	beq sioerror		; return on error
nosentdata:
	lda #$00
	sta SIOError		; clear the error code again
	jsr ComputeTimeout	; compute the timeout for the receiving data
	jsr ReceiveResult	; check the result code of the device
	beq xmiterror		; branch if failure:	timeout!
	
	bit SIOStatus		; now check whether this command expects data in return
	bvs receivedata
	lda SIOError		; if not, check for errors now
	bne sioerror		; return on error now
	beq exit		; otherwise, return without error
;;; here: receive input data
receivedata:			
	jsr InitBuffer		; setup the buffer again
	jsr ReceiveBytes	; wait for the bytes for arrive
	ldy SerialStatus	; get the status code
	lda SIOError		; check for the error code again
	beq norecerror
xmiterror:
	ldy SIOStatusTmp	; retain the old error code on error
	sty SerialStatus
norecerror:
	cpy #$01		; if fine, exit
	beq exit
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
deyloop2:
	dey
	bne deyloop2
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
	ldy #$00
	sty SIOError		; clear the error code
	dey
	sty SerialNoChkSum	; do not expect a checksum for the single byte
	ldy #<SIOAck
	lda #>SIOAck
	ldx #1
	jsr InitShortBuffer	; expect a single byte at SIOAck
	jsr ReceiveBytes	; and receive what we get
	ldy #$ff		; status:	OK
	lda SerialStatus
	cmp #$01		; is it fine?
	bne recerror		; if not, check for error
	ldx SIOAck		; get device result code
	cpx #'A'		; command acknowledged?
	beq exit
	cpx #'C'		; command completed
	beq exit
	lda #DeviceNak		; preload error code
	cpx #'E'		; device error?
	beq setdeverror
	lda #DeviceError	; return unexpected error code error
setdeverror:
	sta SerialStatus	; keep the status code here
recerror:
	cmp #TimeoutError	; is it a timeout error?
	beq handletimeout
	dec SIOError		; otherwise, no timeout: set real error code now
	bne exit
handletimeout:
	ldy #$00		; handle a timeout by returning Z set
exit:
	sta SIOStatusTmp	; keep the error code here for latter reference
	tya			; set status flags
	rts
TimeOut:
	lda #$00
	sta SIOTimerFlag	; clear the timer flag. What a waste for this timer!	
	lda #TimeoutError
	sta SerialStatus	; setup the error code then
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
	;; runs into the following	(Audio register cleanup)
.endproc
;;; Cleanup
;;; Clean the Pokey and serial registers after a SIO operation
.proc	Cleanup
	lda #$00
	jsr AbleIRQ		; disable all serial interrupts
	lda #$00
	sta AudCtrl0
	sta AudCtrl1
	sta AudCtrl2
	sta AudCtrl3		; clear pokey audio:	 Hmm, less would be possible
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
	jsr Cleanup
	sta CriticIO		; clear the IO critical flag (Y still set, A still set)
	sty SIOStatus		; set the SIO Status	
	ldx SIOStackPtr		; reload stack pointer
	txs			; and set it	
	lda #$3c
	sta PIAPortACtrl	; reset PIA ports
	sta PIAPortBCtrl	; ditto
	cli			; release interrupt blocking
	cpy #$00		; set CPU flags
	rts
.endproc
;;; InitForSend
;;; Setup pokey such that we can send bytes
;;; This is also available as a kernel vector
	.global InitForSend
.proc	InitForSend
	jsr AudioInit
	lda #$07
	and SkStatShadow
	ora #$20
	sta SkStatShadow
	sta SkStat
	sta SkReset		; reset pokey, setup serial transfer mode
	lda #$10		; serout IRQ on
	bne AbleIRQ		; for compatibility, required by misc. Not required by SIO. Jumps always
.endproc
;;; InitForReceive
;;; Setup pokey such that we can receive bytes
.proc	InitForReceive
	jsr AudioInit
	lda #$07
	and SkStatShadow
	ora #$10
	sta SkStatShadow
	sta SkStat
	sta SkReset		; reset pokey, setup serial transfer mode
	lda #$20		; serin IRQ on
	;; runs into the following
.endproc
;;; AbleIRQ
;;; Enable the pokey IRQ given in A
	.global	AbleIRQ
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
;;; AudioInit
;;; Initialize Pokey Audio for
;;; serial transfer
.proc	AudioInit
	lda #$28		; setup pokey
	sta AudFreq2
	lda #$00
	sta AudFreq3		; approximately 19.200 baud, I suppose	
	lda #$28
	sta AudioCtrl		; setup the pokey mode
	lda #$a8		; medium loud?
	ldy SerialSound		; or off?
	bne loud
	lda #$a0		; no sound
loud:
	sta AudCtrl3
	lda #$a0
	sta AudCtrl0
	sta AudCtrl1
	sta AudCtrl2
	rts
.endproc
	
