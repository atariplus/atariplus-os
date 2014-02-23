;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: cio.asm,v 1.15 2014/01/19 12:19:57 thor Exp $		**
;;; **									**
;;; ** In this module:	 Central IO functions				**
;;; **********************************************************************
	
	.include "cio.i"
	.include "errors.i"
	
	.segment  "OsHi"
	
;;; *** CIOInit
;;; *** Initialize the IOCBs:
;;; *** Set them all to closed without
;;; *** closing them.
	.global CIOInit
.proc	CIOInit
	ldx #$00		; index into the IOCB
lp:
	lda #$ff
	sta IOCBIndex,x		; declare channel as closed
	lda #<(PutErrorByte-1)	; for RTS, jump address -1
	sta IOCBPut,x		; initialize the put-one-byte to dummy
	lda #>(PutErrorByte-1)
	sta IOCBPut+1,x
	clc
	txa
	adc #$10		; to the next IOCB
	tax
	bpl lp
	rts
.endproc
;;; Dummy entry for the PutOneByte entry
.proc	PutErrorByte
	ldy #ChannelNotOpen
	rts
.endproc
;;; *** CIO
;;; *** Central IO subsystem.
;;; *** This subroutine extracts commands
;;; *** from the IOCB and forwards them
;;; *** to the apropriate handler.
	.global CIO
.proc	CIO
	stx ZIOCB		; keep the IOCB# we need to take
	sta ZIOByte		; keep the byte to transmit
	txa			; check whether the IOCB is valid
	bmi invalidiocb
	and #$0f		; bail out if it is not
	bne invalidiocb
	;; now transmit the channel to the ZIOCB
	tay
tozlp:
	lda IOCBIndex,x
	sta ZIndex,y
	inx
	iny
	cpy #$0c		; do not overwrite the ZPtr
	bcc tozlp
	;; now check the command type
	lda ZCmd		; check whether the command offset is fine
	ldy #InvalidCmd		; if lower than that, then broken
	cmp #CmdOpen		; must be at least open
	bcc error
	cmp #CmdSpecial		; could it be special?
	bcc notspecial
	lda #CmdSpecial		; handle them all by the same vector
notspecial:
	tay			; get the handler address
	lda LowCIOEntries-CmdOpen,y
	sta ZHandlerVec
	lda HiCIOEntries-CmdOpen,y
	sta ZHandlerVec+1
	lda TableOffset-CmdOpen,y ; get the offset from the handler table
	sta ReducedCmd		; keep this for later
	jsr CallVector		; run the corresponding vector
	;; must set the ZStatus here
exit:	
	ldy ZIOCB		; get offset back
	ldx #$00
fromzlp:
	lda ZIndex,x		; copy the data back here
	sta IOCBIndex,y
	inx
	iny
	cpx #$0c
	bcc fromzlp
	lda ZIOByte
	ldx ZIOCB
	ldy ZStatus		; set flag
	rts
invalidiocb:
	ldy #InvalidChannel
	rts
error:
	sty ZStatus
	bmi exit
	;; Call a CIO command vector
CallVector:
	jmp (ZHandlerVec)
.endproc
;;; *** CIOClose
;;; *** Close a CIO Channel
.proc	CIOClose
	ldy #$01		; setup default result code:	fine
	sty ZStatus
	jsr LoadHandlerVec	; get the close vector
	bmi CloseIOCB		; on error:	Mark the vector as free
	jsr RunVector		; run the close vector
.endproc
	;; runs into the following
;;; *** CloseIOCB
;;; *** Mark the IOCB as closed
.proc	CloseIOCB
	lda #<(PutErrorByte-1)
	sta ZPut
	lda #>(PutErrorByte-1)	; install a dummy handler here.
	sta ZPut+1
	lda #$ff		; clean the HaTabs entry
	sta ZIndex		; keep it
	rts
.endproc	
;;; *** CIOOpen
;;; *** Open a CIO Channel
.proc	CIOOpen
	lda ZIndex		; get the handler index:	must be absent, or the thing is open
	cmp #$ff		; if not, open error
	beq channelfree
	ldy #ChannelInUse
	bmi ErrorRts		; return an error condition
channelfree:
	jsr SearchDevice	; try to locate the device here.
	bcs ErrorRts		; branch out on device not found
	;; Setup the "put one byte" vector first.
	;; this allows the handler to modify the vector
	;; in the open vector to work around programs
	;; using it (BASIC)
	;; The ReducedCmd contains the Handler Put vector offset already
	jsr LoadHandlerVec	; load the vector again
	bmi ErrorRts
	lda ZHandlerVec
	sta ZPut		; keep it
	lda ZHandlerVec+1
	sta ZPut+1		; ditto
	;; Now perform the real handler open
	lda #HandlerOpen	; open is at offset 0
	sta ReducedCmd
	jsr LoadHandlerVec	; load the vector for the jump
	bmi ErrorRts
	jsr RunVector		; run the open vector of the device
	;; bmi CloseIOCB	; interestingly, the XL os leaves the channel open...
	rts
.endproc
;;; *** CIOStatus and CIOSpecial
;;; *** handle Status/Special
;;; *** does both in one.
.proc	CIOStatusSpecial
	ldy ZIndex		; check whether we are open
	bpl isopen
	jsr SearchDevice	; if not, open the vector
	bcs ErrorRts		; on error, exit
isopen:
	jsr LoadHandlerVec	; open the vector towards the device:	since we opened, this cannot fail
	jsr RunVector		; run the status or special vector, will set the result code
	ldx ZIOCB		; restore the open flag from the IOCB
	lda IOCBIndex,x
	sta ZIndex		; this will "re-close" the channel if it was open before.
	rts
.endproc
;;; *** ErrorRts
;;; *** Store the error code in Y and
;;; *** return with RTS
.proc	ErrorRts
	sty ZStatus
	rts
.endproc
;;; *** CIOGetBlock vector
;;; *** Read a block from outside, used by the FMS
.proc	CIOGet
	lda ZCmd
	and ZAux1		; check whether the channel is open for reading
	bne isvalid
	ldy #OutputOnly		; signal an error
	bmi ErrorRts
isvalid:
	jsr LoadHandlerVec	; get the vector of the handler
	bmi ErrorRts		; signal an error in case there is no data
	jsr TestForZero		; now check whether the length is zero
	bne readloop
	jsr RunVector		; run the device vector
	sta ZIOByte		; keep the result, installed Y in ZStatus
	rts
readloop:			; get a block or a record
	jsr RunVector		; get the byte
	sta ZIOByte		; keep it
	bmi endblock		; branch on error
	ldy #$00		; now store the data
	sta (ZAdr),y	
	jsr IncBuffer		; advance the pointer	
	lda ZCmd		; now check the type of the get command
	lsr a
	lsr a			; record or block? -> carry (set on blockIO)
	jsr DecLength		; ok, count down the buffer
	beq end			; bail out if end of buffer detected
	bcs readloop		; block IO->continue
	lda ZIOByte		; now check whether we had an EOL here
	cmp #$9b		; ended? (sets carry if so)
	bne readloop		; if not, continue
end:
	;; here: buffer end detected. Check whether we read blocky or unblocky
	bcs endblock		; block IO -> abort
	;; here: record full, but not yet reached its end. Skip to its end manually
	lda ZIOByte
	bcc eoltest
skiploop:
	jsr RunVector		; and continue until either 
	bmi errskip		; we find an error
	ldy #RecordTooLong	; indicate that the record did not fit
	sty ZStatus		; store the result	
eoltest:
	cmp #$9b		; or EOL
	bne skiploop
errskip:
	;; now store an EOL at the end of the record
	jsr DecBuffer
	ldy #$00
	lda #$9b
	sta (ZAdr),y
	jsr IncBuffer
endblock:			; end of block transfer
	jmp RemainingBytesReset	; insert the remaing bytes into the ZIOCB and reset the buffer
.endproc
;;; *** CIOPut vector
;;; *** Write blocks and records
.proc	CIOPut
	lda ZCmd
	and ZAux1		; check whether the channel is open for reading
	bne isvalid
	ldy #InputOnly		; signal an error
error:	
	sty ZStatus		; install the error code
	rts
isvalid:
	jsr LoadHandlerVec	; get the put vector
	bmi error		; signal on error
	jsr TestForZero		; is there a buffer we put from?
	bne writeloop
	inc ZLen		; single byte
	bne bytefetched		; which is in ZIOByte already
writeloop:
	ldy #$00
	lda (ZAdr),y		; read the byte from memory
	sta ZIOByte
bytefetched:			; branched in from above
	jsr RunVector		; put the byte over the line. 
	bmi endblock		; abort immediately on error (The erraneous byte has not been written!) 
	jsr IncBuffer		; advance the buffer pointer
	lda ZCmd		; get the last command
	lsr a			; check whether we are blocky
	lsr a			; into the carry bit
	jsr DecLength		; count down the length
	bcs isblock
	lda ZIOByte		; check whether this is an EOL
	eor #$9b
	beq endblock		; do not output a second EOL
	jsr TestForZero
isblock:			; here:	check whether we reached the end of the block
	bne writeloop		; if so, continue
end:	
	bcs endblock		; check whether this was a block command
	lda #$9b		; terminate explicitly by an EOL
	sta ZIOByte
	jsr RunVector
endblock:
	jmp RemainingBytesReset
.endproc
;;; *** CIO misc. service routines follow
;;; *** IncBuffer: Increment the buffer pointer
.proc	IncBuffer
	inc ZAdr
	bne nocarry
	inc ZAdr+1
nocarry:	
	rts
.endproc
;;; *** DecBuffer: Decrement the buffer pointer
.proc	DecBuffer
	lda ZAdr
	bne nocarry
	dec ZAdr+1
nocarry:
	dec ZAdr
	rts
.endproc
;;; *** DecLength: Decrement the buffer length
;;; *** Return with Z flag set if the length
;;; *** is zero.
.proc	DecLength
	lda ZLen		; decrement the length
	bne nocarry
	dec ZLen+1
nocarry:
	dec ZLen
	;; runs into the following
.endproc
;;; *** TestForZero
;;; *** Check whether the length is zero. Returns
;;; *** zero if so.
.proc	TestForZero
	lda ZLen+1
	ora ZLen		; check for zero
	rts
.endproc
;;; *** RemainingBytesReset:
;;; *** Compute the number of bytes remaining in the buffer
;;; *** and reset the buffer to its beginning.
;;; *** This is done after read/write blocks
;;; *** such that the CIO end routine writes the
;;; *** start of the buffer back.
.proc	RemainingBytesReset
	ldx ZIOCB
	sec
	lda IOCBLen,x
	sbc ZLen
	sta ZLen
	lda IOCBLen+1,x
	sbc ZLen+1
	sta ZLen+1
	
	lda IOCBAdr,x
	sta ZAdr
	lda IOCBAdr+1,x
	sta ZAdr+1
	rts
.endproc
;;; *** LoadHandlerVec:	Get the address for an indirect jump for
;;; *** the reduced handler command found in "ReducedCommand"
;;; *** Returns with C set on error and error code in Y, does
;;; *** not set ZStatus.
.proc	LoadHandlerVec
	ldy ZIndex		; are we really open
	bmi notopen
	lda HaTabs+1,y		; get handler table, lo
	sta ZHandlerVec
	lda HaTabs+2,y		; get hi
	sta ZHandlerVec+1
	ldy ReducedCmd		; get offset in the handler table
	lda (ZHandlerVec),y	; keep lo
	pha
	iny
	lda (ZHandlerVec),y	; keep hi
	sta ZHandlerVec+1
	pla
	sta ZHandlerVec
	ldy #1			; not an error
	rts
notopen:
	ldy #ChannelNotOpen	; return an error
	rts
.endproc
;;; *** SearchDevice:	Find a device
;;; *** within the HaTabs.
;;; *** Does not touch X, sets also the unit number
;;; *** sets C on error and returns the error code in Y
;;; *** then. Does *NOT* set ZStatus
.proc	SearchDevice
	ldy #$00
	lda (ZAdr),y		; get the address of the device
	beq notfound		; NUL Is not a valid device name since it terminates HATABS
	ldy #$21		; last possible HaTabs entry
slop:
	cmp HaTabs,y		; found the device?
	beq found
	dey
	dey
	dey
	bpl slop
notfound:
	;; here:	 not found
	ldy #UnknownDevice
	sec			; indicate error somehow
	rts
found:
	sty ZIndex		; store as HaTabs offset now
	ldy #$01		; check whether we have a unit number
	lda (ZAdr),y
	cmp #'1'		; must be higher or equal than this
	bcc nounit
	cmp #'9'+1		; and lower than this
	bcc haveunit
nounit:
	lda #'1'		; default unit is one
haveunit:
	sec
	sbc #'0'		; map unit '1' to number 1
	sta ZUnit		; store as unit number
	clc
	rts
.endproc
;;; *** RunVector
;;; *** Run the handler vector stored in 
;;; *** ZHandlerVec with the value in 
;;; *** ZIOCB. Installs the result in ZStatus
.proc	RunVector
	ldy #UnsupportedCmd	; default return value
	jsr CallVec
	sty ZStatus		; keep result code
	cpy #$00		; set the flags
	rts
CallVec:
	lda ZHandlerVec+1	; push hi
	pha
	lda ZHandlerVec		; push lo
	pha
	lda ZIOByte		; get the byte to transfer
	ldx ZIOCB		; restore the channel offset
	rts
.endproc	
;;; The following table maps the command introducer into the
;;; index of the handler table
.proc	TableOffset
	.byte HandlerPut	; the open command has a separate vector, but we start it as Put to get PutOneByte
	.byte HandlerGet,HandlerGet,HandlerGet,HandlerGet ; all get commands share the same vector
	.byte HandlerPut,HandlerPut,HandlerPut,HandlerPut ; so do all put commands
	.byte HandlerClose,HandlerStatus,HandlerSpecial
.endproc
;;; This table maps the CIO commands to the handler entries
.proc	LowCIOEntries
	.byte <CIOOpen
	.byte <CIOGet,<CIOGet,<CIOGet,<CIOGet
	.byte <CIOPut,<CIOPut,<CIOPut,<CIOPut
	.byte <CIOClose,<CIOStatusSpecial,<CIOStatusSpecial
.endproc
.proc	HiCIOEntries
	.byte >CIOOpen
	.byte >CIOGet,>CIOGet,>CIOGet,>CIOGet
	.byte >CIOPut,>CIOPut,>CIOPut,>CIOPut
	.byte >CIOClose,>CIOStatusSpecial,>CIOStatusSpecial
.endproc
