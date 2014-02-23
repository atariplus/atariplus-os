;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: 850.asm,v 1.15 2013/06/02 20:41:03 thor Exp $		**
;;; **									**
;;; ** In this module:	The 850 handler, resident part			**
;;; ** this would be part of the 850 interface box			**
;;; **********************************************************************

	.include "kernel.i"
	.include "errors.i"
	.include "cio.i"
	.include "sio.i"
	.include "irq.i"
	.include "pokey.i"
	.include "diskinterf.i"
	.include "reset.i"
	
	.segment  "Handler"

	;; Zero-Page locations we hijack
	;; this can be done since SIO must remain inactive anyhow
	ReadIn	=	$32
	ReadOut	=	$34
	ReadLen	=	$36
;;; ***
;;; *** Useful macro definitions
;;; ***
;;; *** Skip two bytes (by a dummy BIT)
.macro	Skip2
	.byte $2c
.endmacro
.macro	Skip1
	.byte $24
.endmacro

FirstHandlerByte:
;;; *** Main:
;;; *** Here does the boot code launch us
.proc	Main
InitJump:
	jsr InitSerial		; initialize R:	first, do not call thru old DosIni
	bcs exit

	lda DosInit		
	sta OldInit
	lda DosInit+1
	sta OldInit+1
	
	lda ResetVector		; get the jump target from here
	sta DosInit
	lda ResetVector+1
	sta DosInit+1		; we must not modify the BootFlag here since the Os increments(!) it
	
	jsr ReserveMemory	; reserve memory for the handler
	clc
exit:
	rts
.endproc
;;; *** VSerIn
;;; *** The new serial in IRQ
.proc	VSerIn
	lda BreakFlag		; interrupt reading?
	beq DisableTransfer
	tya
	pha
	ldy #$0
	lda SerDat
	sta (ReadIn),y		; read data
	lda SkStat		; read state
	sta SkReset
	eor #$ff
	and #$c0
	ora ErrorFlag
	sta ErrorFlag		; keep errors
	;; now increment the input buffer
	pla
	tay
	txa
	pha
	ldx #$00
	jsr AdvanceInput	; advance the input buffer
	jsr CheckBuffer		; test for buffer overrun
	bne inclen
	ldx #ReadOut-ReadIn	; also increment the read-out position now
	jsr AdvanceInput
	lda ErrorFlag
	ora #$10		; set the buffer-overrun error
	sta ErrorFlag
	bne exit
inclen:
	inc ReadLen		; increment now the buffer size
	bne nocarry
	inc ReadLen+1
nocarry:
exit:
	pla
	tax
	pla
	rti
.endproc
;;; *** VSerOc
;;; *** Interrupt vector called on output complete.
;;; *** Also used as the transmission method for two
;;; *** stop bits (this interrupt comes later)
.proc	VSerOc
	;; check whether there is still data in the buffer
	;; to be transmitted. If so, transmit now
	lda WriteOut
	cmp WriteIn
	bne WriteByteIRQ
	;; if not, the last data byte has been send and we
	;; pause down.
	lda #$e7
	bne DisableIRQ
.endproc
;;; *** VSerOr
;;; *** Interrupt vector called on output ready.
.proc	VSerOr
	lda WriteOut
	cmp WriteIn
	bne WriteByteIRQ
	lda #$ef		; disable VSerOr write it out (but let VSerOc happen)
	;; runs into the following
.endproc
;;; *** DisableIRQ
;;; *** disable the IRQ given the mask in A
.proc	DisableIRQ
	and IRQStatShadow
	sta IRQStatShadow
	sta IRQStat
	pla
	rti
.endproc
;;; *** DisableTransfer
;;; *** Abort the transfer completely if break has been hit
.proc	DisableTransfer
	lda #$c7
	bne DisableIRQ
.endproc
;;; *** WriteByteIRQ
;;; *** Write the byte in the buffer out to the serial port
;;; *** private within the IRQ handler
.proc	WriteByteIRQ	
	lda BreakFlag
	beq DisableTransfer
	tya
	pha
	ldy WriteOut
	lda OutputBuffer,y
	sta SerDat
	iny
	tya
	and #$1f
	sta WriteOut
	dec WriteLen		; reduce the output buffer size
	pla
	tay
	pla
	rti
.endproc
;;; *** CheckUnit
;;; *** Check whether the unit number of the R:	handler is fine
.proc	CheckUnit
	ldx ZUnit
	cpx #1			; only unit 1 is support
	beq isfine
	pla
	pla			; drop the return address
	ldy #IllegalUnit
isfine:	
	rts
.endproc
;;; *** ComputeParity
;;; *** Compute the parity bit of A, 
;;; *** place result into the C bit
.proc	ComputeParity
	ldx #$00
lp:	;; compute the number of one-bits
	lsr a
	bcc nobit
	inx			; count the bit
nobit:
	cmp #$00
	bne lp			; if there are no bits left, copy over
	txa			; get the bitcount->A
	lsr a			; parity -> C
	rts
.endproc
;;; *** CheckBuffer
;;; *** Check wether input and output pointer are
;;; *** identically. Return EQ if so.
.proc	CheckBuffer
	lda ReadIn
	cmp ReadOut
	bne exit
	lda ReadIn+1
	cmp ReadOut+1		; wait until there is something in the buffer
exit:
	rts
.endproc
;;; *** AdvanceInput
;;; *** Advance the input buffer pointer at
;;; *** ReadIn + X by one, possibly wrap around
.proc	AdvanceInput
	inc ReadIn,x
	bne nocarry
	inc ReadIn+1,x
nocarry:
	;; check whether we are at the buffer end
	lda ReadIn,x
	cmp ReadEnd
	lda ReadIn+1,x
	sbc ReadEnd+1
	bcc nowrap
	;; here wrap around-case
	lda ReadStart
	sta ReadIn,x
	lda ReadStart+1
	sta ReadIn+1,x
nowrap:
	rts
.endproc
;;; *** ShortSerial
;;; *** Transmit a short serial status command without
;;; *** a data phase with AUX1,2 copied over from CIO
;;; *** and the command in A
.proc	ShortSerial
	ldy ZAux1
	sty SIOAux1
	ldy ZAux2
	sty SIOAux2
	ldy #$00		; no data phase
	;; runs into the following
.endproc
;;; *** SerialInterf
;;; *** Interface of the R: handler towards SIO
;;; *** Expects the command in A, direction in Y
.proc	SerialInterf
	bit ConFlag		; if the concurrent mode is on, we cannot use SIO
	bvc concurrent
	sta SIOCommand
	sty SIOStatus
	ldx #$01		; initialize the unit
	stx SIODeviceUnit
	ldy #$50		; the device
	sty SIODeviceId
	lda SerialTimeout	; get the timeout
	sta SIOTimeout
	jmp SIOVector
concurrent:
	ldy #InConcurrent	; cannot use SIO, concurrent mode active.
	rts
.endproc
;;; *** LoadBuffer
;;; *** Initialize the SIO pointers
;;; *** to transmit the internal buffer
.proc	LoadBuffer
	lda OutputBufferPtr
	sta SIOBufferLo
	lda OutputBufferPtr+1
	sta SIOBufferHi
	lda #<64		; transmit 64 bytes at once. Strange, but that's it...
	sta SIOSizeLo
	lda #>64
	sta SIOSizeHi
	sta SIOAux2		; no AUX2 (would not matter anyhow)
	lda WriteIn
	sta SIOAux1
	rts
.endproc
;;; *** ROpen
;;; *** Open the R handler
.proc	ROpen
	jsr CheckUnit		; we only support unit one
	lda OpenMode		; check whether we are open
	bpl channelused
	ldy ZAux1		; get the open mode
	tya			; check whether we are either opened for read or write
	and #$0c
	beq invalidmode
	tya			; keep the open mode
	sta OpenMode
	ldy #$ff
	sty ConFlag		; reset the concurrent flag
	sty ParityMask		; no parity
	lda #' '
	sta InvReplace		; Character to be filled in on invalid ASCII input
	iny			; to zero
	sty WriteIn		; clear the fill-in position for the output buffer
	sty ErrorFlag
	sty StopBits		; one stop bit
	sty ChrTrans	
	sty WidthMask		; eight bits
	sty SIOAux1		; clear AUX for what follows
	sty SIOAux2		; ditto
	lda #'W'		; make a dummy write
	jsr SerialInterf	; to disable a possibly set concurrent mode
	bmi exit
	dey			; Y should have been one
	lda #'B'		; reset to 300 baud, eight bits, 1 stop bit, no handshaking
	jsr SerialInterf
	bmi exit
	lda #$ff
	sta SIOAux1		; reset DTR,RTS,XMT to all one (i.e. signal that we are ready)
	dey			; Y should have been one
	lda #'A'
	jsr SerialInterf
	bmi exit
	Skip2
channelused:
	ldy #SerialBusy
	Skip2
invalidmode:
	ldy #InvalidMode
exit:	
	rts
.endproc
;;; *** RClose
;;; *** Close a channel to the R: handler
.proc	RClose
	ldy #$01
	lda OpenMode		; channel used?
	bmi exit		; if not, bail out now
	bit ConFlag		; open for concurrent mode?
	bvc bufferclean		; output buffer not used
	;; here it is
	lda WriteIn		; do we have anything in our buffer?
	beq nobuffer
	jsr LoadBuffer		; initialize for the buffer
	ldy #$80		; direction is now out->into interface box
	bmi pushbuffer
	;; here: concurrent mode is active
bufferclean:
	lda IRQStatShadow
	and #$08		; check whether VSeroc is still active
	bne bufferclean		; if so, loop until the serial port disables it
	sei
	;; copy the old vectors into the vector
	ldx #6-1
lp:	
	lda OldVectors,x
	sta SerInVec,x		; copy data over
	dex
	bpl lp
	cli
nobuffer:
	;; Now write out an "empty buffer" to indicate that we're done.
	ldy #$00
	sty SIOAux1
pushbuffer:
	sty SIOStatus
	ldx #$ff
	stx ConFlag		; turn off the concurrent mode
	stx OpenMode		; indicate that the channel is free
	lda #'W'		; write out the buffer
	jsr SerialInterf	; serial interface to SIO
exit:
	rts
.endproc
;;; *** RGet
;;; *** Read a byte (in concurrent mode)
.proc	RGet
	bit ConFlag		; is the concurrent mode enabled?
	bvc congetwt
	ldy #NotConcurrent	; deliver the error for the handler
	rts
abort:
	jmp BreakConcurrent	; stop the concurrent mode
congetwt:	
	lda BreakFlag		; are we interrupted?
	beq abort
	lda IRQStatShadow	; check whether interrupts are disabled
	and #$20		; serin allowed?
	bne wtloop
	jsr InitForRead		; restart it. Must happen if we got interrupted
wtloop:	
	cli			; give interrupts a chance
	lda BreakFlag		; are we interrupted?
	beq abort
	sei
	jsr CheckBuffer		; check whether input/output pointer are identically
	beq wtloop

	ldy #0
	lda (ReadOut),y		; read the data
	sta ZIOByte		; submit the data

	lda ReadLen
	bne nocarry
	dec ReadLen+1
nocarry:
	dec ReadLen		; count down the number of bytes in here
	ldx #ReadOut-ReadIn
	jsr AdvanceInput	; advance the read output buffer
	cli			; interrupts may continue now
	;; Now transpose the characters if required
	lda ChrTrans
	and #$0c		; parity check?
	beq keep		; if not, keep the character raw, including the high one-bits (urgl!)
	cmp #$0c		; clear parity and all upper bits ?
	beq clear
	tay			; keep flags
	;; now clear all upper bits beyond the parity bit
	lda WidthMask
	and ParityMask		; remove the parity bit from the width mask, contains now all bits to be cleared
	eor #$ff	
	and ZIOByte		; mask out all irrelevant bits
	jsr ComputeParity	; compute theparity
	tya
	and #$08		; set or clear?
	beq odd
	bcc clear		; if parity is even on even check, then fine
parityerror:
	lda ErrorFlag
	ora #$20		; set the parity error
	sta ErrorFlag
	bne clear
odd:				; here odd parity
	bcc parityerror		; if odd on odd parity check, then fine
clear:	
	lda WidthMask		; mask out irrelevant bits
	eor #$ff
	and ZIOByte
	sta ZIOByte		; erase the parity bit now
keep:
	lda ChrTrans		; check now character translations
	and #$30
	cmp #$20		; raw translations?
	tay
	lda ZIOByte		; get character
	bcs exit		; if raw, keep it
	cmp #$0d		; is it CR?
	bne nocr
	lda #$9b		; translate CR->EOL
	bne exit
nocr:
	cpy #$00		; "partial" translation?
	beq exit		; keep it as such otherwise
	and #$7f		; clear bit 8 for "full translation"
	cmp #$20		; special character
	bcc illegal
	cmp #$7d
	bcc exit
illegal:
	lda InvReplace		; replace character by invalid one (shouldn't we loop here?)
exit:
	ldy #$01
	rts
.endproc	
;;; *** RPut
;;; *** Write a character out over the serial R: handler
.proc	RPut
	sta ZIOByte		; keep the data
	lda OpenMode		; do we allow writing
	and #$08		; if not, error
	beq errorout
valid:
	lda ChrTrans		; check for character translations
	and #$30
	tay
	beq partial		; partial translation?
	lda ZIOByte
	cpy #$20		; no translation?
	bcs PutByte		; if so, put now directly
partial:
	lda ZIOByte		; get the character again
	cmp #$9b		; do we have an EOL here?
	bne noeol		; simple case
	lda ChrTrans
	and #$40		; shall we insert an CR here before the LF?
	beq nolf
	lda #$0d		; insert a CR here
	jsr PutByte		; directly
	bmi exit
	lda #$0a		; write an LF here
	bne PutByte
nolf:				; only a CR (yuck, only a LF would be better)
	lda #$0d
	bne PutByte
	;; here:	translate a non-EOL
noeol:
	cpy #$10		; full translation
	beq full
	and #$7f		; here partial translation:	Mask out bit 7 (FIXME:	nonsense)
	bpl PutByte		; write data out
full:
	cmp #$20		; control character group?
	bcc exitfine		; if so, ignore
	cmp #$7d		; or above ?
	bcc PutByte
exitfine:
	ldy #$01
	Skip2
errorout:			; error: output only
	ldy #InputOnly		; input only
exit:
	rts
.endproc
	
;;; *** PutByte
;;; *** Write a byte (raw, untranslated) to the serial port
.proc	PutByte
	sta ZIOByte		
	lda ChrTrans		; check for parity out now
	and #$03
	beq keep		; no character filtering, raw, ignore data width
	tay			; keep type in Y
	lda ZIOByte
	ora WidthMask		; set all upper bits, these are not part of the data, 
	sta ZIOByte		; but rather "extended stop bits"
	cpy #$03
	beq keep		; simple filtering, keep parity set?
	lda WidthMask
	eor #$ff	
	and ZIOByte		; clear all bits that do not participate to the parity	
	jsr ComputeParity	; recompute it
	tya			; flags->A
	and #$01
	bne odd
	bcs keep		; if total number of one bits is odd, keep parity bit set
clearpar:			; here:	 clear parity
	lda ZIOByte
	and ParityMask
	sta ZIOByte		; clear the parity bit
	clc
odd:				; odd parity. If the total number of one bits is odd, clear the parity. Else keep it set
	bcs clearpar		; clear if number of one bits is odd already
keep:
	bit ConFlag		; concurrent mode on?
	bvc ConcurrentPut
.endproc
;;; *** BlockPut
;;; *** Put out a byte in block mode
.proc	BlockPut
	;; here: block output
	ldy WriteIn
	lda ZIOByte		; insert the character
	sta OutputBuffer,y		; into the buffer
	inc WriteIn
	cmp #$0d		; end of the/a line?
	beq PushBuffer
	lda #$20		; is the buffer full (bizarre, the buffer can take 64 character)
	cmp WriteIn
	beq PushBuffer
	ldy #$01		; all fine here
	rts
.endproc
;;; *** PushBuffer
;;; *** Write out the buffer, then clean it
.proc	PushBuffer
	jsr LoadBuffer
	ldy #$80		; data direction: out
	lda #'W'		; write out data
	jsr SerialInterf
	lda #$00
	sta WriteIn		; clean buffer size
	rts			; that finishes the write
.endproc
;;; *** BreakConcurrent
;;; *** This gets called if concurrent
;;; *** mode shall be stopped.
.proc	BreakConcurrent
	lda IRQStatShadow
	and #$c7
	sta IRQStatShadow
	sta IRQStat		; disable all serial interrupts	
	lda #$00		; clear the write pointers
	sta WriteIn
	sta WriteOut		; read pointers get reset as soon as we enter RGet again
	ldy #BreakError
	sty BreakFlag		; clear the break flag, deliver the error
	rts
.endproc
;;; *** ConcurrentPut
;;; *** Put byte in concurrent
.proc	ConcurrentPut
wtloop:
	lda BreakFlag		; abort the transfer?
	beq BreakConcurrent
	lda WriteLen
	cmp #$1f		; wait until there's more room in the buffer
	bcs wtloop
	sei			; since this is interrupt driven, we need to wait a little
	ldy WriteIn
	lda ZIOByte
	sta OutputBuffer,y	; keep in the buffer
	iny
	tya
	and #$1f		; increment the fill-in position
	sta WriteIn
	inc WriteLen
	cli
	lda IRQStatShadow
	ldy StopBits		; check whether we have one or two stop bits
	bmi launch
	ora #$10		; allow serial output on one stop bit. This waits shorter because
	;; the serial output ready happens earlier.
launch:
	ora #$08		; allow XMT done (this then triggers the output)
	sta IRQStatShadow
	sta IRQStat
	ldy #$01		; return OK
	rts
.endproc
;;; *** RStatus
;;; *** Get the status of the R: handler
.proc	RStatus	
	jsr CheckUnit		; we only support unit one
	lda ErrorFlag
	sta DiskStatus		; keep the error flags here.
	ldy #$00
	sty ErrorFlag		; erase again
	bit ConFlag		; concurrent mode active?
	bvc ConcurrentStatus
	;; runs into the following
.endproc
;;; *** BlockStatus
;;; *** Return the serial status in the block mode
.proc	BlockStatus
	;; here: status in block-IO mode
	pha			; keep error flag
	lda #<DiskStatus
	sta SIOBufferLo
	lda #>DiskStatus
	sta SIOBufferHi		; read into this buffer (overwriting the flag)
	lda #2			; two bytes
	sta SIOSizeLo
	lda #0
	sta SIOSizeHi
	ldy #$40		; from interface->computer
	lda #'S'		; read status
	jsr SerialInterf
	pla
	ora DiskStatus		; add the errors of the interface box itself
	sta DiskStatus
	cpy #$00		; set flags for CIO
	rts
.endproc
;;; *** ConcurrentStatus
;;; *** Return the status in the concurrent mode
.proc	ConcurrentStatus
	sei
	lda ReadLen
	sta DiskStatus+1	; keep here characters in the input buffer
	lda ReadLen+1
	sta DiskStatus+2
	lda WriteLen		; keep here the characters in the output buffer
	sta DiskStatus+3
	cli
	iny
	rts
.endproc
;;; *** RestoreAux1
;;; *** Restore Aux1 after a Special. This
;;; *** is required because CIO Read/Write checks
;;; *** it.
.proc	RestoreAux1
	lda OpenMode
	sta ZAux1
	rts
.endproc
;;; *** RSpecial
;;; *** The handler call-in for extended commands 
;;; ***	of the R: device
.proc	RSpecial	
	jsr CheckUnit		; we only support unit one
	lda ZCmd		; get the command
	cmp #32			; below this
	bcc invalid
	cmp #41			; or above this
	bcs invalid		; are invalid
	and #$0f		; get only the lower bits
	tay
	lsr a
	bcs invalid		; and must be even
	lda XioCmds+1,y
	pha
	lda XioCmds,y
	pha
	rts
invalid:
	ldy #InvalidCmd
	rts
.endproc
;;; *** InitForRead
;;; *** Init the read buffer pointer
;;; *** and start reading by the interrupt
.proc	InitForRead
	lda ReadStart
	sta ReadIn
	sta ReadOut
	lda ReadStart+1
	sta ReadIn+1
	sta ReadOut+1		; initialize the buffer
	lda #$00
	sta ReadLen
	sta ReadLen+1
	lda IRQStatShadow
	ora #$20		; enable the VSERIN interrupt
	sta IRQStatShadow
	sta IRQStat
	rts
.endproc
;;; *** EnterConcurrent
;;; *** Enter the concurrent mode
.proc	EnterConcurrent
	lda OpenMode		; channel must be open for this to work
	bmi notopen
	bit ConFlag		; and we must not yet be in concurrent mode
	bvc isentered
	lsr a			; append mode open?
	bcc disallowed
	lsr a
	lsr a			; are we open for reading?
	bcc noreading
	;; here: allow concurrent read, provide a buffer
	lda ZAux1		; do we get a buffer?
	beq providebuffer	; provide a default buffer
	lda ZLen
	ora ZLen+1		; consistency check again
	beq providebuffer
	;; set the buffer
	clc
	lda ZAdr
	sta ReadStart
	adc ZLen
	sta ReadEnd
	lda ZAdr+1
	sta ReadStart+1
	adc ZLen+1
	sta ReadEnd+1
	bcc gotbuffer
invalidbuffer:
	ldy #InvalidBuffer
	Skip2
disallowed:
	ldy #NotOpenForCM
	Skip2
isentered:
	ldy #InConcurrent
	Skip2
notopen:
	ldy #ChannelNotOpen
exit:	
	rts
providebuffer:			; provide a default buffer for concurrent mode here
	ldx #3
cplp:
	lda InputBufferSettings,x
	sta ReadStart,x
	dex
	bpl cplp
gotbuffer:

noreading:			; continue with init of output buffer
	lda #$00		; clear the write pointers
	sta WriteIn
	sta WriteOut
	;; now request pokey parameters for the data
	sta SIOSizeHi
	sta SIOAux2
	lda #9			; nine bytes
	sta SIOSizeLo
	lda PokeyBufPtr
	sta SIOBufferLo
	lda PokeyBufPtr+1
	sta SIOBufferHi
	lda OpenMode		; well, this is actually ignored, but so what
	sta SIOAux1
	ldy #$40		; direction is reading
	lda #'X'
	jsr SerialInterf
	bmi exit
	;; now initialize pokey for the interrupt thingy
	sei
	lda #$73
	sta SkStat		; serial port operation mode
	ldx #$08
lp:
	lda PokeyBuffer,x
	sta PokeyBase,x
	dex
	bpl lp
	ldx #6-1
lp2:
	lda SerInVec,x
	sta OldVectors,x
	lda NewVectors,x
	sta SerInVec,x
	dex
	bpl lp2
	lda OpenMode
	and #$04		; open for reading?
	beq noreading2	
	jsr InitForRead
noreading2:
	ldx #$00
	stx ConFlag		; enter concurrent mode
	cli
	jsr RestoreAux1
	ldy #$01
	rts
.endproc
;;; *** CleanBuffer
;;; *** write the serial buffer out, i.e.
;;; *** clean partially filled output buffer
.proc	CleanBuffer
	lda OpenMode
	bmi notopen
	and #$08		; must be open for write
	beq notforwriting
	lda WriteIn
	beq exit

	bit ConFlag		; concurrent mode active?
	bvs blockclean
bufferclean:
	lda IRQStatShadow
	and #$08		; check whether VSeroc is still active
	bne bufferclean		; if so, loop until the serial port disables it
	beq exit
blockclean:
	jsr PushBuffer		; write it out
	Skip2
exit:
	ldy #$01
	Skip2
notforwriting:
	ldy #InputOnly
	Skip2
notopen:
	ldy #ChannelNotOpen
	jsr RestoreAux1
	rts
.endproc
;;; *** SetBaud
;;; *** Set the baud rate and handshake options
.proc	SetBaud
	lda ZAux1
	tay
	and #$80
	sta StopBits		; keep # of stop bits here
	tya
	lsr a
	lsr a
	lsr a
	lsr a			; extract # of bits
	and #$03
	tax
	lda WidthMaskTable,x	; get the data to be or'd to truncate to the given # of bits
	sta WidthMask
	lda ParityMaskTable,x
	sta ParityMask
	lda #'B'
	jsr ShortSerial		; inform handler about baud rate change
	jsr RestoreAux1
	rts
.endproc
;;; *** SetDTR
;;; *** Set the DTR/RTS/XMT lines
.proc	SetDTR
	lda #'A'
	jsr ShortSerial
	jsr RestoreAux1
	rts
.endproc
;;; *** SetParity
;;; *** Set parity control, character transposition
.proc	SetParity
	lda ZAux1
	sta ChrTrans		; keep character transposition here
	lda ZAux2
	sta InvReplace
	jsr RestoreAux1
	ldy #$01
	rts
.endproc
;;; *** InitSerial
;;; *** initialize the R: handler
.proc	InitSerial	
	ldy #$ff
	sty ConFlag
	sty OpenMode		; initialize the handler
	jsr BlockStatus		; get the status from the R: handler (force-aborts concurrent mode)
	bmi error		; do not install if this is the error

	ldx #$00
fndloop:
	lda HaTabs,x		; found empty slot in HaTabs?
	beq foundslot
	cmp #'R'		; if the handler is already mounted, abort
	beq error
	inx
	inx
	inx
	cpx #32			; until all positions scanned
	bcc fndloop
error:
	sec
	rts			; carry is now set to indicate an error
foundslot:	
	lda #'R'
	sta HaTabs,x		; handler name
	lda DevInitPtr
	sta HaTabs+1,x
	lda DevInitPtr+1
	sta HaTabs+2,x
	clc
	rts
.endproc
;;; *** The serial handler Reset routine
.proc	ResetSerial
	jsr InitSerial		; initialize us
	jsr ReserveMemory	; to inform the called vector
	jsr RunOldDosInit
	;; runs into the following (again)
.endproc
;;; *** ReserveMemory
;;; *** reserve memory after MemLo for this handler
.proc	ReserveMemory
	lda MemLo
	cmp MemEnd
	lda MemLo+1
	sbc MemEnd+1
	bcs nomemlo
	lda #<(LastHandlerByte-FirstHandlerByte)
	adc MemLo
	sta MemLo
	lda #>(LastHandlerByte-FirstHandlerByte)
	adc MemLo+1
	sta MemLo+1
nomemlo:
	clc
	rts
.endproc	
;;; *** RunOldDosInit
;;; *** Run the previous DosIni
;;; *** vector if we have one.
.proc	RunOldDosInit
	lda OldInit		; check whether we must run DOSINI
	ora OldInit+1
	beq RunOldDosInit-1	; to the RTS
	jmp (OldInit)
.endproc
;;; *** Various tables
HandlerTable:			; addresses of the various handler points
	.word ROpen-1
	.word RClose-1
	.word RGet-1
	.word RPut-1
	.word RStatus-1
	.word RSpecial-1
DevInitPtr:	.word HandlerTable ; pointer to the handler entry points
XioCmds:			; entry points for special commands
	.word CleanBuffer-1
	.word SetDTR-1
	.word SetBaud-1
	.word SetParity-1
	.word EnterConcurrent-1
InputBufferSettings:		; initializer for the input buffer
	.word InputBuffer,InputBuffer+32
OutputBufferPtr:
	.word OutputBuffer
PokeyBufPtr:
	.word PokeyBuffer
NewVectors:			; new pokey interrupt vectors
	.word VSerIn		; serial input vector
	.word VSerOr		; serial output ready
	.word VSerOc		; serial output complete
ResetVector:
	.word ResetSerial
MemEnd:
	.word LastHandlerByte	; last byte that is used here
SerialTimeout:
	.byte 8			; 8 seconds timeout
	;; mask that must get or'd to the data to truncate them to 8,7,6,5 bits
	;; this also sets the parity to one
WidthMaskTable:		.byte $00,$80,$c0,$e0
	;; mask that must get and'ed to set the parity to zero
ParityMaskTable:	.byte $ff,$7f,$bf,$df
OldVectors:			; place holders for old pokey vectors
		.word 0,0,0
OldInit:	.word 0		; old DosIni vector
OpenMode:	.byte 0		; keeps the mode the channel was opened with
ConFlag:	.byte 0		; gets cleared on concurrent mode
ErrorFlag:	.byte 0		; keeps the last serial errors as bit mask
WriteIn:	.byte 0		; fill-in position into the output buffer
WriteOut:	.byte 0		; read-out position of the output buffer
WriteLen:	.byte 0		; size of the output buffer in bytes
ChrTrans:	.byte 0		; keeps the character translation/parity flags
InvReplace:	.byte 0		; character that gets inserted on reading as invalid replacement
StopBits:	.byte 0		; non-zero for two stop bits
WidthMask:	.byte 0		; mask to be or'd to truncate to the given # of bits
ParityMask:	.byte 0		; mask to be and'ed to clear the parity bits
ReadStart:	.word 0		; read buffer start address
ReadEnd:	.word 0		; read buffer end address (exclusive)
PokeyBuffer:	.res 9		; contains pokey register contents
OutputBuffer:	.res 32		; 32 bytes buffer
InputBuffer:	.res 32		; 32 bytes buffer
LastHandlerByte:		; no code/data beyond that point. MemLo gets seated here
