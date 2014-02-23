;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: setheadline.asm,v 1.6 2014/02/23 12:27:01 thor Exp $	**
;;; **									**
;;; ** In this module:	 An external command to set the headline	**
;;; ** cart								**
;;; **********************************************************************

	.include "cio.i"
	.include "sio.i"
	.include "kernel.i"
	.include "dup.i"
	.include "fms.i"
	.segment "sethdl"

	DirBuffer	=	$680
	ZPtr		=	$D4 ;fr0
	
;;; *** Program starts here
.proc	Start
	jsr GetArgument
	bcs exit
	lda #'R'
	jsr AccessDir
	bcs exit
	jsr IfAvailable
	bcs exit
	lda WriteCommand
	jsr AccessDir
exit:
	rts
.endproc
;;; *** AccessDir
;;; *** Access the first sector of the directory where
;;; *** the headline is stored. A=SIO Command
.proc	AccessDir
	sta SIOCommand
	lda #<DirBuffer
	sta SIOBufferLo
	lda #>DirBuffer
	sta SIOBufferHi
	lda #<$169		;the first directory sector
	sta SIOAux1
	lda #>$169
	sta SIOAux2
	jsr DiskInterfVector
	bmi Error
	clc
	rts
.endproc
;;; *** Print the error and abort the function
;;; *** Error
.proc	Error
	ldx #<ErrorMsg
	ldy #>ErrorMsg
	;; runs into the following
.endproc
;;; *** Print the EOL terminated string at X,Y and return with C=1
;;; *** Print
.proc	Print
	lda #CmdPutRecord
	sta IOCBLen
	sta IOCBLen+1		;long enough
	;; runs into the following
.endproc
;;; *** Run the IOCB Cmd A on the address X,Y
.proc	RunCIOZero
	stx IOCBAdr
	sty IOCBAdr+1
	ldx #0
	sta IOCBCmd
	jsr CIOVector
	sec
	rts
.endproc
;;; *** Set the CIO Length to X,Y
.proc	SetLength
	stx IOCBLen
	sty IOCBLen+1
	rts
.endproc
;;; *** Check whether the headline position in the directory
;;; *** is available. Return C=0 if so, otherwise error.
.proc	IfAvailable
	lda DirBuffer
	beq isfree		;end of directory
	bmi isfree		;deleted
	cmp #$63		;is headline
	beq isfree
	ldx #<HeadlineUsed
	ldy #>HeadlineUsed
	bne Print		;abort with an error
isfree:
	ldy #0
	lda (ZPtr),y
	cmp #$9b
	beq blankit
	;; here insert the headline from the buffer
	lda #$63
	ldx #0
	sta DirBuffer,x
loop:
	lda (ZPtr),y
	cmp #$9b
	bne insert
	lda #' '
	dey
insert:
	ora #$80
	inx
	sta DirBuffer,x
	iny
	cpx #$0f
	bcc loop
	clc
	rts
blankit:
	lda #$80
	sta DirBuffer
	clc
	rts
.endproc
;;; *** GetArgument
;;; *** Read the command line argument
.proc	GetArgument
	ldx #0
loop:	
	lda DupBuffer,x
	cmp #$9b
	beq noarg
	inx
	cmp #' '
	bne loop
skip:
	lda DupBuffer,x
	cmp #' '
	bne abort
	inx
	bne skip
noarg:	
	ldx #<RequestL
	ldy #>RequestL
	jsr SetLength
	ldx #<Request
	ldy #>Request
	lda #CmdPutBlock
	jsr RunCIOZero
	ldx #<17
	ldy #>17
	jsr SetLength
	ldx #<DupTargetBuffer
	ldy #>DupTargetBuffer
	lda #CmdGetRecord
	jsr RunCIOZero
	bmi errorabort

	ldx #DupTargetBuffer-DupBuffer
	lda DupBuffer,x
abort:
	cmp #'D'		; is there a D: or a Dn:?
	bne nodisk
	ldy #1
	lda #':'
	cmp DupBuffer+1,x	;D:?
	beq setdrive
	cmp DupBuffer+2,x	;or Dx:?
	bne nodisk
	lda DupBuffer+1,x	;the drive number
	cmp #'1'
	bcc nodisk
	cmp #'9'
	bcs nodisk
	and #$0f
	tay
	inx
setdrive:
	inx
	inx
	sty SIODeviceUnit
nodisk:	
	txa
	clc
	adc #<DupBuffer
	sta ZPtr
	lda #0
	adc #>DupBuffer
	sta ZPtr+1
	rts
errorabort:
	sec
	rts
.endproc
ErrorMsg:	.byte "I/O Error - aborted",155
HeadlineUsed:	.byte "Headline position is used !",155
Request:	.byte "Enter (device:)headline or press",155
		.byte 'R'+$80,'E'+$80,'T'+$80,'U'+$80,'R'+$80,'N'+$80
		.byte " to clear :"
RequestL	=	*-Request
