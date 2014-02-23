;;; **********************************************************************
;;; ** THOR Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: menuloader.asm,v 1.6 2013/06/02 20:41:06 thor Exp $          **
;;; **                                                                  **
;;; ** In this module:   DUP Menu - resident loader			**
;;; **********************************************************************

	.include "reset.i"
	.include "cio.i"
	.include "kernel.i"
	.include "editor.i"
	.include "errors.i"
	.include "menuloader.i"
	.include "menu.i"
	
	.segment "menuloader"
	
;;; *** Relocatable loader for the DUP menu
.proc	Start
	txa
	pha			;keep the IOCB
	
	ldx #0
	stx CursorInhibit
	ldy #<Txt
	lda #>Txt
	jsr SetBuffer
	ldy #<TxtLen
	lda #>TxtLen
	jsr SetLength
	lda #CmdPutBlock
	jsr ExecCmd

	ldy MemLo+1
	ldx MemLo
	beq notnextpage
	iny
	ldx #0
notnextpage:
	stx TargetPtr
	tya
	clc
	adc #3
	and #$fc
	sta TargetPtr+1
	tay
	clc
	lda #<StartOffset
	sta RunAddress
	sta TargetPtr
	tya
	adc #>StartOffset
	sta RunAddress+1

	pla
	tax
	
	ldy #<MenuLength
	lda #>MenuLength
	jsr SetLength
	ldy RunAddress
	lda RunAddress+1
	jsr SetBuffer
	lda #CmdGetBlock
	jsr ExecCmd
	bmi error

	lda TargetPtr+1
	sec
	sbc #7			;the address the menu is compiled to
	sta RelocOffset

	lda RunAddress+1
	sta TargetPtr+1
	
	lda #0
	tay
	jsr SetLength
relocate:
	jsr CIOVector
	bmi erroreof
	clc
	adc TargetPtr
	sta TargetPtr
	bcc noinc
	inc TargetPtr+1
noinc:
	ldy #0			;only high-byte
	lda (TargetPtr),y
	clc
	adc RelocOffset
	sta (TargetPtr),y
	bcc relocate
erroreof:
	cpy #EndOfFile
	bne error

	lda #CmdClose
	jsr ExecCmd

	jmp (RunAddress)
error:
	ldx #0
	ldy #<Error
	lda #>Error
	jsr SetBuffer
	ldy #<ErrorL
	lda #>ErrorL
	jsr SetLength
	lda #CmdPutBlock
	jsr ExecCmd
exit:	
	rts
.endproc
;;; *** SetLength
;;; *** In the IOCB x set the length to (A,Y)
.proc	SetLength
	sta IOCBLen+1,x
	tya
	sta IOCBLen,x
	rts
.endproc
;;; *** SetBuffer
;;; *** Install the buffer of IOCB #x
.proc	SetBuffer
	sta IOCBAdr+1,x
	tya
	sta IOCBAdr,x
	rts
.endproc
;;; *** ExecCmd
;;; *** Run the CIO command in A over IOCB #x
.proc	ExecCmd
	sta IOCBCmd,x
	jmp CIOVector
.endproc
Txt:
		.byte $7d
		.byte 29,29,29,29,29,29,29,29,29,29
		.byte "Please wait for loading DUP menu ....."
TxtLen	=	*-Txt
	
Error:		.byte 125,"Error loading the menu.",155
ErrorL	=	*-Error

