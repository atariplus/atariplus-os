;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: help.asm,v 1.2 2013/06/02 20:41:04 thor Exp $		**
;;; **									**
;;; ** In this module:	 List the DUP commands				**
;;; ** cart								**
;;; **********************************************************************

	.include "kernel.i"
	.include "cio.i"
	.segment "help"

Start:
	jsr SetBuffer
	lda #CmdGetRecord
	sta IOCBCmd,x
	jsr CIOVector
	bmi exit
	txa
	pha
	ldx #0
	jsr SetBuffer
	lda #CmdPutRecord
	sta IOCBCmd,x
	jsr CIOVector
	pla
	tax
	tya
	bpl Start
exit:
	rts
SetBuffer:
	lda #<$600
	sta IOCBAdr,x
	lda #>$600
	sta IOCBAdr+1,x
	lda #<$100
	sta IOCBLen,x
	lda #>$100
	sta IOCBLen+1,x
	rts
