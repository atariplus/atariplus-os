;;; **********************************************************************
;;; ** Thor Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: enablebasic.asm,v 1.3 2013/06/02 20:41:04 thor Exp $	**
;;; **									**
;;; ** In this module:	 A simple switch for enabling a disabled BASIC	**
;;; ** cart								**
;;; **********************************************************************

	.include "reset.i"
	.include "kernel.i"
	.segment "enablebasic"

Start:
	lda BasicDisabled
	beq ison		;nothing to do
	lda #0
	sta BasicDisabled
	lda FmsBootFlag
	ora #1			;clear the basic RAM
	sta FmsBootFlag
ison:
	jmp WarmStartVector

	
