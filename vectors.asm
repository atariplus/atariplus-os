;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: vectors.asm,v 1.2 2013/06/02 20:41:07 thor Exp $		**
;;; **									**
;;; ** In this module:	 CPU vectors and checksum			**
;;; **********************************************************************

	.include "reset.i"
	.include "nmi.i"
	.include "irq.i"
	
	.segment  "CPUVectors"

	.word 0			; the checksum goes here
	.word NMIEntry
	.word CPUReset
	.word IRQEntry
 
