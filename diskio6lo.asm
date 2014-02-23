;;; **********************************************************************
;;; ** THOR Os                                                          **
;;; ** A free operating system for the Atari 8 Bit series               **
;;; ** (c) 2003 THOR Software, Thomas Richter                           **
;;; ** $Id: diskio6lo.asm,v 1.3 2013/06/02 20:41:04 thor Exp $          **
;;; **                                                                  **
;;; ** In this module:   DiskIO helper 					**
;;; **********************************************************************

	.include "reset.i"
	.include "kernel.i"
	.include "diskio6lo.i"
	
	.segment "diskiolo"

;;; This is the resident low-memory part of diskIO
	
;;; *** NewTab: The new editor HaTabs
;;;
LoStart		=	*
	.global NewTab
NewTab:
	.word 0,0,0,0,0,0

;;; *** FMSSwitchOn
;;; *** Enable the cartridge, eventually through the FMS.
;;; *** or not, depending on whether there is a switcher
.proc	FMSSwitchOn
	.global FMSSwitchOnOffset
FMSSwitchOnOffset	=	*-LoStart
	jmp $ffff
.endproc
;;; *** FMSSwitchOff
;;; *** Disable the cartridge, eventually through the FMS
.proc	FMSSwitchOff
	.global FMSSwitchOffOffset
FMSSwitchOffOffset	=	*-LoStart
	jmp $ffff
.endproc
;;; *** NewGet
;;; *** Read a character from the editor, return
.proc NewGet
	.global NewGetEntryOffset
NewGetEntryOffset	=	*-LoStart
	jsr $ffff		; will be patched over
	jsr FMSSwitchOff
	.global NewGetOffset
NewGetOffset	=	*-LoStart
	jmp $ffff		; handle it...
.endproc
;;; *** CIOEntry
;;; *** Called by the resident part to jump into CIO
.proc	CIOEntry
	.global	CIOEntryOffset
CIOEntryOffset		=	*-LoStart
	jsr FMSSwitchOn
	jsr CIOVector
	jmp FMSSwitchOff
.endproc
;;; *** NewInit
;;; *** Calls the old init vector
.proc	NewInit
	.global NewInitEntryOffset
NewInitEntryOffset	=	*-LoStart
	jsr FMSSwitchOn
	.global ResetOldOffset
ResetOldOffset		=	*-LoStart
	jsr $ffff		; will be patched over
	jmp FMSSwitchOff
.endproc
;;; *** NewReset
;;; *** The reset vector, re-installs DiskIO and makes it resident
.proc Reset
	.global ResetNewOffset
ResetNewOffset		=	*-LoStart
	jsr FMSSwitchOff
	.global ResetOffset
ResetOffset	=		*-LoStart
	jsr $ffff
	jmp FMSSwitchOn
.endproc
DirBufferOffset	=		*-LoStart
DirBuffer:		.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
			.byte 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

LoLen		=	*-LoStart
	
