;;; **********************************************************************
;;; ** THOR Os								**
;;; ** A free operating system for the Atari 8 Bit series		**
;;; ** (c) 2003 THOR Software, Thomas Richter				**
;;; ** $Id: misc.i,v 1.7 2008-12-29 23:37:23 thor Exp $		**
;;; **									**
;;; ** In this module:	 Miscellaneous helper functions in the kernel	**
;;; **********************************************************************
	
;; Memory used by miscellaneous routines

IRQTemp			=	$22d		; temporary buffer for SetIRQ
Trig3Shadow		=	$3fa		; shadow register for the cart Ctrl Trig3
BootSpace		=	$500		; 850 boot requires this
			; since this shadow register is the #1 reason for OsXL crashes,
			; we no longer support or encourage its usage, but leave it
			; filled in for backwards compatiblity

	;** global vectors defined within here
	.global SetIRQ			; initialize timer/IRQ vector
	.global InitNMI			; initialize antic NMI
	.global ReadTapeBlock		; read a block from tape (unsupported)
	.global OpenTapeChannel		; open tape for reading  (unsupported)
	.global MountHandler		; mount new device in HaTabs
	.global UnlinkParHandler	; unlink an extended device handler (unsupported)
	.global LinkParHandler		; link an extended device (unsupported)
	.global Bye
	.global PowerupDisplay
	.global SelfTest		; all unused
	.global	CIODirect		; call CIO disabling the selftest (service)
	.global	Boot850			; boot the 850 interface
	.global RunRunVector		; run thru $2e0 disabling the DUP ROM
	.global	MapSelfTest		; Make selftest area visible
	.global HideSelfTest		; Make selftest area invisible
